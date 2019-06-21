{-# LANGUAGE MultiWayIf #-}
module Draw.Main (drawMain) where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center ( hCenter )
import           Brick.Widgets.List ( listElements )
import           Brick.Widgets.Edit ( editContentsL, renderEditor, getEditContents )
import           Control.Arrow ( (>>>) )
import           Data.Char ( isSpace, isPunctuation )
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Zipper ( cursorPosition, insertChar, getText, gotoEOL )
import           Data.Time.Calendar ( fromGregorian )
import           Data.Time.Clock ( UTCTime(..) )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.~), (^?!), to, view, folding )

import           Network.Mattermost.Types ( ChannelId, Type(Direct, Private, Group)
                                          , ServerTime(..), UserId
                                          )


import           Constants
import           Draw.ChannelList ( renderChannelList )
import           Draw.Messages
import           Draw.Autocomplete
import           Draw.URLList
import           Draw.Util
import           Events.Keybindings
import           Events.MessageSelect
import           Markdown
import           State.MessageSelect
import           Themes
import           TimeUtils ( justAfter, justBefore )
import           Types
import           Types.KeyEvents


previewFromInput :: Maybe MessageType -> UserId -> Text -> Maybe Message
previewFromInput _ _ s | s == T.singleton cursorSentinel = Nothing
previewFromInput overrideTy uId s =
    -- If it starts with a slash but not /me, this has no preview
    -- representation
    let isCommand = "/" `T.isPrefixOf` s
        isEmoteCmd = "/me " `T.isPrefixOf` s
        content = if isEmoteCmd
                  then T.stripStart $ T.drop 3 s
                  else s
        msgTy = fromMaybe (if isEmoteCmd then CP Emote else CP NormalPost) overrideTy
    in if isCommand && not isEmoteCmd
       then Nothing
       else Just $ Message { _mText          = getBlocks content
                           , _mMarkdownSource = content
                           , _mUser          = UserI False uId
                           , _mDate          = ServerTime $ UTCTime (fromGregorian 1970 1 1) 0
                           -- The date is not used for preview
                           -- rendering, but we need to provide one.
                           -- Ideally we'd just use today's date, but
                           -- the rendering function is pure so we
                           -- can't.
                           , _mType          = msgTy
                           , _mPending       = False
                           , _mDeleted       = False
                           , _mAttachments   = mempty
                           , _mInReplyToMsg  = NotAReply
                           , _mMessageId     = Nothing
                           , _mReactions     = mempty
                           , _mOriginalPost  = Nothing
                           , _mFlagged       = False
                           , _mChannelId     = Nothing
                           }

-- | Tokens in spell check highlighting.
data Token =
    Ignore Text
    -- ^ This bit of text is to be ignored for the purposes of
    -- spell-checking.
    | Check Text
    -- ^ This bit of text should be checked against the spell checker's
    -- misspelling list.
    deriving (Show)

drawEditorContents :: ChatState -> HighlightSet -> [Text] -> Widget Name
drawEditorContents st hs =
    let noHighlight = txt . T.unlines
    in case st^.csEditState.cedSpellChecker of
        Nothing -> noHighlight
        Just _ ->
            case S.null (st^.csEditState.cedMisspellings) of
                True -> noHighlight
                False -> doHighlightMisspellings
                           hs
                           (st^.csEditState.cedMisspellings)

-- | This function takes a set of misspellings from the spell
-- checker, the editor lines, and builds a rendering of the text with
-- misspellings highlighted.
--
-- This function processes each line of text from the editor as follows:
--
-- * Tokenize the line based on our rules for what constitutes
--   whitespace. We do this because we need to check "words" in the
--   user's input against the list of misspellings returned by the spell
--   checker. But to do this we need to ignore the same things that
--   Aspell ignores, and it ignores whitespace and lots of puncutation.
--   We also do this because once we have identified the misspellings
--   present in the input, we need to reconstruct the user's input and
--   that means preserving whitespace so that the input looks as it was
--   originally typed.
--
-- * Once we have a list of tokens -- the whitespace tokens to be
--   preserved but ignored and the tokens to be checked -- we check
--   each non-whitespace token for presence in the list of misspellings
--   reported by the checker.
--
-- * Having indicated which tokens correspond to misspelled words, we
--   then need to coallesce adjacent tokens that are of the same
--   "misspelling status", i.e., two neighboring tokens (of whitespace
--   or check type) need to be coallesced if they both correspond to
--   text that is a misspelling or if they both are NOT a misspelling.
--   We do this so that the final Brick widget is optimal in that it
--   uses a minimal number of box cells to display substrings that have
--   the same attribute.
--
-- * Finally we build a widget out of these coallesced tokens and apply
--   the misspellingAttr attribute to the misspelled tokens.
--
-- Note that since we have to come to our own conclusion about which
-- words are worth checking in the checker's output, sometimes our
-- algorithm will differ from aspell in what is considered "part of a
-- word" and what isn't. In particular, Aspell is smart about sometimes
-- noticing that "'" is an apostrophe and at other times that it is
-- a single quote as part of a quoted string. As a result there will
-- be cases where Markdown formatting characters interact poorly
-- with Aspell's checking to result in misspellings that are *not*
-- highlighted.
--
-- One way to deal with this would be to *not* parse the user's input
-- as done here, complete with all its Markdown metacharacters, but to
-- instead 1) parse the input as Markdown, 2) traverse the Markdown AST
-- and extract the words from the relevant subtrees, and 3) spell-check
-- those words. The reason we don't do it that way in the first place is
-- because 1) the user's input might not be valid markdown and 2) even
-- if we did that, we'd still have to do this tokenization operation to
-- annotate misspellings and reconstruct the user's raw input.
doHighlightMisspellings :: HighlightSet -> S.Set Text -> [Text] -> Widget Name
doHighlightMisspellings hSet misspellings contents =
    -- Traverse the input, gathering non-whitespace into tokens and
    -- checking if they appear in the misspelling collection
    let whitelist = S.union (hUserSet hSet) (hChannelSet hSet)

        handleLine t | t == "" = txt " "
        handleLine t =
            -- For annotated tokens, coallesce tokens of the same type
            -- and add attributes for misspellings.
            let mkW (Left tok) =
                    let s = getTokenText tok
                    in if T.null s
                       then emptyWidget
                       else withDefAttr misspellingAttr $ txt $ getTokenText tok
                mkW (Right tok) =
                    let s = getTokenText tok
                    in if T.null s
                       then emptyWidget
                       else txt s

                go :: Either Token Token -> [Either Token Token] -> [Either Token Token]
                go lst [] = [lst]
                go lst (tok:toks) =
                    case (lst, tok) of
                        (Left a, Left b)   -> go (Left $ combineTokens a b) toks
                        (Right a, Right b) -> go (Right $ combineTokens a b) toks
                        _                  -> lst : go tok toks

            in hBox $ mkW <$> (go (Right $ Ignore "") $ annotatedTokens t)

        combineTokens (Ignore a) (Ignore b) = Ignore $ a <> b
        combineTokens (Check a) (Check b) = Check $ a <> b
        combineTokens (Ignore a) (Check b) = Check $ a <> b
        combineTokens (Check a) (Ignore b) = Check $ a <> b

        getTokenText (Ignore a) = a
        getTokenText (Check a) = a

        annotatedTokens t =
            -- For every token, check on whether it is a misspelling.
            -- The result is Either Token Token where the Left is a
            -- misspelling and the Right is not.
            checkMisspelling <$> tokenize t (Ignore "")

        checkMisspelling t@(Ignore _) = Right t
        checkMisspelling t@(Check s) =
            if s `S.member` whitelist
            then Right t
            else if s `S.member` misspellings
                 then Left t
                 else Right t

        ignoreChar c = isSpace c || isPunctuation c || c == '`'

        tokenize t curTok
            | T.null t = [curTok]
            | ignoreChar $ T.head t =
                case curTok of
                    Ignore s -> tokenize (T.tail t) (Ignore $ s <> (T.singleton $ T.head t))
                    Check s -> Check s : tokenize (T.tail t) (Ignore $ T.singleton $ T.head t)
            | otherwise =
                case curTok of
                    Ignore s -> Ignore s : tokenize (T.tail t) (Check $ T.singleton $ T.head t)
                    Check s -> tokenize (T.tail t) (Check $ s <> (T.singleton $ T.head t))

    in vBox $ handleLine <$> contents

renderUserCommandBox :: ChatState -> HighlightSet -> Widget Name
renderUserCommandBox st hs =
    let prompt = txt $ case st^.csEditState.cedEditMode of
            Replying _ _ -> "reply> "
            Editing _ _  ->  "edit> "
            NewPost      ->      "> "
        inputBox = renderEditor (drawEditorContents st hs) True (st^.csEditState.cedEditor)
        curContents = getEditContents $ st^.csEditState.cedEditor
        multilineContent = length curContents > 1
        multilineHints =
            hBox [ borderElem bsHorizontal
                 , str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                        st^.csEditState.cedEditor.editContentsL) <>
                         "/" <> (show $ length curContents) <> "]"
                 , hBorderWithLabel $ withDefAttr clientEmphAttr $
                   txt $ "In multi-line mode. Press " <>
                         (ppBinding (getFirstDefaultBinding ToggleMultiLineEvent)) <>
                         " to finish."
                 ]

        replyDisplay = case st^.csEditState.cedEditMode of
            Replying msg _ ->
                let msgWithoutParent = msg & mInReplyToMsg .~ NotAReply
                in hBox [ replyArrow
                        , addEllipsis $ renderMessage MessageData
                          { mdMessage           = msgWithoutParent
                          , mdUserName          = msgWithoutParent^.mUser.to (nameForUserRef st)
                          , mdIsBot             = isBotMessage msg
                          , mdParentMessage     = Nothing
                          , mdParentUserName    = Nothing
                          , mdHighlightSet      = hs
                          , mdEditThreshold     = Nothing
                          , mdShowOlderEdits    = False
                          , mdRenderReplyParent = True
                          , mdIndentBlocks      = False
                          , mdThreadState       = NoThread
                          , mdShowReactions     = True
                          }
                        ]
            _ -> emptyWidget

        commandBox = case st^.csEditState.cedEphemeral.eesMultiline of
            False ->
                let linesStr = "line" <> if numLines == 1 then "" else "s"
                    numLines = length curContents
                in vLimit 1 $ hBox $
                   prompt : if multilineContent
                            then [ withDefAttr clientEmphAttr $
                                   str $ "[" <> show numLines <> " " <> linesStr <>
                                         "; Enter: send, M-e: edit, Backspace: cancel] "
                                 , txt $ head curContents
                                 , showCursor MessageInput (Location (0,0)) $ str " "
                                 ]
                            else [inputBox]
            True -> vLimit multilineHeightLimit inputBox <=> multilineHints
    in replyDisplay <=> commandBox

renderChannelHeader :: ChatState -> HighlightSet -> ClientChannel -> Widget Name
renderChannelHeader st hs chan =
    let chnType = chan^.ccInfo.cdType
        topicStr = chan^.ccInfo.cdHeader
        userHeader u = let s = T.intercalate " " $ filter (not . T.null) parts
                           parts = [ userSigil <> u^.uiName
                                   , if (all T.null names)
                                     then mempty
                                     else "is"
                                   ] <> names <> [
                                     if T.null (u^.uiEmail)
                                     then mempty
                                     else "(" <> u^.uiEmail <> ")"
                                   ]
                           names = [ u^.uiFirstName
                                   , nick
                                   , u^.uiLastName
                                   ]
                           quote n = "\"" <> n <> "\""
                           nick = maybe "" quote $ u^.uiNickName
                       in s
        maybeTopic = if T.null topicStr
                     then ""
                     else " - " <> topicStr
        channelNameString = case chnType of
            Direct ->
                case chan^.ccInfo.cdDMUserId >>= flip userById st of
                    Nothing -> mkChannelName (chan^.ccInfo)
                    Just u -> userHeader u
            Private ->
                mkChannelName (chan^.ccInfo) <> " (Private)"
            Group ->
                mkChannelName (chan^.ccInfo) <> " (Private group)"
            _ ->
                mkChannelName (chan^.ccInfo)
        newlineToSpace '\n' = ' '
        newlineToSpace c = c

    in renderText'
         hs
         (T.map newlineToSpace (channelNameString <> maybeTopic))

renderCurrentChannelDisplay :: ChatState -> HighlightSet -> Widget Name
renderCurrentChannelDisplay st hs = header <=> messages
    where
    header = withDefAttr channelHeaderAttr $
             padRight Max $
             renderChannelHeader st hs chan
    messages = padTop Max $ padRight (Pad 1) body

    body = chatText

    chatText = case appMode st of
        MessageSelect ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        MessageSelectDeleteConfirm ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        _ ->
            cached (ChannelMessages cId) $
            renderLastMessages st hs editCutoff $
            retrogradeMsgsWithThreadStates $
            reverseMessages channelMessages

    renderMessagesWithSelect (MessageSelectState selMsgId) msgs =
        -- In this case, we want to fill the message list with messages
        -- but use the post ID as a cursor. To do this efficiently we
        -- only want to render enough messages to fill the screen.
        --
        -- If the message area is H rows high, this actually renders at
        -- most 2H rows' worth of messages and then does the appropriate
        -- cropping. This way we can simplify the math needed to figure
        -- out how to crop while bounding the number of messages we
        -- render around the cursor.
        --
        -- First, we sanity-check the application state because under
        -- some conditions, the selected message might be gone (e.g.
        -- deleted).
        let (s, (before, after)) = splitDirSeqOn (\(m, _) -> m^.mMessageId == selMsgId) msgsWithStates
            msgsWithStates = chronologicalMsgsWithThreadStates msgs
        in case s of
             Nothing ->
                 renderLastMessages st hs editCutoff before
             Just m ->
                 unsafeRenderMessageSelection (m, (before, after)) (renderSingleMessage st hs Nothing)

    cutoff = getNewMessageCutoff cId st
    editCutoff = getEditedMessageCutoff cId st
    channelMessages =
        insertTransitions (getMessageListing cId st)
                          cutoff
                          (getDateFormat st)
                          (st ^. timeZone)

    cId = st^.csCurrentChannelId
    chan = st^.csCurrentChannel

getMessageListing :: ChannelId -> ChatState -> Messages
getMessageListing cId st =
    st ^?! csChannels.folding (findChannelById cId) . ccContents . cdMessages . to (filterMessages isShown)
    where isShown m
            | st^.csResources.crUserPreferences.userPrefShowJoinLeave = True
            | otherwise = not $ isJoinLeave m

insertTransitions :: Messages -> Maybe NewMessageIndicator -> Text -> TimeZoneSeries -> Messages
insertTransitions ms cutoff = insertDateMarkers $ foldr addMessage ms newMessagesT
    where anyNondeletedNewMessages t =
              isJust $ findLatestUserMessage (not . view mDeleted) (messagesAfter t ms)
          newMessagesT = case cutoff of
              Nothing -> []
              Just Hide -> []
              Just (NewPostsAfterServerTime t)
                  | anyNondeletedNewMessages t -> [newMessagesMsg $ justAfter t]
                  | otherwise -> []
              Just (NewPostsStartingAt t)
                  | anyNondeletedNewMessages (justBefore t) -> [newMessagesMsg $ justBefore t]
                  | otherwise -> []
          newMessagesMsg d = newMessageOfType (T.pack "New Messages")
                             (C NewMessagesTransition) d

renderChannelSelectPrompt :: ChatState -> Widget Name
renderChannelSelectPrompt st =
    let e = st^.csChannelSelectState.channelSelectInput
    in withDefAttr channelSelectPromptAttr $
       (txt "Switch to channel [use ^ and $ to anchor]: ") <+>
       (renderEditor (txt . T.concat) True e)

drawMain :: Bool -> ChatState -> [Widget Name]
drawMain useColor st =
    let maybeColor = if useColor then id else forceAttr "invalid"
    in maybeColor <$>
           [ connectionLayer st
           , autocompleteLayer st
           , joinBorders $ mainInterface st
           ]

connectionLayer :: ChatState -> Widget Name
connectionLayer st =
    case st^.csConnectionStatus of
        Connected -> emptyWidget
        Disconnected ->
            Widget Fixed Fixed $ do
                ctx <- getContext
                let aw = ctx^.availWidthL
                    w = length msg + 2
                    msg = "NOT CONNECTED"
                render $ translateBy (Location (max 0 (aw - w), 0)) $
                         withDefAttr errorMessageAttr $
                         border $ str msg

messageSelectBottomBar :: ChatState -> Widget Name
messageSelectBottomBar st =
    let optionStr = if null usableOptions
                    then "(no actions available for this message)"
                    else T.intercalate " " usableOptions
        usableOptions = catMaybes $ mkOption <$> options
        mkOption (f, k, desc) = if f postMsg
                                then Just $ k <> ":" <> desc
                                else Nothing
        numURLs = Seq.length $ msgURLs postMsg
        s = if numURLs == 1 then "" else "s"
        hasURLs = numURLs > 0
        openUrlsMsg = "open " <> (T.pack $ show numURLs) <> " URL" <> s
        hasVerb = isJust (findVerbatimChunk (postMsg^.mText))
        -- make sure these keybinding pieces are up-to-date!
        ev e =
          let keyconf = st^.csResources.crConfiguration.to configUserKeys
              keymap = messageSelectKeybindings keyconf
          in T.intercalate ","
               [ ppBinding (eventToBinding b)
               | KB { kbBindingInfo = Just e'
                    , kbEvent       = b
                    } <- keymap
               , e' == e
               ]
        options = [ ( not . isGap
                    , ev YankWholeMessageEvent
                    , "yank-all"
                    )
                  , ( \m -> isFlaggable m && not (m^.mFlagged)
                    , ev FlagMessageEvent
                    , "flag"
                    )
                  , ( \m -> isFlaggable m && m^.mFlagged
                    , ev FlagMessageEvent
                    , "unflag"
                    )
                  , ( isReplyable
                    , ev ReplyMessageEvent
                    , "reply"
                    )
                  , ( not . isGap
                    , ev ViewMessageEvent
                    , "view"
                    )
                  , ( isGap
                    , ev FillGapEvent
                    , "load messages"
                    )
                  , ( \m -> isMine st m && isEditable m
                    , ev EditMessageEvent
                    , "edit"
                    )
                  , ( \m -> isMine st m && isDeletable m
                    , ev DeleteMessageEvent
                    , "delete"
                    )
                  , ( const hasURLs
                    , ev OpenMessageURLEvent
                    , openUrlsMsg
                    )
                  , ( const hasVerb
                    , ev YankMessageEvent
                    , "yank-code"
                    )
                  , ( isReactable
                    , ev ReactToMessageEvent
                    , "react"
                    )
                  ]
        Just postMsg = getSelectedMessage st

    in hBox [ borderElem bsHorizontal
            , txt "["
            , withDefAttr messageSelectStatusAttr $
              txt $ "Message select: " <> optionStr
            , txt "]"
            , hBorder
            ]

maybePreviewViewport :: Widget Name -> Widget Name
maybePreviewViewport w =
    Widget Greedy Fixed $ do
        result <- render w
        case (Vty.imageHeight $ result^.imageL) > previewMaxHeight of
            False -> return result
            True ->
                render $ vLimit previewMaxHeight $ viewport MessagePreviewViewport Vertical $
                         (Widget Fixed Fixed $ return result)

inputPreview :: ChatState -> HighlightSet -> Widget Name
inputPreview st hs | not $ st^.csShowMessagePreview = emptyWidget
                   | otherwise = thePreview
    where
    uId = myUserId st
    -- Insert a cursor sentinel into the input text just before
    -- rendering the preview. We use the inserted sentinel (which is
    -- not rendered) to get brick to ensure that the line the cursor is
    -- on is visible in the preview viewport. We put the sentinel at
    -- the *end* of the line because it will still influence markdown
    -- parsing and can create undesirable/confusing churn in the
    -- rendering while the cursor moves around. If the cursor is at the
    -- end of whatever line the user is editing, that is very unlikely
    -- to be a problem.
    curContents = getText $ (gotoEOL >>> insertChar cursorSentinel) $
                  st^.csEditState.cedEditor.editContentsL
    curStr = T.intercalate "\n" curContents
    overrideTy = case st^.csEditState.cedEditMode of
        Editing _ ty -> Just ty
        _ -> Nothing
    previewMsg = previewFromInput overrideTy uId curStr
    thePreview = let noPreview = str "(No preview)"
                     msgPreview = case previewMsg of
                       Nothing -> noPreview
                       Just pm -> if T.null curStr
                                  then noPreview
                                  else prview pm $ getParentMessage st pm
                     prview m p = renderMessage MessageData
                                  { mdMessage           = m
                                  , mdUserName          = m^.mUser.to (nameForUserRef st)
                                  , mdParentMessage     = p
                                  , mdIsBot             = isBotMessage m
                                  , mdParentUserName    = p >>= (^.mUser.to (nameForUserRef st))
                                  , mdHighlightSet      = hs
                                  , mdEditThreshold     = Nothing
                                  , mdShowOlderEdits    = False
                                  , mdRenderReplyParent = True
                                  , mdIndentBlocks      = True
                                  , mdThreadState       = NoThread
                                  , mdShowReactions     = True
                                  }
                 in (maybePreviewViewport msgPreview) <=>
                    hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

userInputArea :: ChatState -> HighlightSet -> Widget Name
userInputArea st hs =
    case appMode st of
        ChannelSelect -> renderChannelSelectPrompt st
        UrlSelect     -> hCenter $ hBox [ txt "Press "
                                        , withDefAttr clientEmphAttr $ txt "Enter"
                                        , txt " to open the selected URL or "
                                        , withDefAttr clientEmphAttr $ txt "Escape"
                                        , txt " to cancel."
                                        ]
        MessageSelectDeleteConfirm -> renderDeleteConfirm
        _             -> renderUserCommandBox st hs

renderDeleteConfirm :: Widget Name
renderDeleteConfirm =
    hCenter $ txt "Are you sure you want to delete the selected message? (y/n)"

mainInterface :: ChatState -> Widget Name
mainInterface st =
    vBox [ renderTeamSelection st
         , if st^.csShowChannelList || appMode st == ChannelSelect
           then hBox [hLimit channelListWidth (renderChannelList st), vBorder, mainDisplay]
           else mainDisplay
         , bottomBorder
         , inputPreview st hs
         , userInputArea st hs
         ]
    where
    hs = getHighlightSet st
    channelListWidth = configChannelListWidth $ st^.csResources.crConfiguration
    mainDisplay = case appMode st of
        UrlSelect -> renderUrlList st
        _         -> maybeSubdue $ renderCurrentChannelDisplay st hs

    bottomBorder = case appMode st of
        MessageSelect -> messageSelectBottomBar st
        _ -> maybeSubdue $ hBox
             [ showAttachmentCount
             , hBorder
             , showTypingUsers
             , showBusy
             ]

    showAttachmentCount =
        let count = length $ listElements $ st^.csEditState.cedAttachmentList
        in if count == 0
           then emptyWidget
           else hBox [ borderElem bsHorizontal
                     , withDefAttr clientMessageAttr $
                       txt $ "(" <> (T.pack $ show count) <> " attachment" <>
                             (if count == 1 then "" else "s") <> "; "
                     , withDefAttr clientEmphAttr $
                       txt $ ppBinding (getFirstDefaultBinding ShowAttachmentListEvent)
                     , txt " to manage)"
                     ]

    showTypingUsers = case allTypingUsers (st^.csCurrentChannel.ccInfo.cdTypingUsers) of
                        [] -> emptyWidget
                        [uId] | Just un <- usernameForUserId uId st ->
                           txt $ un <> " is typing"
                        [uId1, uId2] | Just un1 <- usernameForUserId uId1 st
                                     , Just un2 <- usernameForUserId uId2 st ->
                           txt $ un1 <> " and " <> un2 <> " are typing"
                        _ -> txt "several people are typing"

    showBusy = case st^.csWorkerIsBusy of
                 Just (Just n) -> hLimit 2 hBorder <+> txt (T.pack $ "*" <> show n)
                 Just Nothing -> hLimit 2 hBorder <+> txt "*"
                 Nothing -> emptyWidget

    maybeSubdue = if appMode st == ChannelSelect
                  then forceAttr ""
                  else id

replyArrow :: Widget a
replyArrow =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let bs = ctx^.ctxBorderStyleL
        render $ str [' ', bsCornerTL bs, '▸']

renderTeamSelection :: ChatState -> Widget a
renderTeamSelection st = case myTeamNames st of
  []   -> error "no teams"
  [_]  -> emptyWidget
  f:ts -> hBox (txt ("[" <> f <> "]") : [ txt (" " <> t <> " ") | t <- ts ])
