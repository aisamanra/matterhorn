{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Types
  ( ConnectionStatus(..)
  , HelpTopic(..)
  , MessageSelectState(..)
  , ProgramOutput(..)
  , MHEvent(..)
  , InternalEvent(..)
  , Name(..)
  , ChannelSelectMatch(..)
  , StartupStateInfo(..)
  , MHError(..)
  , AttachmentData(..)
  , CPUUsagePolicy(..)
  , OpenInBrowser(..)
  , ConnectionInfo(..)
  , SidebarUpdate(..)
  , PendingChannelChange(..)
  , clearChannelUnreadStatus
  , ChannelListEntry(..)
  , channelListEntryChannelId
  , channelListEntryUserId
  , userIdsFromZipper
  , entryIsDMEntry
  , ciHostname
  , ciPort
  , ciUsername
  , ciPassword
  , Config(..)
  , HelpScreen(..)
  , PasswordSource(..)
  , MatchType(..)
  , Mode(..)
  , ChannelSelectPattern(..)
  , PostListContents(..)
  , AuthenticationException(..)
  , BackgroundInfo(..)
  , RequestChan
  , UserFetch(..)

  , mkChannelZipperList
  , ChannelListGroup(..)

  , trimChannelSigil

  , ChannelSelectState(..)
  , channelSelectMatches
  , channelSelectInput
  , emptyChannelSelectState

  , ChatState
  , newState
  , csResources
  , csFocus
  , csCurrentChannel
  , csCurrentChannelId
  , csUrlList
  , csShowMessagePreview
  , csShowChannelList
  , csPostMap
  , csRecentChannel
  , csPostListOverlay
  , csUserListOverlay
  , csChannelListOverlay
  , csReactionEmojiListOverlay
  , csMyTeam
  , csMessageSelect
  , csConnectionStatus
  , csWorkerIsBusy
  , csChannel
  , csChannels
  , csChannelSelectState
  , csEditState
  , csClientConfig
  , csPendingChannelChange
  , csViewedMessage
  , timeZone
  , whenMode
  , setMode
  , setMode'
  , appMode

  , ChatEditState
  , emptyEditState
  , cedAttachmentList
  , cedFileBrowser
  , cedYankBuffer
  , cedSpellChecker
  , cedMisspellings
  , cedEditMode
  , cedEphemeral
  , cedEditor
  , cedInputHistory
  , cedAutocomplete
  , cedAutocompletePending

  , AutocompleteState(..)
  , acPreviousSearchString
  , acCompletionList
  , acListElementType
  , acCachedResponses

  , AutocompleteAlternative(..)
  , autocompleteAlternativeReplacement
  , SpecialMention(..)
  , specialMentionName
  , isSpecialMention

  , PostListOverlayState
  , postListSelected
  , postListPosts

  , UserSearchScope(..)
  , ChannelSearchScope(..)

  , ListOverlayState
  , listOverlaySearchResults
  , listOverlaySearchInput
  , listOverlaySearchScope
  , listOverlaySearching
  , listOverlayEnterHandler
  , listOverlayNewList
  , listOverlayFetchResults

  , getUsers

  , ChatResources(..)
  , crUserPreferences
  , crEventQueue
  , crTheme
  , crStatusUpdateChan
  , crSubprocessLog
  , crWebsocketActionChan
  , crWebsocketThreadId
  , crRequestQueue
  , crFlaggedPosts
  , crConn
  , crConfiguration
  , crSyntaxMap
  , crLogManager
  , crEmoji
  , getSession
  , getResourceSession

  , specialUserMentions

  , UserPreferences(UserPreferences)
  , userPrefShowJoinLeave
  , userPrefFlaggedPostList
  , userPrefGroupChannelPrefs
  , userPrefDirectChannelPrefs
  , userPrefTeammateNameDisplayMode
  , dmChannelShowPreference
  , groupChannelShowPreference

  , defaultUserPreferences
  , setUserPreferences

  , WebsocketAction(..)

  , Cmd(..)
  , commandName
  , CmdArgs(..)

  , MH
  , runMHEvent
  , scheduleUserFetches
  , scheduleUserStatusFetches
  , getScheduledUserFetches
  , getScheduledUserStatusFetches
  , mh
  , generateUUID
  , generateUUID_IO
  , mhSuspendAndResume
  , mhHandleEventLensed
  , St.gets
  , mhError

  , mhLog
  , mhGetIOLogger
  , ioLogWithManager
  , LogContext(..)
  , withLogContext
  , withLogContextChannelId
  , getLogContext
  , LogMessage(..)
  , LogCommand(..)
  , LogCategory(..)

  , LogManager(..)
  , startLoggingToFile
  , stopLoggingToFile
  , requestLogSnapshot
  , requestLogDestination
  , sendLogMessage

  , requestQuit
  , getMessageForPostId
  , getParentMessage
  , resetSpellCheckTimer
  , withChannel
  , withChannelOrDefault
  , userList
  , resetAutocomplete
  , hasUnread
  , hasUnread'
  , isMine
  , setUserStatus
  , myUser
  , myUserId
  , myTeamId
  , myTeamNames
  , usernameForUserId
  , userByUsername
  , userByNickname
  , channelIdByChannelName
  , channelIdByUsername
  , channelByName
  , userById
  , allUserIds
  , addNewUser
  , useNickname
  , useNickname'
  , displayNameForUserId
  , displayNameForUser
  , raiseInternalEvent
  , getNewMessageCutoff
  , getEditedMessageCutoff

  , normalChannelSigil

  , HighlightSet(..)
  , UserSet
  , ChannelSet
  , getHighlightSet

  , module Types.Channels
  , module Types.Messages
  , module Types.Posts
  , module Types.Users
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Brick
import           Brick ( EventM, Next )
import           Brick.Main ( invalidateCache, invalidateCacheEntry )
import           Brick.AttrMap ( AttrMap )
import           Brick.BChan
import           Brick.Widgets.Edit ( Editor, editor )
import           Brick.Widgets.List ( List, list )
import qualified Brick.Widgets.FileBrowser as FB
import           Control.Concurrent ( ThreadId )
import           Control.Concurrent.Async ( Async )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException )
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as R
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.Ord ( comparing )
import qualified Data.HashMap.Strict as HM
import           Data.List ( sortBy )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock ( UTCTime, getCurrentTime, addUTCTime )
import           Data.UUID ( UUID )
import qualified Data.Vector as Vec
import           Lens.Micro.Platform ( at, makeLenses, lens, (%~), (^?!), (.=)
                                     , (%=), (^?), (.~)
                                     , _Just, Traversal', preuse, to
                                     , SimpleGetter
                                     )
import           Network.Connection ( HostNotResolved, HostCannotConnect )
import           Skylighting.Types ( SyntaxMap )
import           System.Exit ( ExitCode )
import           System.Random ( randomIO )
import           Text.Aspell ( Aspell )

import           Network.Mattermost ( ConnectionData )
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types
import           Network.Mattermost.Types.Config
import           Network.Mattermost.WebSocket ( WebsocketEvent )

import           InputHistory
import           Emoji
import           Types.Common
import           Types.Channels
import           Types.DirectionalSeq ( emptyDirSeq )
import           Types.KeyEvents
import           Types.Messages
import           Types.Posts
import           Types.Users
import           Zipper ( Zipper, toList, fromList, unsafeFocus )


-- * Configuration

-- | A user password is either given to us directly, or a command
-- which we execute to find the password.
data PasswordSource =
    PasswordString Text
    | PasswordCommand Text
    deriving (Eq, Read, Show)

-- | The type of channel list group headings.
data ChannelListGroup =
    ChannelGroupPublicChannels
    | ChannelGroupDirectMessages
    deriving (Eq)

-- | The type of channel list entries.
data ChannelListEntry =
    CLChannel ChannelId
    -- ^ A non-DM entry
    | CLUserDM ChannelId UserId
    -- ^ A single-user DM entry
    | CLGroupDM ChannelId
    -- ^ A multi-user DM entry
    deriving (Eq, Show)

-- | This is how we represent the user's configuration. Most fields
-- correspond to configuration file settings (see Config.hs) but some
-- are for internal book-keeping purposes only.
data Config =
    Config { configUser :: Maybe Text
           -- ^ The username to use when connecting.
           , configHost :: Maybe Text
           -- ^ The hostname to use when connecting.
           , configTeam :: Maybe Text
           -- ^ The team name to use when connecting.
           , configPort :: Int
           -- ^ The port to use when connecting.
           , configPass :: Maybe PasswordSource
           -- ^ The password source to use when connecting.
           , configTimeFormat :: Maybe Text
           -- ^ The format string for timestamps.
           , configDateFormat :: Maybe Text
           -- ^ The format string for dates.
           , configTheme :: Maybe Text
           -- ^ The name of the theme to use.
           , configThemeCustomizationFile :: Maybe Text
           -- ^ The path to the theme customization file, if any.
           , configSmartBacktick :: Bool
           -- ^ Whether to enable smart quoting characters.
           , configURLOpenCommand :: Maybe Text
           -- ^ The command to use to open URLs.
           , configURLOpenCommandInteractive :: Bool
           -- ^ Whether the URL-opening command is interactive (i.e.
           -- whether it should be given control of the terminal).
           , configActivityNotifyCommand :: Maybe T.Text
           -- ^ The command to run for activity notifications.
           , configActivityBell :: Bool
           -- ^ Whether to ring the terminal bell on activity.
           , configShowBackground :: BackgroundInfo
           -- ^ Whether to show async background worker thread info.
           , configShowMessagePreview :: Bool
           -- ^ Whether to show the message preview area.
           , configShowChannelList :: Bool
           -- ^ Whether to show the channel list.
           , configEnableAspell :: Bool
           -- ^ Whether to enable Aspell spell checking.
           , configAspellDictionary :: Maybe Text
           -- ^ A specific Aspell dictionary name to use.
           , configUnsafeUseHTTP :: Bool
           -- ^ Whether to permit an insecure HTTP connection.
           , configChannelListWidth :: Int
           -- ^ The width, in columns, of the channel list sidebar.
           , configLogMaxBufferSize :: Int
           -- ^ The maximum size, in log entries, of the internal log
           -- message buffer.
           , configShowOlderEdits :: Bool
           -- ^ Whether to highlight the edit indicator on edits made
           -- prior to the beginning of the current session.
           , configShowTypingIndicator :: Bool
           -- ^ Whether to show the typing indicator for other users,
           -- and whether to send typing notifications to other users.
           , configAbsPath :: Maybe FilePath
           -- ^ A book-keeping field for the absolute path to the
           -- configuration. (Not a user setting.)
           , configUserKeys :: KeyConfig
           -- ^ The user's keybinding configuration.
           , configHyperlinkingMode :: Bool
           -- ^ Whether to enable terminal hyperlinking mode.
           , configSyntaxDirs :: [FilePath]
           -- ^ The search path for syntax description XML files.
           , configDirectChannelExpirationDays :: Int
           -- ^ The number of days to show a user in the channel menu after a direct
           -- message with them.
           , configCpuUsagePolicy :: CPUUsagePolicy
           -- ^ The CPU usage policy for the application.
           } deriving (Eq, Show)

-- | The policy for CPU usage.
--
-- The idea is that Matterhorn can benefit from using multiple CPUs,
-- but the exact number is application-determined. We expose this policy
-- setting to the user in the configuration.
data CPUUsagePolicy =
    SingleCPU
    -- ^ Constrain the application to use one CPU.
    | MultipleCPUs
    -- ^ Permit the usage of multiple CPUs (the exact number is
    -- determined by the application).
    deriving (Eq, Show)

-- | The state of the UI diagnostic indicator for the async worker
-- thread.
data BackgroundInfo =
    Disabled
    -- ^ Disable (do not show) the indicator.
    | Active
    -- ^ Show the indicator when the thread is working.
    | ActiveCount
    -- ^ Show the indicator when the thread is working, but include the
    -- thread's work queue length.
    deriving (Eq, Show)

data UserPreferences =
    UserPreferences { _userPrefShowJoinLeave     :: Bool
                    , _userPrefFlaggedPostList   :: Seq FlaggedPost
                    , _userPrefGroupChannelPrefs :: HashMap ChannelId Bool
                    , _userPrefDirectChannelPrefs :: HashMap UserId Bool
                    , _userPrefTeammateNameDisplayMode :: Maybe TeammateNameDisplayMode
                    }

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = fromMaybe False $
    hasUnread' <$> findChannelById cId (_csChannels st)

hasUnread' :: ClientChannel -> Bool
hasUnread' chan = fromMaybe False $ do
    let info = _ccInfo chan
    lastViewTime <- _cdViewed info
    return $ ((_cdUpdated info) > lastViewTime) ||
             (isJust $ _cdEditedMessageThreshold info)

mkChannelZipperList :: UTCTime
                    -> Config
                    -> Maybe ClientConfig
                    -> UserPreferences
                    -> ClientChannels
                    -> Users
                    -> [(ChannelListGroup, [ChannelListEntry])]
mkChannelZipperList now config cconfig prefs cs us =
    [ (ChannelGroupPublicChannels, getNonDMChannelIdsInOrder cs)
    , (ChannelGroupDirectMessages, getDMChannelsInOrder now config cconfig prefs us cs)
    ]

getNonDMChannelIdsInOrder :: ClientChannels -> [ChannelListEntry]
getNonDMChannelIdsInOrder cs =
    let matches (_, info) = info^.ccInfo.cdType `notElem` [Direct, Group]
    in fmap (CLChannel . fst) $
       sortBy (comparing ((^.ccInfo.cdName) . snd)) $
       filteredChannels matches cs

getDMChannelsInOrder :: UTCTime
                     -> Config
                     -> Maybe ClientConfig
                     -> UserPreferences
                     -> Users
                     -> ClientChannels
                     -> [ChannelListEntry]
getDMChannelsInOrder now config cconfig prefs us cs =
    let oneOnOneDmChans = getDMChannels now config cconfig prefs us cs
        groupChans = getGroupDMChannels now config prefs cs
        allDmChans = groupChans <> oneOnOneDmChans
        sorter (u1, n1, _) (u2, n2, _) =
            if u1 == u2
            then compare n1 n2
            else if u1 && not u2
                 then LT
                 else GT
        sorted = sortBy sorter allDmChans
        third (_, _, c) = c
    in third <$> sorted

useNickname' :: Maybe ClientConfig -> UserPreferences -> Bool
useNickname' clientConfig prefs =
    let serverSetting = case clientConfig^?_Just.to clientConfigTeammateNameDisplay of
            Just TMNicknameOrFullname -> Just True
            _                         -> Nothing
        accountSetting = (== TMNicknameOrFullname) <$> (_userPrefTeammateNameDisplayMode prefs)
        fallback = False
    in fromMaybe fallback $ accountSetting <|> serverSetting

displayNameForUser :: UserInfo -> Maybe ClientConfig -> UserPreferences -> Text
displayNameForUser u clientConfig prefs
    | useNickname' clientConfig prefs =
        fromMaybe (u^.uiName) (u^.uiNickName)
    | otherwise =
        u^.uiName

getGroupDMChannels :: UTCTime
                   -> Config
                   -> UserPreferences
                   -> ClientChannels
                   -> [(Bool, T.Text, ChannelListEntry)]
getGroupDMChannels now config prefs cs =
    let matches (_, info) = info^.ccInfo.cdType == Group &&
                            groupChannelShouldAppear now config prefs info
    in fmap (\(cId, ch) -> (hasUnread' ch, ch^.ccInfo.cdName, CLGroupDM cId)) $
       filteredChannels matches cs

getDMChannels :: UTCTime
              -> Config
              -> Maybe ClientConfig
              -> UserPreferences
              -> Users
              -> ClientChannels
              -> [(Bool, T.Text, ChannelListEntry)]
getDMChannels now config cconfig prefs us cs =
    let mapping = allDmChannelMappings cs
        mappingWithUserInfo = catMaybes $ getInfo <$> mapping
        getInfo (uId, cId) = do
            c <- findChannelById cId cs
            u <- findUserById uId us
            case u^.uiDeleted of
                True -> Nothing
                False ->
                    if dmChannelShouldAppear now config prefs c
                    then return (hasUnread' c, displayNameForUser u cconfig prefs, CLUserDM cId uId)
                    else Nothing
    in mappingWithUserInfo

-- Always show a DM channel if it has unread activity.
--
-- If it has no unread activity and if the preferences explicitly say to
-- hide it, hide it.
--
-- Otherwise, only show it if at least one of the other conditions are
-- met (see 'or' below).
dmChannelShouldAppear :: UTCTime -> Config -> UserPreferences -> ClientChannel -> Bool
dmChannelShouldAppear now config prefs c =
    let ndays = configDirectChannelExpirationDays config
        localCutoff = addUTCTime (nominalDay * (-(fromIntegral ndays))) now
        cutoff = ServerTime localCutoff
        updated = c^.ccInfo.cdUpdated
        Just uId = c^.ccInfo.cdDMUserId
    in if hasUnread' c || maybe False (>= localCutoff) (c^.ccInfo.cdSidebarShowOverride)
       then True
       else case dmChannelShowPreference prefs uId of
           Just False -> False
           _ -> or [
                   -- The channel was updated recently enough
                     updated >= cutoff
                   ]

groupChannelShouldAppear :: UTCTime -> Config -> UserPreferences -> ClientChannel -> Bool
groupChannelShouldAppear now config prefs c =
    let ndays = configDirectChannelExpirationDays config
        localCutoff = addUTCTime (nominalDay * (-(fromIntegral ndays))) now
        cutoff = ServerTime localCutoff
        updated = c^.ccInfo.cdUpdated
    in if hasUnread' c || maybe False (>= localCutoff) (c^.ccInfo.cdSidebarShowOverride)
       then True
       else case groupChannelShowPreference prefs (c^.ccInfo.cdChannelId) of
           Just False -> False
           _ -> or [
                   -- The channel was updated recently enough
                     updated >= cutoff
                   ]

dmChannelShowPreference :: UserPreferences -> UserId -> Maybe Bool
dmChannelShowPreference ps uId = HM.lookup uId (_userPrefDirectChannelPrefs ps)

groupChannelShowPreference :: UserPreferences -> ChannelId -> Maybe Bool
groupChannelShowPreference ps cId = HM.lookup cId (_userPrefGroupChannelPrefs ps)

-- * Internal Names and References

-- | This 'Name' type is the type used in 'brick' to identify various
-- parts of the interface.
data Name =
    ChannelMessages ChannelId
    | MessageInput
    | ChannelList
    | HelpViewport
    | HelpText
    | ScriptHelpText
    | ThemeHelpText
    | SyntaxHighlightHelpText
    | KeybindingHelpText
    | ChannelSelectString
    | CompletionAlternatives
    | CompletionList
    | JoinChannelList
    | UrlList
    | MessagePreviewViewport
    | UserListSearchInput
    | JoinChannelListSearchInput
    | UserListSearchResults
    | ViewMessageArea
    | ChannelSidebar
    | ChannelSelectInput
    | AttachmentList
    | AttachmentFileBrowser
    | MessageReactionsArea
    | ReactionEmojiList
    | ReactionEmojiListInput
    deriving (Eq, Show, Ord)

-- | The sum type of exceptions we expect to encounter on authentication
-- failure. We encode them explicitly here so that we can print them in
-- a more user-friendly manner than just 'show'.
data AuthenticationException =
    ConnectError HostCannotConnect
    | ResolveError HostNotResolved
    | AuthIOError IOError
    | LoginError LoginFailureException
    | OtherAuthError SomeException
    deriving (Show)

-- | Our 'ConnectionInfo' contains exactly as much information as is
-- necessary to start a connection with a Mattermost server. This is
-- built up during interactive authentication and then is used to log
-- in.
data ConnectionInfo =
    ConnectionInfo { _ciHostname :: Text
                   , _ciPort     :: Int
                   , _ciUsername :: Text
                   , _ciPassword :: Text
                   }

-- | We want to continue referring to posts by their IDs, but we don't
-- want to have to synthesize new valid IDs for messages from the client
-- itself (like error messages or informative client responses). To that
-- end, a PostRef can be either a PostId or a newly-generated client ID.
data PostRef
    = MMId PostId
    | CLId Int
    deriving (Eq, Show)

-- Sigils
normalChannelSigil :: Text
normalChannelSigil = "~"

-- ** Channel-matching types

-- | A match in channel selection mode.
data ChannelSelectMatch =
    ChannelSelectMatch { nameBefore     :: Text
                       -- ^ The content of the match before the user's
                       -- matching input.
                       , nameMatched    :: Text
                       -- ^ The potion of the name that matched the
                       -- user's input.
                       , nameAfter      :: Text
                       -- ^ The portion of the name that came after the
                       -- user's matching input.
                       , matchFull      :: Text
                       -- ^ The full string for this entry so it doesn't
                       -- have to be reassembled from the parts above.
                       , matchEntry     :: ChannelListEntry
                       -- ^ The original entry data corresponding to the
                       -- text match.
                       }
                       deriving (Eq, Show)

data ChannelSelectPattern = CSP MatchType Text
                          | CSPAny
                          deriving (Eq, Show)

data MatchType =
    Prefix
    | Suffix
    | Infix
    | Equal
    | PrefixDMOnly
    | PrefixNonDMOnly
    deriving (Eq, Show)

-- * Application State Values

data ProgramOutput =
    ProgramOutput { program :: FilePath
                  , programArgs :: [String]
                  , programStdout :: String
                  , programStdoutExpected :: Bool
                  , programStderr :: String
                  , programExitCode :: ExitCode
                  }

defaultUserPreferences :: UserPreferences
defaultUserPreferences =
    UserPreferences { _userPrefShowJoinLeave     = True
                    , _userPrefFlaggedPostList   = mempty
                    , _userPrefGroupChannelPrefs = mempty
                    , _userPrefDirectChannelPrefs = mempty
                    , _userPrefTeammateNameDisplayMode = Nothing
                    }

setUserPreferences :: Seq Preference -> UserPreferences -> UserPreferences
setUserPreferences = flip (F.foldr go)
    where go p u
            | Just fp <- preferenceToFlaggedPost p =
              u { _userPrefFlaggedPostList =
                  _userPrefFlaggedPostList u Seq.|> fp
                }
            | Just gp <- preferenceToDirectChannelShowStatus p =
              u { _userPrefDirectChannelPrefs =
                  HM.insert
                    (directChannelShowUserId gp)
                    (directChannelShowValue gp)
                    (_userPrefDirectChannelPrefs u)
                }
            | Just gp <- preferenceToGroupChannelPreference p =
              u { _userPrefGroupChannelPrefs =
                  HM.insert
                    (groupChannelId gp)
                    (groupChannelShow gp)
                    (_userPrefGroupChannelPrefs u)
                }
            | preferenceName p == PreferenceName "join_leave" =
              u { _userPrefShowJoinLeave =
                  preferenceValue p /= PreferenceValue "false" }
            | preferenceCategory p == PreferenceCategoryDisplaySettings &&
              preferenceName p == PreferenceName "name_format" =
                  let PreferenceValue txt = preferenceValue p
                  in u { _userPrefTeammateNameDisplayMode = Just $ teammateDisplayModeFromText txt }
            | otherwise = u

-- | Log message tags.
data LogCategory =
    LogGeneral
    | LogAPI
    | LogWebsocket
    | LogError
    deriving (Eq, Show)

-- | A log message.
data LogMessage =
    LogMessage { logMessageText :: !Text
               -- ^ The text of the log message.
               , logMessageContext :: !(Maybe LogContext)
               -- ^ The optional context information relevant to the log
               -- message.
               , logMessageCategory :: !LogCategory
               -- ^ The category of the log message.
               , logMessageTimestamp :: !UTCTime
               -- ^ The timestamp of the log message.
               }
               deriving (Show)

-- | A logging thread command.
data LogCommand =
    LogToFile FilePath
    -- ^ Start logging to the specified path.
    | LogAMessage !LogMessage
    -- ^ Log the specified message.
    | StopLogging
    -- ^ Stop any active logging.
    | ShutdownLogging
    -- ^ Shut down.
    | GetLogDestination
    -- ^ Ask the logging thread about its active logging destination.
    | LogSnapshot FilePath
    -- ^ Ask the logging thread to dump the current buffer to the
    -- specified destination.
    deriving (Show)

-- | A handle to the log manager thread.
data LogManager =
    LogManager { logManagerCommandChannel :: STM.TChan LogCommand
               , logManagerHandle :: Async ()
               }

startLoggingToFile :: LogManager -> FilePath -> IO ()
startLoggingToFile mgr loc = sendLogCommand mgr $ LogToFile loc

stopLoggingToFile :: LogManager -> IO ()
stopLoggingToFile mgr = sendLogCommand mgr StopLogging

requestLogSnapshot :: LogManager -> FilePath -> IO ()
requestLogSnapshot mgr path = sendLogCommand mgr $ LogSnapshot path

requestLogDestination :: LogManager -> IO ()
requestLogDestination mgr = sendLogCommand mgr GetLogDestination

sendLogMessage :: LogManager -> LogMessage -> IO ()
sendLogMessage mgr lm = sendLogCommand mgr $ LogAMessage lm

sendLogCommand :: LogManager -> LogCommand -> IO ()
sendLogCommand mgr c =
    STM.atomically $ STM.writeTChan (logManagerCommandChannel mgr) c

-- | 'ChatResources' represents configuration and connection-related
-- information, as opposed to current model or view information.
-- Information that goes in the 'ChatResources' value should be limited
-- to information that we read or set up prior to setting up the bulk of
-- the application state.
data ChatResources =
    ChatResources { _crSession             :: Session
                  , _crWebsocketThreadId   :: Maybe ThreadId
                  , _crConn                :: ConnectionData
                  , _crRequestQueue        :: RequestChan
                  , _crEventQueue          :: BChan MHEvent
                  , _crSubprocessLog       :: STM.TChan ProgramOutput
                  , _crWebsocketActionChan :: STM.TChan WebsocketAction
                  , _crTheme               :: AttrMap
                  , _crStatusUpdateChan    :: STM.TChan (Zipper ChannelListGroup ChannelListEntry)
                  , _crConfiguration       :: Config
                  , _crFlaggedPosts        :: Set PostId
                  , _crUserPreferences     :: UserPreferences
                  , _crSyntaxMap           :: SyntaxMap
                  , _crLogManager          :: LogManager
                  , _crEmoji               :: EmojiCollection
                  }

-- | A "special" mention that does not map to a specific user, but is an
-- alias that the server uses to notify users.
data SpecialMention =
    MentionAll
    -- ^ @all: notify everyone in the channel.
    | MentionChannel
    -- ^ @channel: notify everyone in the channel.

data AutocompleteAlternative =
    UserCompletion User Bool
    -- ^ User, plus whether the user is in the channel that triggered
    -- the autocomplete
    | SpecialMention SpecialMention
    -- ^ A special mention.
    | ChannelCompletion Bool Channel
    -- ^ Channel, plus whether the user is a member of the channel
    | SyntaxCompletion Text
    -- ^ Name of a skylighting syntax definition
    | CommandCompletion Text Text Text
    -- ^ Name of a slash command, argspec, and description
    | EmojiCompletion Text
    -- ^ The text of an emoji completion

specialMentionName :: SpecialMention -> Text
specialMentionName MentionChannel = "channel"
specialMentionName MentionAll = "all"

isSpecialMention :: T.Text -> Bool
isSpecialMention n = isJust $ lookup (T.toLower $ trimUserSigil n) pairs
    where
        pairs = mkPair <$> mentions
        mentions = [ MentionChannel
                   , MentionAll
                   ]
        mkPair v = (specialMentionName v, v)

autocompleteAlternativeReplacement :: AutocompleteAlternative -> Text
autocompleteAlternativeReplacement (EmojiCompletion e) =
    ":" <> e <> ":"
autocompleteAlternativeReplacement (SpecialMention m) =
    userSigil <> specialMentionName m
autocompleteAlternativeReplacement (UserCompletion u _) =
    userSigil <> userUsername u
autocompleteAlternativeReplacement (ChannelCompletion _ c) =
    normalChannelSigil <> (sanitizeUserText $ channelName c)
autocompleteAlternativeReplacement (SyntaxCompletion t) =
    "```" <> t
autocompleteAlternativeReplacement (CommandCompletion t _ _) =
    "/" <> t

data AutocompleteState =
    AutocompleteState { _acPreviousSearchString :: Text
                      -- ^ The search string used for the
                      -- currently-displayed autocomplete results, for
                      -- use in deciding whether to issue another server
                      -- query
                      , _acCompletionList :: List Name AutocompleteAlternative
                      -- ^ The list of alternatives that the user
                      -- selects from
                      , _acListElementType :: Text
                      -- ^ The label (plural noun, e.g. "Users") used to
                      -- display the result list to the user
                      , _acCachedResponses :: HM.HashMap Text [AutocompleteAlternative]
                      -- ^ A cache of alternative lists, keyed on search
                      -- string, for use in avoiding server requests.
                      -- The idea here is that users type quickly enough
                      -- (and edit their input) that would normally lead
                      -- to rapid consecutive requests, some for the
                      -- same strings during editing, that we can avoid
                      -- that by caching them here. Note that this cache
                      -- gets destroyed whenever autocompletion is not
                      -- on, so this cache does not live very long.
                      }

-- | The 'ChatEditState' value contains the editor widget itself as well
-- as history and metadata we need for editing-related operations.
data ChatEditState =
    ChatEditState { _cedEditor :: Editor Text Name
                  , _cedEditMode :: EditMode
                  , _cedEphemeral :: EphemeralEditState
                  , _cedInputHistory :: InputHistory
                  -- ^ The map of per-channel input history for the
                  -- application. We don't distribute the per-channel
                  -- history into the per-channel states (like we do
                  -- for other per-channel state) since keeping it
                  -- under the InputHistory banner lets us use a nicer
                  -- startup/shutdown disk file management API.
                  , _cedYankBuffer :: Text
                  , _cedSpellChecker :: Maybe (Aspell, IO ())
                  , _cedMisspellings :: Set Text
                  , _cedAutocomplete :: Maybe AutocompleteState
                  -- ^ The autocomplete state. The autocompletion UI is
                  -- showing only when this state is present.
                  , _cedAutocompletePending :: Maybe Text
                  -- ^ The search string associated with the latest
                  -- in-flight autocompletion request. This is used to
                  -- determine whether any (potentially late-arriving)
                  -- API responses are for stale queries since the user
                  -- can type more quickly than the server can get us
                  -- the results, and we wouldn't want to show results
                  -- associated with old editor states.
                  , _cedAttachmentList :: List Name AttachmentData
                  -- ^ The list of attachments to be uploaded with the
                  -- post being edited.
                  , _cedFileBrowser :: FB.FileBrowser Name
                  -- ^ The browser for selecting attachment files.
                  }

-- | An attachment.
data AttachmentData =
    AttachmentData { attachmentDataFileInfo :: FB.FileInfo
                   , attachmentDataBytes :: BS.ByteString
                   }
                   deriving (Eq, Show)

-- | We can initialize a new 'ChatEditState' value with just an edit
-- history, which we save locally.
emptyEditState :: InputHistory -> Maybe (Aspell, IO ()) -> IO ChatEditState
emptyEditState hist sp = do
    browser <- FB.newFileBrowser FB.selectNonDirectories AttachmentFileBrowser Nothing
    return ChatEditState { _cedEditor               = editor MessageInput Nothing ""
                         , _cedEphemeral            = defaultEphemeralEditState
                         , _cedInputHistory         = hist
                         , _cedEditMode             = NewPost
                         , _cedYankBuffer           = ""
                         , _cedSpellChecker         = sp
                         , _cedMisspellings         = mempty
                         , _cedAutocomplete         = Nothing
                         , _cedAutocompletePending  = Nothing
                         , _cedAttachmentList       = list AttachmentList mempty 1
                         , _cedFileBrowser          = browser
                         }

-- | A 'RequestChan' is a queue of operations we have to perform in the
-- background to avoid blocking on the main loop
type RequestChan = STM.TChan (IO (Maybe (MH ())))

-- | The 'HelpScreen' type represents the set of possible 'Help'
-- dialogues we have to choose from.
data HelpScreen =
    MainHelp
    | ScriptHelp
    | ThemeHelp
    | SyntaxHighlightHelp
    | KeybindingHelp
    deriving (Eq)

-- | Help topics
data HelpTopic =
    HelpTopic { helpTopicName         :: Text
              , helpTopicDescription  :: Text
              , helpTopicScreen       :: HelpScreen
              , helpTopicViewportName :: Name
              }
              deriving (Eq)

-- | Mode type for the current contents of the post list overlay
data PostListContents =
    PostListFlagged
    | PostListSearch Text Bool -- for the query and search status
    deriving (Eq)

-- | The 'Mode' represents the current dominant UI activity
data Mode =
    Main
    | ShowHelp HelpTopic
    | ChannelSelect
    | UrlSelect
    | LeaveChannelConfirm
    | DeleteChannelConfirm
    | MessageSelect
    | MessageSelectDeleteConfirm
    | PostListOverlay PostListContents
    | UserListOverlay
    | ReactionEmojiListOverlay
    | ChannelListOverlay
    | ViewMessage
    | ManageAttachments
    | ManageAttachmentsBrowseFiles
    deriving (Eq)

-- | We're either connected or we're not.
data ConnectionStatus = Connected | Disconnected deriving (Eq)

-- | This is the giant bundle of fields that represents the current
-- state of our application at any given time. Some of this should be
-- broken out further, but hasn't yet been.
data ChatState =
    ChatState { _csResources :: ChatResources
              -- ^ Global application-wide resources that don't change
              -- much.
              , _csFocus :: Zipper ChannelListGroup ChannelListEntry
              -- ^ The channel sidebar zipper that tracks which channel
              -- is selected.
              , _csMe :: User
              -- ^ The authenticated user.
              , _csMyTeam :: Zipper () Team
              -- ^ The current active team of the authenticated user.
              , _csChannels :: ClientChannels
              -- ^ The channels that we are showing, including their
              -- message lists.
              , _csPostMap :: HashMap PostId Message
              -- ^ The map of post IDs to messages. This allows us to
              -- access messages by ID without having to linearly scan
              -- channel message lists.
              , _csUsers :: Users
              -- ^ All of the users we know about.
              , _timeZone :: TimeZoneSeries
              -- ^ The client time zone.
              , _csEditState :: ChatEditState
              -- ^ The state of the input box used for composing and
              -- editing messages and commands.
              , _csMode :: Mode
              -- ^ The current application mode. This is used to
              -- dispatch to different rendering and event handling
              -- routines.
              , _csShowMessagePreview :: Bool
              -- ^ Whether to show the message preview area.
              , _csShowChannelList :: Bool
              -- ^ Whether to show the channe list.
              , _csChannelSelectState :: ChannelSelectState
              -- ^ The state of the user's input and selection for
              -- channel selection mode.
              , _csRecentChannel :: Maybe ChannelId
              -- ^ The most recently-selected channel, if any.
              , _csUrlList :: List Name LinkChoice
              -- ^ The URL list used to show URLs drawn from messages in
              -- a channel.
              , _csConnectionStatus :: ConnectionStatus
              -- ^ Our view of the connection status.
              , _csWorkerIsBusy :: Maybe (Maybe Int)
              -- ^ Whether the async worker thread is busy, and its
              -- queue length if so.
              , _csMessageSelect :: MessageSelectState
              -- ^ The state of message selection mode.
              , _csPostListOverlay :: PostListOverlayState
              -- ^ The state of the post list overlay.
              , _csUserListOverlay :: ListOverlayState UserInfo UserSearchScope
              -- ^ The state of the user list overlay.
              , _csChannelListOverlay :: ListOverlayState Channel ChannelSearchScope
              -- ^ The state of the user list overlay.
              , _csReactionEmojiListOverlay :: ListOverlayState (Bool, T.Text) ()
              -- ^ The state of the reaction emoji list overlay.
              , _csClientConfig :: Maybe ClientConfig
              -- ^ The Mattermost client configuration, as we understand it.
              , _csPendingChannelChange :: Maybe PendingChannelChange
              -- ^ A pending channel change that we need to apply once
              -- the channel in question is available. We set this up
              -- when we need to change to a channel in the sidebar, but
              -- it isn't even there yet because we haven't loaded its
              -- metadata.
              , _csViewedMessage :: Maybe Message
              -- ^ Set when the ViewMessage mode is active. The message
              -- being viewed.
              }

data PendingChannelChange =
    ChangeByChannelId ChannelId
    | ChangeByUserId UserId
    deriving (Eq, Show)

-- | Startup state information that is constructed prior to building a
-- ChatState.
data StartupStateInfo =
    StartupStateInfo { startupStateResources      :: ChatResources
                     , startupStateChannelZipper  :: Zipper ChannelListGroup ChannelListEntry
                     , startupStateConnectedUser  :: User
                     , startupStateTeam           :: Zipper () Team
                     , startupStateTimeZone       :: TimeZoneSeries
                     , startupStateInitialHistory :: InputHistory
                     , startupStateSpellChecker   :: Maybe (Aspell, IO ())
                     }

newState :: StartupStateInfo -> IO ChatState
newState (StartupStateInfo {..}) = do
    editState <- emptyEditState startupStateInitialHistory startupStateSpellChecker
    return ChatState { _csResources                   = startupStateResources
                     , _csFocus                       = startupStateChannelZipper
                     , _csMe                          = startupStateConnectedUser
                     , _csMyTeam                      = startupStateTeam
                     , _csChannels                    = noChannels
                     , _csPostMap                     = HM.empty
                     , _csUsers                       = noUsers
                     , _timeZone                      = startupStateTimeZone
                     , _csEditState                   = editState
                     , _csMode                        = Main
                     , _csShowMessagePreview          = configShowMessagePreview $ _crConfiguration startupStateResources
                     , _csShowChannelList             = configShowChannelList $ _crConfiguration startupStateResources
                     , _csChannelSelectState          = emptyChannelSelectState
                     , _csRecentChannel               = Nothing
                     , _csUrlList                     = list UrlList mempty 2
                     , _csConnectionStatus            = Connected
                     , _csWorkerIsBusy                = Nothing
                     , _csMessageSelect               = MessageSelectState Nothing
                     , _csPostListOverlay             = PostListOverlayState emptyDirSeq Nothing
                     , _csUserListOverlay             = nullUserListOverlayState
                     , _csChannelListOverlay          = nullChannelListOverlayState
                     , _csReactionEmojiListOverlay    = nullEmojiListOverlayState
                     , _csClientConfig                = Nothing
                     , _csPendingChannelChange        = Nothing
                     , _csViewedMessage               = Nothing
                     }

nullChannelListOverlayState :: ListOverlayState Channel ChannelSearchScope
nullChannelListOverlayState =
    let newList rs = list JoinChannelList rs 2
    in ListOverlayState { _listOverlaySearchResults  = newList mempty
                        , _listOverlaySearchInput    = editor JoinChannelListSearchInput (Just 1) ""
                        , _listOverlaySearchScope    = AllChannels
                        , _listOverlaySearching      = False
                        , _listOverlayEnterHandler   = const $ return False
                        , _listOverlayNewList        = newList
                        , _listOverlayFetchResults   = const $ const $ const $ return mempty
                        }

nullUserListOverlayState :: ListOverlayState UserInfo UserSearchScope
nullUserListOverlayState =
    let newList rs = list UserListSearchResults rs 1
    in ListOverlayState { _listOverlaySearchResults  = newList mempty
                        , _listOverlaySearchInput    = editor UserListSearchInput (Just 1) ""
                        , _listOverlaySearchScope    = AllUsers Nothing
                        , _listOverlaySearching      = False
                        , _listOverlayEnterHandler   = const $ return False
                        , _listOverlayNewList        = newList
                        , _listOverlayFetchResults   = const $ const $ const $ return mempty
                        }

nullEmojiListOverlayState :: ListOverlayState (Bool, T.Text) ()
nullEmojiListOverlayState =
    let newList rs = list ReactionEmojiList rs 1
    in ListOverlayState { _listOverlaySearchResults  = newList mempty
                        , _listOverlaySearchInput    = editor ReactionEmojiListInput (Just 1) ""
                        , _listOverlaySearchScope    = ()
                        , _listOverlaySearching      = False
                        , _listOverlayEnterHandler   = const $ return False
                        , _listOverlayNewList        = newList
                        , _listOverlayFetchResults   = const $ const $ const $ return mempty
                        }

-- | The state of channel selection mode.
data ChannelSelectState =
    ChannelSelectState { _channelSelectInput :: Editor Text Name
                       , _channelSelectMatches :: Zipper ChannelListGroup ChannelSelectMatch
                       }

emptyChannelSelectState :: ChannelSelectState
emptyChannelSelectState =
    ChannelSelectState { _channelSelectInput = editor ChannelSelectInput (Just 1) ""
                       , _channelSelectMatches = fromList []
                       }

-- | The state of message selection mode.
data MessageSelectState =
    MessageSelectState { selectMessageId :: Maybe MessageId
                       }

-- | The state of the post list overlay.
data PostListOverlayState =
    PostListOverlayState { _postListPosts    :: Messages
                         , _postListSelected :: Maybe PostId
                         }

-- | The state of the search result list overlay. Type 'a' is the type
-- of data in the list. Type 'b' is the search scope type.
data ListOverlayState a b =
    ListOverlayState { _listOverlaySearchResults :: List Name a
                     -- ^ The list of search results currently shown in
                     -- the overlay.
                     , _listOverlaySearchInput :: Editor Text Name
                     -- ^ The editor for the overlay's search input.
                     , _listOverlaySearchScope :: b
                     -- ^ The overlay's current search scope.
                     , _listOverlaySearching :: Bool
                     -- ^ Whether a search is in progress (i.e. whether
                     -- we are currently awaiting a response from a
                     -- search query to the server).
                     , _listOverlayEnterHandler :: a -> MH Bool
                     -- ^ The handler to invoke on the selected element
                     -- when the user presses Enter.
                     , _listOverlayNewList :: Vec.Vector a -> List Name a
                     -- ^ The function to build a new brick List from a
                     -- vector of search results.
                     , _listOverlayFetchResults :: b -> Session -> Text -> IO (Vec.Vector a)
                     -- ^ The function to call to issue a search query
                     -- to the server.
                     }

-- | The scope for searching for users in a user list overlay.
data UserSearchScope =
    ChannelMembers ChannelId TeamId
    | ChannelNonMembers ChannelId TeamId
    | AllUsers (Maybe TeamId)

-- | The scope for searching for channels to join.
data ChannelSearchScope =
    AllChannels

-- | Actions that can be sent on the websocket to the server.
data WebsocketAction =
    UserTyping UTCTime ChannelId (Maybe PostId) -- ^ user typing in the input box
    deriving (Read, Show, Eq, Ord)

-- * MH Monad

-- | Logging context information, in the event that metadata should
-- accompany a log message.
data LogContext =
    LogContext { logContextChannelId :: Maybe ChannelId
               }
               deriving (Eq, Show)

-- | A user fetching strategy.
data UserFetch =
    UserFetchById UserId
    -- ^ Fetch the user with the specified ID.
    | UserFetchByUsername Text
    -- ^ Fetch the user with the specified username.
    | UserFetchByNickname Text
    -- ^ Fetch the user with the specified nickname.
    deriving (Eq, Show)

data MHState =
    MHState { mhCurrentState :: ChatState
            , mhNextAction :: ChatState -> EventM Name (Next ChatState)
            , mhUsersToFetch :: [UserFetch]
            , mhPendingStatusZipper :: Maybe (Zipper ChannelListGroup ChannelListEntry)
            }

-- | A value of type 'MH' @a@ represents a computation that can
-- manipulate the application state and also request that the
-- application quit
newtype MH a =
    MH { fromMH :: R.ReaderT (Maybe LogContext) (St.StateT MHState (EventM Name)) a }

-- | Use a modified logging context for the duration of the specified MH
-- action.
withLogContext :: (Maybe LogContext -> Maybe LogContext) -> MH a -> MH a
withLogContext modifyContext act =
    MH $ R.withReaderT modifyContext (fromMH act)

withLogContextChannelId :: ChannelId -> MH a -> MH a
withLogContextChannelId cId act =
    let f Nothing = Just $ LogContext (Just cId)
        f (Just c) = Just $ c { logContextChannelId = Just cId }
    in withLogContext f act

-- | Get the current logging context.
getLogContext :: MH (Maybe LogContext)
getLogContext = MH R.ask

-- | Log a message.
mhLog :: LogCategory -> Text -> MH ()
mhLog cat msg = do
    logger <- mhGetIOLogger
    liftIO $ logger cat msg

-- | Get a logger suitable for use in IO. The logger always logs using
-- the MH monad log context at the time of the call to mhGetIOLogger.
mhGetIOLogger :: MH (LogCategory -> Text -> IO ())
mhGetIOLogger = do
    ctx <- getLogContext
    mgr <- use (to (_crLogManager . _csResources))
    return $ ioLogWithManager mgr ctx

ioLogWithManager :: LogManager -> Maybe LogContext -> LogCategory -> Text -> IO ()
ioLogWithManager mgr ctx cat msg = do
    now <- getCurrentTime
    let lm = LogMessage { logMessageText = msg
                        , logMessageContext = ctx
                        , logMessageCategory = cat
                        , logMessageTimestamp = now
                        }
    sendLogMessage mgr lm

-- | Run an 'MM' computation, choosing whether to continue or halt based
-- on the resulting
runMHEvent :: ChatState -> MH () -> EventM Name (Next ChatState)
runMHEvent st (MH mote) = do
  let mhSt = MHState { mhCurrentState = st
                     , mhNextAction = Brick.continue
                     , mhUsersToFetch = []
                     , mhPendingStatusZipper = Nothing
                     }
  ((), st') <- St.runStateT (R.runReaderT mote Nothing) mhSt
  (mhNextAction st') (mhCurrentState st')

scheduleUserFetches :: [UserFetch] -> MH ()
scheduleUserFetches fs = MH $ do
    St.modify $ \s -> s { mhUsersToFetch = fs <> mhUsersToFetch s }

scheduleUserStatusFetches :: Zipper ChannelListGroup ChannelListEntry -> MH ()
scheduleUserStatusFetches z = MH $ do
    St.modify $ \s -> s { mhPendingStatusZipper = Just z }

getScheduledUserFetches :: MH [UserFetch]
getScheduledUserFetches = MH $ St.gets mhUsersToFetch

getScheduledUserStatusFetches :: MH (Maybe (Zipper ChannelListGroup ChannelListEntry))
getScheduledUserStatusFetches = MH $ St.gets mhPendingStatusZipper

-- | lift a computation in 'EventM' into 'MH'
mh :: EventM Name a -> MH a
mh = MH . R.lift . St.lift

generateUUID :: MH UUID
generateUUID = liftIO generateUUID_IO

generateUUID_IO :: IO UUID
generateUUID_IO = randomIO

mhHandleEventLensed :: Lens' ChatState b -> (e -> b -> EventM Name b) -> e -> MH ()
mhHandleEventLensed ln f event = MH $ do
    s <- St.get
    let st = mhCurrentState s
    n <- R.lift $ St.lift $ f event (st ^. ln)
    St.put (s { mhCurrentState = st & ln .~ n })

mhSuspendAndResume :: (ChatState -> IO ChatState) -> MH ()
mhSuspendAndResume mote = MH $ do
    s <- St.get
    St.put $ s { mhNextAction = \ _ -> Brick.suspendAndResume (mote $ mhCurrentState s) }

-- | This will request that after this computation finishes the
-- application should exit
requestQuit :: MH ()
requestQuit = MH $ do
    s <- St.get
    St.put $ s { mhNextAction = Brick.halt }

instance Functor MH where
    fmap f (MH x) = MH (fmap f x)

instance Applicative MH where
    pure x = MH (pure x)
    MH f <*> MH x = MH (f <*> x)

instance Monad MH where
    return x = MH (return x)
    MH x >>= f = MH (x >>= \ x' -> fromMH (f x'))

-- We want to pretend that the state is only the ChatState, rather
-- than the ChatState and the Brick continuation
instance St.MonadState ChatState MH where
    get = mhCurrentState `fmap` MH St.get
    put st = MH $ do
        s <- St.get
        St.put $ s { mhCurrentState = st }

instance St.MonadIO MH where
    liftIO = MH . St.liftIO

-- | This represents events that we handle in the main application loop.
data MHEvent =
    WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
    | RespEvent (MH ())
    -- ^ For the result values of async IO operations
    | RefreshWebsocketEvent
    -- ^ Tell our main loop to refresh the websocket connection
    | WebsocketParseError String
    -- ^ We failed to parse an incoming websocket event
    | WebsocketDisconnect
    -- ^ The websocket connection went down.
    | WebsocketConnect
    -- ^ The websocket connection came up.
    | BGIdle
    -- ^ background worker is idle
    | BGBusy (Maybe Int)
    -- ^ background worker is busy (with n requests)
    | IEvent InternalEvent
    -- ^ MH-internal events

-- | Internal application events.
data InternalEvent =
    DisplayError MHError
    -- ^ Some kind of application error occurred
    | LoggingStarted FilePath
    | LoggingStopped FilePath
    | LogStartFailed FilePath String
    | LogDestination (Maybe FilePath)
    | LogSnapshotSucceeded FilePath
    | LogSnapshotFailed FilePath String
    -- ^ Logging events from the logging thread

-- | Application errors.
data MHError =
    GenericError T.Text
    -- ^ A generic error message constructor
    | NoSuchChannel T.Text
    -- ^ The specified channel does not exist
    | NoSuchUser T.Text
    -- ^ The specified user does not exist
    | AmbiguousName T.Text
    -- ^ The specified name matches both a user and a channel
    | ServerError MattermostError
    -- ^ A Mattermost server error occurred
    | ClipboardError T.Text
    -- ^ A problem occurred trying to deal with yanking or the system
    -- clipboard
    | ConfigOptionMissing T.Text
    -- ^ A missing config option is required to perform an operation
    | ProgramExecutionFailed T.Text T.Text
    -- ^ Args: program name, path to log file. A problem occurred when
    -- running the program.
    | NoSuchScript T.Text
    -- ^ The specified script was not found
    | NoSuchHelpTopic T.Text
    -- ^ The specified help topic was not found
    | AsyncErrEvent SomeException
    -- ^ For errors that arise in the course of async IO operations
    deriving (Show)

-- ** Application State Lenses

makeLenses ''ChatResources
makeLenses ''ChatState
makeLenses ''ChatEditState
makeLenses ''AutocompleteState
makeLenses ''PostListOverlayState
makeLenses ''ListOverlayState
makeLenses ''ChannelSelectState
makeLenses ''UserPreferences
makeLenses ''ConnectionInfo

getSession :: MH Session
getSession = use (csResources.crSession)

getResourceSession :: ChatResources -> Session
getResourceSession = _crSession

whenMode :: Mode -> MH () -> MH ()
whenMode m act = do
    curMode <- use csMode
    when (curMode == m) act

setMode :: Mode -> MH ()
setMode m = do
    csMode .= m
    mh invalidateCache

setMode' :: Mode -> ChatState -> ChatState
setMode' m = csMode .~ m

appMode :: ChatState -> Mode
appMode = _csMode

resetSpellCheckTimer :: ChatEditState -> IO ()
resetSpellCheckTimer s =
    case s^.cedSpellChecker of
        Nothing -> return ()
        Just (_, reset) -> reset

-- ** Utility Lenses
csCurrentChannelId :: SimpleGetter ChatState ChannelId
csCurrentChannelId = csFocus.to unsafeFocus.to channelListEntryChannelId

channelListEntryChannelId :: ChannelListEntry -> ChannelId
channelListEntryChannelId (CLChannel cId) = cId
channelListEntryChannelId (CLUserDM cId _) = cId
channelListEntryChannelId (CLGroupDM cId) = cId

channelListEntryUserId :: ChannelListEntry -> Maybe UserId
channelListEntryUserId (CLUserDM _ uId) = Just uId
channelListEntryUserId _ = Nothing

userIdsFromZipper :: Zipper ChannelListGroup ChannelListEntry -> [UserId]
userIdsFromZipper z =
    concat $ (catMaybes . fmap channelListEntryUserId . snd) <$> Zipper.toList z

entryIsDMEntry :: ChannelListEntry -> Bool
entryIsDMEntry (CLUserDM {}) = True
entryIsDMEntry (CLGroupDM {}) = True
entryIsDMEntry (CLChannel {}) = False

csCurrentChannel :: Lens' ChatState ClientChannel
csCurrentChannel =
    lens (\ st -> findChannelById (st^.csCurrentChannelId) (st^.csChannels) ^?! _Just)
         (\ st n -> st & csChannels %~ addChannel (st^.csCurrentChannelId) n)

csChannel :: ChannelId -> Traversal' ChatState ClientChannel
csChannel cId =
    csChannels . channelByIdL cId

withChannel :: ChannelId -> (ClientChannel -> MH ()) -> MH ()
withChannel cId = withChannelOrDefault cId ()

withChannelOrDefault :: ChannelId -> a -> (ClientChannel -> MH a) -> MH a
withChannelOrDefault cId deflt mote = do
    chan <- preuse (csChannel(cId))
    case chan of
        Nothing -> return deflt
        Just c  -> mote c

-- ** 'ChatState' Helper Functions

raiseInternalEvent :: InternalEvent -> MH ()
raiseInternalEvent ev = do
    queue <- use (csResources.crEventQueue)
    St.liftIO $ writeBChan queue (IEvent ev)

-- | Log and raise an error.
mhError :: MHError -> MH ()
mhError err = do
    mhLog LogError $ T.pack $ show err
    raiseInternalEvent (DisplayError err)

isMine :: ChatState -> Message -> Bool
isMine st msg =
    case msg^.mUser of
        UserI _ uid -> uid == myUserId st
        _ -> False

getMessageForPostId :: ChatState -> PostId -> Maybe Message
getMessageForPostId st pId = st^.csPostMap.at(pId)

getParentMessage :: ChatState -> Message -> Maybe Message
getParentMessage st msg
    | InReplyTo pId <- msg^.mInReplyToMsg
      = st^.csPostMap.at(pId)
    | otherwise = Nothing

setUserStatus :: UserId -> Text -> MH ()
setUserStatus uId t = do
    csUsers %= modifyUserById uId (uiStatus .~ statusFromText t)
    mh $ invalidateCacheEntry ChannelSidebar

usernameForUserId :: UserId -> ChatState -> Maybe Text
usernameForUserId uId st = _uiName <$> findUserById uId (st^.csUsers)

displayNameForUserId :: UserId -> ChatState -> Maybe Text
displayNameForUserId uId st = do
    u <- findUserById uId (st^.csUsers)
    return $ displayNameForUser u (st^.csClientConfig) (st^.csResources.crUserPreferences)

-- | Note: this only searches users we have already loaded. Be
-- aware that if you think you need a user we haven't fetched, use
-- withFetchedUser!
userIdForUsername :: Text -> ChatState -> Maybe UserId
userIdForUsername name st =
    fst <$> (findUserByUsername name $ st^.csUsers)

channelIdByChannelName :: Text -> ChatState -> Maybe ChannelId
channelIdByChannelName name st =
    let matches (_, cc) = cc^.ccInfo.cdName == (trimChannelSigil name)
    in listToMaybe $ fst <$> filteredChannels matches (st^.csChannels)

channelIdByUsername :: Text -> ChatState -> Maybe ChannelId
channelIdByUsername name st = do
    uId <- userIdForUsername name st
    getDmChannelFor uId (st^.csChannels)

useNickname :: ChatState -> Bool
useNickname st =
    useNickname' (st^.csClientConfig) (st^.csResources.crUserPreferences)

channelByName :: Text -> ChatState -> Maybe ClientChannel
channelByName n st = do
    cId <- channelIdByChannelName n st
    findChannelById cId (st^.csChannels)

trimChannelSigil :: Text -> Text
trimChannelSigil n
    | normalChannelSigil `T.isPrefixOf` n = T.tail n
    | otherwise = n

addNewUser :: UserInfo -> MH ()
addNewUser u = do
    csUsers %= addUser u
    -- Invalidate the cache because channel message rendering may need
    -- to get updated if this user authored posts in any channels.
    mh invalidateCache

data SidebarUpdate =
    SidebarUpdateImmediate
    | SidebarUpdateDeferred
    deriving (Eq, Show)


resetAutocomplete :: MH ()
resetAutocomplete = do
    csEditState.cedAutocomplete .= Nothing
    csEditState.cedAutocompletePending .= Nothing


-- * Slash Commands

-- | The 'CmdArgs' type represents the arguments to a slash-command; the
-- type parameter represents the argument structure.
data CmdArgs :: * -> * where
    NoArg    :: CmdArgs ()
    LineArg  :: Text -> CmdArgs Text
    UserArg  :: CmdArgs rest -> CmdArgs (Text, rest)
    ChannelArg :: CmdArgs rest -> CmdArgs (Text, rest)
    TokenArg :: Text -> CmdArgs rest -> CmdArgs (Text, rest)

-- | A 'CmdExec' value represents the implementation of a command when
-- provided with its arguments
type CmdExec a = a -> MH ()

-- | A 'Cmd' packages up a 'CmdArgs' specifier and the 'CmdExec'
-- implementation with a name and a description.
data Cmd =
    forall a. Cmd { cmdName    :: Text
                  , cmdDescr   :: Text
                  , cmdArgSpec :: CmdArgs a
                  , cmdAction  :: CmdExec a
                  }

-- | Helper function to extract the name out of a 'Cmd' value
commandName :: Cmd -> Text
commandName (Cmd name _ _ _ ) = name

-- *  Channel Updates and Notifications

userList :: ChatState -> [UserInfo]
userList st = filter showUser $ allUsers (st^.csUsers)
    where showUser u = not (isSelf u) && (u^.uiInTeam)
          isSelf u = (myUserId st) == (u^.uiId)

allUserIds :: ChatState -> [UserId]
allUserIds st = getAllUserIds $ st^.csUsers

-- BEWARE: you probably don't want this, but instead
-- State.Users.withFetchedUser, since this only looks up users in the
-- collection we have already loaded rather than all valid users on the
-- server.
userById :: UserId -> ChatState -> Maybe UserInfo
userById uId st = findUserById uId (st^.csUsers)

myUserId :: ChatState -> UserId
myUserId st = myUser st ^. userIdL

myTeamId :: ChatState -> TeamId
myTeamId st = st ^. csMyTeam . to unsafeFocus . teamIdL

myTeamNames :: ChatState -> [Text]
myTeamNames st = st ^. csMyTeam . to (map (unsafeUserText . teamName) . concat . map snd . Zipper.toList)

myUser :: ChatState -> User
myUser st = st^.csMe

-- BEWARE: you probably don't want this, but instead
-- State.Users.withFetchedUser, since this only looks up users in the
-- collection we have already loaded rather than all valid users on the
-- server.
userByUsername :: Text -> ChatState -> Maybe UserInfo
userByUsername name st = do
    snd <$> (findUserByUsername name $ st^.csUsers)

-- BEWARE: you probably don't want this, but instead
-- State.Users.withFetchedUser, since this only looks up users in the
-- collection we have already loaded rather than all valid users on the
-- server.
userByNickname :: Text -> ChatState -> Maybe UserInfo
userByNickname name st =
    snd <$> (findUserByNickname name $ st^.csUsers)

getUsers :: MH Users
getUsers = use csUsers

-- * HighlightSet

type UserSet = Set Text
type ChannelSet = Set Text

-- | The set of usernames, channel names, and language names used for
-- highlighting when rendering messages.
data HighlightSet =
    HighlightSet { hUserSet    :: Set Text
                 , hChannelSet :: Set Text
                 , hSyntaxMap  :: SyntaxMap
                 }

getHighlightSet :: ChatState -> HighlightSet
getHighlightSet st =
    HighlightSet { hUserSet = addSpecialUserMentions $ getUsernameSet $ st^.csUsers
                 , hChannelSet = getChannelNameSet $ st^.csChannels
                 , hSyntaxMap = st^.csResources.crSyntaxMap
                 }

-- From: https://docs.mattermost.com/help/messaging/mentioning-teammates.html
specialUserMentions :: [T.Text]
specialUserMentions = ["all", "channel", "here"]

addSpecialUserMentions :: Set Text -> Set Text
addSpecialUserMentions s = foldr Set.insert s specialUserMentions

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe NewMessageIndicator
getNewMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    return $ cc^.ccInfo.cdNewMessageIndicator

getEditedMessageCutoff :: ChannelId -> ChatState -> Maybe ServerTime
getEditedMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    cc^.ccInfo.cdEditedMessageThreshold

clearChannelUnreadStatus :: ChannelId -> MH ()
clearChannelUnreadStatus cId = do
    mh $ invalidateCacheEntry (ChannelMessages cId)
    csChannel(cId) %= (clearNewMessageIndicator .
                       clearEditedThreshold)

data OpenInBrowser =
    OpenLinkChoice LinkChoice
    | OpenLocalFile FilePath
    deriving (Eq, Show)
