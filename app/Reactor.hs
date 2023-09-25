{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

{- |
This is an example language server built with haskell-lsp using a 'Reactor'
design. With a 'Reactor' all requests are handled on a /single thread/.
A thread is spun up for it, which repeatedly reads from a 'TChan' of
'ReactorInput's.
The `lsp` handlers then simply pass on all the requests and
notifications onto the channel via 'ReactorInput's.
This way there is the option of executing requests on multiple threads, without
blocking server communication.

To try out this server, install it with
> cabal install lsp-demo-reactor-server -fdemo
and plug it into your client of choice.
-}
module Reactor where
import           Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import qualified Colog.Core as L
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                     as E
import qualified GHC.IO.Exception                      as EG
import           Control.Lens hiding (Iso)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Aeson                            as J
import qualified Data.Aeson.KeyMap                     as H
import qualified Data.Text                             as T
import           Prettyprinter ( Pretty(pretty) )
import           GHC.Generics (Generic)
import           Language.LSP.Server
-- import qualified Language.LSP.Server.Core      as LSPCore
import           Language.LSP.Diagnostics
import           Language.LSP.Logging (defaultClientLogger)
import qualified Language.LSP.Types            as J
import qualified Language.LSP.Types.Lens       as J
import qualified Language.LSP.Types.Lens       as JL
import           Language.LSP.VFS
import           System.Exit
import qualified System.Process as Process
import System.IO
    ( stdin, stderr, stdout, hPrint )
-- import System.Log.Logger
--     ( errorM, debugM, removeAllHandlers, Priority(DEBUG), warningM )
import           Control.Concurrent
import           System.Directory (createDirectoryIfMissing, canonicalizePath)

import qualified GF
import qualified GF.Infra.Option as GF

import GFExtras

import System.Environment (withArgs, setEnv, getEnv)
import qualified GF.Support as GF
-- import qualified GF.Compile as S
-- import GF.Compiler (linkGrammars)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
-- import Data.Time (UTCTime)
import GHC.IO.Handle
import Data.List.Split
import Text.ParserCombinators.ReadP
import Control.Arrow (first)
import qualified Data.List as List
import Data.Function (on)
import Data.Ord (comparing)
import Control.Applicative ((<|>))
import qualified PGF
import System.FilePath (takeBaseName, takeDirectory)
import Control.Exception (SomeException(SomeException))
import Data.Typeable (typeRep, typeOf)
import qualified Data.Map as Map
import GFTags (Tags, Tag (..))
import qualified System.IO.Error as E
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import Data.Foldable (Foldable(toList))
import qualified System.FilePath as FP
-- import Debug.Trace (traceM, traceShowId)


traceM :: Applicative f => p -> f ()
traceM _ = pure ()
traceShowId :: a -> a
traceShowId = id

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Redundant lambda"   :: String) #-}
-- ---------------------------------------------------------------------
--

-- TODO: Add flag for for version number
-- And for debugging


-- | The directory to put .gfo files
outputDir :: String
outputDir = ".gf-lsp"

-- DONE: Generate type lenses for functions
-- DONE: Show concrete types on hover
-- TODO: Write tests
-- WONTFIX: Figure out why compilation of this is slow
-- TODO: Allow going to definition of modules
-- TODO: Catch all errors in handlers
-- DONE: Make GF_LIB_PATH a config option
--       Or maybe figure it out using the gf executable
-- TODO: Handle warnings in case of errors
-- TODO: Don't reverse errors
-- TODO: Handle OCCURED_IN which is unindented
-- TODO: Handle "conflict between" "and" which is unindented
-- TODO: Show the location in the hover
-- TODO: Show hover types/definitions for types as well
-- TODO: Handle cancellation
-- TODO: Add timeout and multithreading
-- TODO: Make proper releases with tags
-- TODO: Check for issues with workspaces
-- TODO: Make output less spammy

-- TODO: Tab completion, both for symbols and modules

-- TODO: Detect dependencies and automatically recompile dependent modules on change

-- ---------------------------------------------------------------------

data LspContext = LspContext { compileEnv :: TVar CompileEnv , config :: Config }
data Config = Config { fooTheBar :: Maybe Bool, wibbleFactor :: Maybe Int, gfLibPath :: Maybe [FilePath] }
  deriving (Generic, J.ToJSON, J.FromJSON, Show)

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  cEnv <- newTVarIO emptyCompileEnv :: IO (TVar CompileEnv)
  -- LC.withBackgroundLogger

  -- Duplicate the stdout and stderr handle so we can capture them without getting a mixup
  -- This can still cause confusion if we call GF twice at the same time
  realStdout <- hDuplicate stdout
  realStderr <- hDuplicate stderr

  let
    -- Three loggers:
    -- 1. To stderr
    -- 2. To the client (filtered by severity)
    -- 3. To both
    prettyMsg l = "[" <> show (L.getSeverity l) <> "] " <> T.unpack (L.getMsg l)
    stderrLogger :: LogAction IO (WithSeverity T.Text)
    stderrLogger = L.cmap prettyMsg $ L.logStringHandle realStderr
    clientLogger :: LogAction (LspM LspContext) (WithSeverity T.Text)
    clientLogger = defaultClientLogger
    dualLogger :: LogAction (LspM LspContext) (WithSeverity T.Text)
    -- dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger
    dualLogger = clientLogger

    serverDefinition = ServerDefinition
      { defaultConfig = LspContext { compileEnv = cEnv, config = Config
            { fooTheBar = Just False
            , wibbleFactor = Just 0
            , gfLibPath = Nothing }}
      , onConfigurationChange = \old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right $ old {config = cfg }
      -- , doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env)
      -- , staticHandlers = lspHandlers rin
      , doInitialize = \env _ -> do
          _ <- forkIO (reactor stderrLogger rin)
          -- env' <- guessLibpath env
          pure (Right env)
      -- Handlers log to both the client and stderr
      , staticHandlers = lspHandlers dualLogger rin
      -- , staticHandlers = lspHandlers clientLogger rin
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = lspOptions
      }

  -- flip E.finally finalProc $ do
  --   -- setupLogger Nothing ["reactor"] DEBUG
  --   runServer serverDefinition
  let
    logToText = T.pack . show . pretty
  runServerWithHandles
      -- Log to both the client and stderr when we can, stderr beforehand
    (L.cmap (fmap logToText) stderrLogger)
    (L.cmap (fmap logToText) dualLogger)
    stdin
    realStdout
    serverDefinition

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- guessLibpath :: LanguageContextEnv LspContext -> IO (LanguageContextEnv LspContext)
-- guessLibpath = _
guessLibpath :: L.LogAction (LspM LspContext) (WithSeverity T.Text) -> LspT LspContext IO ()
guessLibpath logger = do
  conf <- getConfig
  let libPathSetting = gfLibPath $ config conf
  case libPathSetting of
    Just _ -> pure () -- Assume valid setting
    Nothing -> do
      libPathEnv <- liftIO $ E.try @E.IOException $ getEnv "GF_LIB_PATH"
      case libPathEnv of
        Right _ -> pure () -- Assume valid setting
        Left err | not $ E.isDoesNotExistError err -> do
          errorM logger "guessLibPath" $ "Got unexpected exception: " ++ show err
        Left e -> do
          debugM logger "guessLibPath" $ "Got expected exception: " ++ show e
          warningM logger "guessLibPath" "No GF_LIB_PATH found, trying to call gf to get value"
          let nonExistingFile = "non-existing-file-name.gf"
          -- No lib path set, try guessing using the gf in path
          response <- liftIO $ E.try @E.IOException $ Process.readProcessWithExitCode "gf" ["-make", nonExistingFile] ""
          -- (exitCode, out, err)
          debugM logger "guessLibPath" $ "Got response: " ++ show response
          case response of
            Left (EG.IOError _mh EG.NoSuchThing _loc _descr _errno _fnm) ->
              errorM logger "guessLibPath" $ unlines
                [ "No GF installation found. Couldn't guess GF_LIB_PATH."
                , "Either ensure that gf is installed and in PATH or manually set GF_LIB_PATH."
                , ""
                , "You can still use the language server, but you won't have RGL available"
                ]
            Left err@(EG.IOError mh tp loc descr errno fileNm) -> do
              debugM logger "guessLibPath" $ "Error of type " ++ show (mh, tp, loc, descr, errno, fileNm)
              errorM logger "guessLibPath" $ "Unexpected error when running gf: " ++ show err
              -- error of type (Nothing,does not exist,"readCreateProcessWithExitCode: posix_spawnp","No such file or directory",Just 2,Just "gf")
            -- Left (SomeException err) -> debugM logger "guessLibPath" $ "Error of type " ++ show (typeOf err)
            Right (ExitFailure 1, "", err)
              | ("":msg: pfxFilePaths) <- lines err
              , msg `elem` ["Unable to find: ","None of these files exist:"]
              , Just filepaths <- mapM (List.stripPrefix "  ") pfxFilePaths -> do
              -- TODO: Use --version (only works for newer versions)
              -- TODO: Handle majestic somehow (e.g. by compiling a separate copy of RGL)
              debugM logger "guessLibPath" $ "GF successfully ran with: " ++ show filepaths
              let newLibPath = takeDirectory <$> filepaths
              debugM logger "guessLibPath" $ "Got directory: " ++ show newLibPath
              modifyConfig $ \ctx -> ctx {config = (config ctx){gfLibPath = Just newLibPath}}
            Right (exitCode, out, err) -> do
              errorM logger "guessLibPath" $ unlines
                [ "Unexpected response from GF: " ++ show exitCode
                , ""
                , "Stdout: " ++ out
                , ""
                , "Stderr: " ++ err
                ]

          pure ()

-- | Not atomic, since what's needed is not exported from LSP
modifyConfig :: MonadLsp config m => (config -> config) -> m ()
modifyConfig f = do
  conf <- getConfig
  setConfig $ f conf
-- modifyConfig config = LSPCore.stateState resConfig (const ((), config)) -- Not exported
-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  , executeCommandCommands = Just ["lsp-hello-command"]
  }

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

data ReactorInput
  = ReactorAction (IO ())
  | Diagnostics


-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: L.LogAction IO (WithSeverity T.Text) -> TChan ReactorInput -> IO ()
reactor logger inp = do
  logger <& "Started the reactor" `WithSeverity` Info
  forever $ do
    input <- atomically $ readTChan inp
    -- debugM logger "reactor" "Got request"
    case input of
      ReactorAction act -> act
      Diagnostics -> do
        withArgs
          [ "-make",
            "--output-dir=generated",
            "--gfo-dir=generated",
            -- "-v=0",
            "Foo.gf"
          ]
          GF.main
    -- debugM logger "reactor" "Completed request"

-- TODO: Run type checker over hover
-- TODO: Package for mac
-- TODO: Find locations (Maybe through tags or some other solution)

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: L.LogAction (LspM LspContext) (WithSeverity T.Text) -> TChan ReactorInput -> Handlers (LspM LspContext)
lspHandlers logger rin = mapHandlers goReq goNot (handle logger)
  where
    goReq :: forall (a :: J.Method J.FromClient J.Request). Handler (LspM LspContext) a -> Handler (LspM LspContext) a
    goReq f = \msg k -> do
      env <- getLspEnv
      let handleErrs = do
            E.catch @SomeException (runLspT env $ f msg k) $ \e ->
              runLspT env $ k $ Left $ J.ResponseError J.InternalError (T.pack $ show e) Nothing
      liftIO $ atomically $ writeTChan rin $ ReactorAction handleErrs

    goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler (LspM LspContext) a -> Handler (LspM LspContext) a
    goNot f = \msg -> do
      env <- getLspEnv
      let handleErrs = do
            E.catch @SomeException (runLspT env $ f msg) $ \(SomeException inner) -> runLspT env $ do
              warningM logger "Erro" $ "Got error of type " ++ show (typeOf inner)
              errorM logger "Error" $ show (E.displayException inner)
      liftIO $ atomically $ writeTChan rin $ ReactorAction handleErrs

-- | Where the actual logic resides for handling requests and notifications.
handle ::  L.LogAction (LspM LspContext) (WithSeverity T.Text) -> Handlers (LspM LspContext)
handle logger = mconcat
  [ notificationHandler J.SInitialized $ \_msg -> do
      debugM logger "reactor.handle" "Processing the Initialized notification"
      guessLibpath logger
      -- foo <- registerCapability J.STextDocumentDefinition (J.DefinitionRegistrationOptions Nothing Nothing) $ \req responder -> do
      --   -- let J.DefinitionParams {J._textDocument = doc, J._position = pos} = req
      --   -- let doc = req ^. J.params . J.textDocument
      --   -- let pos = req ^. J.params . J.position
      --   debugM logger "def" $ "Got request: " ++ show req
      --   responder $ Right $ J.InR $ J.InL $ J.List []
      pure ()

      -- -- We're initialized! Lets send a showMessageRequest now
      -- let params = J.ShowMessageRequestParams
      --                    J.MtWarning
      --                    "What's your favourite language extension?"
      --                    (Just [J.MessageActionItem "Rank2Types", J.MessageActionItem "NPlusKPatterns"])

      -- void $ sendRequest J.SWindowShowMessageRequest params $ \case
      --     Left e -> liftIO $ errorM "reactor.handle" $ "Got an error: " ++ show e
      --     Right _ -> do
      --       sendNotification J.SWindowShowMessage (J.ShowMessageParams J.MtInfo "Excellent choice")

      --       -- We can dynamically register a capability once the user accepts it
      --       sendNotification J.SWindowShowMessage (J.ShowMessageParams J.MtInfo "Turning on code lenses dynamically")

      --       let regOpts = J.CodeLensRegistrationOptions Nothing Nothing (Just False)

      --       void $ registerCapability J.STextDocumentCodeLens regOpts $ \_req responder -> do
      --         debugM logger "reactor.handle" "Processing a textDocument/codeLens request"
      --         let cmd = J.Command "Say hello" "lsp-hello-command" Nothing
      --             rsp = J.List [J.CodeLens (J.mkRange 0 0 0 100) (Just cmd) Nothing]
      --         responder (Right rsp)

  , notificationHandler J.SCancelRequest $ \msg -> do
      -- TODO: Actually try to cancel stuff
      case msg ^. J.params of
        (J.CancelParams reqId) -> debugM logger "reactor.handle" $ "Got cancel request for: " ++ show reqId

  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    debugM logger "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
    -- sendDiagnostics "Example message" (J.toNormalizedUri doc) (Just 0)
    callGF logger doc fileName
  , notificationHandler J.STextDocumentDidClose $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    debugM logger "reactor.handle" $ "Processing DidCloseTextDocument for: " ++ show fileName

  , notificationHandler J.SWorkspaceDidChangeConfiguration $ \msg -> do
      cfg <- config <$> getConfig
      debugM logger "configuration changed: " (show (msg,cfg))
      sendNotification J.SWindowShowMessage $
        J.ShowMessageParams J.MtInfo $ "Wibble factor set to " <> T.pack (show (wibbleFactor cfg))

  , notificationHandler J.STextDocumentDidChange $ \msg -> do
    let doc  = msg ^. J.params
                    . J.textDocument
                    . J.uri
                    . to J.toNormalizedUri
    debugM logger "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
    mdoc <- getVirtualFile doc
    case mdoc of
      Just (VirtualFile _version fileVersion _) -> do
        debugM logger "reactor.handle" $ "Found the virtual file: " ++ show fileVersion
      Nothing -> do
        debugM logger "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc

  , notificationHandler J.STextDocumentDidSave $ \msg -> do
      let doc = msg ^. J.params . J.textDocument . J.uri
          fileName = J.uriToFilePath doc
      debugM logger "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
      callGF logger doc fileName
      -- sendDiagnostics "Example message" (J.toNormalizedUri doc) Nothing

  , requestHandler J.STextDocumentRename $ \req responder -> do
      debugM logger "reactor.handle" "Processing a textDocument/rename request"
      let params = req ^. J.params
          J.Position l c = params ^. J.position
          newName = params ^. J.newName
      vdoc <- getVersionedTextDoc (params ^. J.textDocument)
      -- Replace some text at the position with what the user entered
      let edit = J.InL $ J.TextEdit (J.mkRange l c l (c + fromIntegral (T.length newName))) newName
          tde = J.TextDocumentEdit vdoc (J.List [edit])
          -- "documentChanges" field is preferred over "changes"
          rsp = J.WorkspaceEdit Nothing (Just (J.List [J.InL tde])) Nothing
      responder (Right rsp)

  , requestHandler J.STextDocumentHover $ \req responder -> do
      -- debugM logger "reactor.handle" "Processing a textDocument/hover request"
      let J.HoverParams docI pos _workDone = req ^. J.params
          doc = docI ^. J.uri
          fileName = J.uriToFilePath doc
      hovStr <- getHoverString logger pos doc
      case hovStr of
        Just (fullWord, range) -> do
          -- debugM logger "reactor.handle" $ "Guessing range: " ++ show range
          (tags, gr, modEnv) <- getCompileEnv
          let opts = GF.modifyFlags $ \flags -> flags
                { GF.optOutputDir = Just outputDir
                , GF.optGFODir = Just outputDir
                , GF.optPMCFG = False
                , GF.optVerbosity = GF.Quiet
                -- , GF.optStopAfterPhase = Linker -- Default Compile
                }
          case takeBaseName <$> fileName of
            Nothing -> do
              -- TODO: Clean this nesting up
              debugM logger "reactor.handle" $ "Didn't find filename for: " ++ show doc
              responder (Right Nothing)
            Just modName -> do
              debugM logger "hover.handle" $ "For file named: " ++ show modName
              let showModuleName (GF.MN x) = GF.showIdent x
              debugM logger "hover.handle" $ "Modules available: " ++ show (showModuleName . fst <$> GF.modules gr)
              -- GF.Compile.link converts gr to pgf
              case PGF.readExpr $ T.unpack fullWord of
                Nothing -> do
                  debugM logger "hover.handle" $ "Invalid expression: " ++ show fullWord
                  responder (Right Nothing)
                Just expr -> do
                  debugM logger "hover.handle" $ "For file named: " ++ show modName
                  let absName = GF.srcAbsName gr (GF.moduleNameS modName)
                  mtag <- findTagsForIdentDeep logger (GF.moduleNameS modName) (GF.identS $ T.unpack fullWord) tags
                  matag <- findTagsForIdentDeep logger absName (GF.identS $ T.unpack fullWord) tags
                  forM_ mtag $ \tag -> debugM logger "hover.handle" $ "Found tags: " ++ show tag
                  forM_ matag $ \tag -> debugM logger "hover.handle" $ "Found abstract tags: " ++ show tag
                  let typeTags = [ (typ, tag) | tag@LocalTag {gfTypeOf=Just typ} <- matag ++ mtag ]
                  case typeTags of
                    [] -> do
                      warningM logger "reactor.handle" "Failed to find tag"
                      -- Warning already handled
                      responder (Right Nothing)
                    _ -> do
                      debugM logger "reactor.handle" "Found tags:"
                      forM_ (map snd typeTags) $ \tag -> debugM logger "hover.handle" $ "tags: " ++ show tag
                      let message = List.nub [ PGF.showExpr [] expr ++ " : " ++ typ | (typ, _) <- typeTags ]
                      let ms = J.HoverContents $ J.markedUpContent "lsp-hello" $ T.pack $ unlines message
                          rsp = J.Hover ms (Just range)
                      responder $ Right $ Just rsp
        Nothing -> do
          debugM logger "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc
          responder (Right Nothing)
      -- callGF logger fileName
      -- responder (Right $ Just rsp)

  , requestHandler J.STextDocumentDocumentSymbol $ \req responder -> do
      debugM logger "reactor.handle" "Processing a textDocument/documentSymbol request"
      let J.DocumentSymbolParams _ _ doc = req ^. J.params
          loc = J.Location (doc ^. J.uri) (J.Range (J.Position 0 0) (J.Position 0 0))
          sym = J.SymbolInformation "lsp-hello" J.SkFunction Nothing Nothing loc Nothing
          rsp = J.InR (J.List [sym])
      responder (Right rsp)

  , requestHandler J.STextDocumentCodeAction $ \req responder -> do
      debugM logger "reactor.handle" "Processing a textDocument/codeAction request"
      let params = req ^. J.params
          doc = params ^. J.textDocument
          (J.List diags) = params ^. J.context . J.diagnostics
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand (J.Diagnostic (J.Range startPo _) _s _c (Just "lsp-hello") _m _t _l) = [J.Command title cmd cmdparams]
            where
              title = "Apply LSP hello command:" <> headDef "" (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
              cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
              args = J.List
                      [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                      , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON startPo)])]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m _t _l) = []
          rsp = J.List $ map J.InL $ concatMap makeCommand diags
      responder (Right rsp)

  , requestHandler J.SWorkspaceExecuteCommand $ \req responder -> do
      debugM logger "reactor.handle" "Processing a workspace/executeCommand request"
      let params = req ^. J.params
          margs = params ^. J.arguments

      debugM logger "reactor.handle" $ "The arguments are: " ++ show margs
      responder (Right (J.Object mempty)) -- respond to the request

      void $ withProgress "Executing some long running command" Cancellable $ \update ->
        forM [0..10] $ \i -> do
          update (ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
          liftIO $ threadDelay (1 * 1000000)
  , requestHandler J.STextDocumentDefinition $ \req responder -> do
      -- let J.DefinitionParams {J._textDocument = doc, J._position = pos} = req
      let doc = req ^. J.params . J.textDocument . J.uri
      let pos = req ^. J.params . J.position
      debugM logger "def" $ "Got request: " ++ show req
      let fileName = J.uriToFilePath doc
      hovStr <- getHoverString logger pos doc
      case hovStr of
        Nothing -> do
          warningM logger "def" $ "Didn't find string for: " ++ show pos
          responder $ Right $ J.InR $ J.InL $ J.List []
        Just (fullWord, _range) -> do
          debugM logger "def" $ "Found word: " ++ show fullWord
          -- debugM logger "reactor.handle" $ "Guessing range: " ++ show range
          (tags, gr, modEnv) <- getCompileEnv
          case takeBaseName <$> fileName of
            Nothing -> do
              -- TODO: Clean this nesting up
              debugM logger "reactor.handle" $ "Didn't find filename for: " ++ show doc
              responder $ Right $ J.InR $ J.InL $ J.List []
            Just modName -> do
              debugM logger "definition.handle" $ "For file named: " ++ show modName
              let absName = GF.srcAbsName gr (GF.moduleNameS modName)
              mtag <- findTagsForIdentDeep logger (GF.moduleNameS modName) (GF.identS $ T.unpack fullWord) tags
              matag <- findTagsForIdentDeep logger absName (GF.identS $ T.unpack fullWord) tags
              -- debugM logger "definition.handle" $ "Found tags: " ++ show mtag
              forM_ mtag $ \tag -> debugM logger "definition.handle" $ "Found tags: " ++ show tag
              forM_ matag $ \tag -> debugM logger "definition.handle" $ "Found abstract tags: " ++ show tag
              case map location $ matag ++ mtag of
                [] -> do
                  warningM logger "reactor.handle" "Failed to find tag"
                  -- Warning already handled
                  responder $ Right $ J.InR $ J.InL $ J.List []
                tagThings -> do
                  let getLoc fil0 tag0 = case tag0 of
                        GF.Local l c -> pure (fil0, l,c)
                        GF.NoLoc -> do
                          warningM logger "definition.handle" "No location found"
                          pure (fil0, 0,0)
                        GF.External fil' tag' -> do
                          debugM logger "definition.handle" $ "Found external loc: " ++ show fil'
                          getLoc fil' tag'
                  allLocs <- forM tagThings $ \(fil,loc) -> do
                    debugM logger "definition.handle" $ "Initial file loc: " ++ show fil
                    (fil', l1,l2) <- getLoc fil loc
                    let pos1 = mkPos l1 1 :: J.Position
                    let pos2 = mkPos (l2+1) 1 :: J.Position
                    let uri = J.filePathToUri fil' :: J.Uri
                    pure $ J.Location uri (J.Range pos1 pos2)
                  responder $ Right $ J.InR $ J.InL $ J.List $ List.nub allLocs
  ]

headDef :: a -> [a] -> a
headDef x [] = x
headDef _ (x:_) = x

-- findTagsForIdentDeep :: GF.ModuleName -> GF.Ident -> Map.Map GF.ModuleName Tags -> ExceptT String (LspM LspContext) ()
findTagsForIdentDeep :: LogAction (LspT LspContext IO) (WithSeverity T.Text)
  -> GF.ModuleName -> GF.Ident -> Map.Map GF.ModuleName Tags -> LspM LspContext [Tag]
findTagsForIdentDeep logger mNm ident tags = do
  case findTagsForIdent mNm ident tags of
    Left warn -> do
      warningM logger "reactor.handle" warn
      pure []
    Right tagsHere -> do
      res <- forM tagsHere $ \tag -> do
        case tag of
          LocalTag _ident _kind _loc _typ -> pure [tag]
          ImportedTag _ident mNm' _al _fil -> do
            debugM logger "findTags" $ "Found imported tag: " ++ show tag
            findTagsForIdentDeep logger mNm' ident tags
      pure $ concat res


        -- pure $ Just tag
findTagsForIdent :: GF.ModuleName -> GF.Ident -> Map.Map GF.ModuleName Tags -> Either String [Tag]
findTagsForIdent modName ident tags = do
    mtags <- case Map.lookup modName tags of
      Nothing -> Left $ "Didn't find tags for module: " ++ show modName
      Just mtags -> pure mtags
    case Map.lookup ident mtags of
      Nothing -> do
        Left $ "Didn't find tags for ident: " ++ show ident ++ " in module " ++ show modName
      Just tag -> pure tag
    -- -- debugM logger "reactor.handle" $ "Found tags for ident: " ++ show tag
    -- pure tag

getHoverString :: LogAction (LspT LspContext IO) (WithSeverity T.Text)
  -> J.Position
  -> J.Uri
  -> LspT LspContext IO (Maybe (T.Text, J.Range))
getHoverString logger pos doc = do
      let J.Position _l col = pos
          -- rsp = J.Hover ms (Just range)
          -- ms = J.HoverContents $ J.markedUpContent "lsp-hello" "" -- "Your type info here!"
          -- range = J.Range pos pos
          lineStart = pos {J._character = 0}
          lineRange = J.Range lineStart (lineStart & JL.line +~ 1 )
          -- fileName = J.uriToFilePath $ doc
      mdoc <- getVirtualFile $ J.toNormalizedUri doc
      case mdoc of
        Nothing -> do
          debugM logger "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc
          pure Nothing
        Just vf@(VirtualFile _version _fileVersion _) -> do
          let selectedLine = rangeLinesFromVfs vf lineRange
          let (prefix, postfix) = T.splitAt (fromIntegral col) selectedLine
          -- TODO: Use less naive lexer
          let isIdentChar c =
                (c == '_') ||
                (c == '\'') ||
                isDigit c ||
                isAsciiLower c ||
                isAsciiUpper c ||
                (c >= '\192' && c <= '\255' && c /= '\247' && c /= '\215')
          let preWord = T.takeWhileEnd isIdentChar prefix
          let postWord = T.takeWhile isIdentChar postfix
          let fullWord = preWord <> postWord
          debugM logger "reactor.handle" $ "Hovering word: " ++ show fullWord
          let range = J.Range (pos & JL.character -~ fromIntegral (T.length preWord))
                              (pos & JL.character +~ fromIntegral (T.length postWord))
          pure $ Just (fullWord, range)

callGF :: LogAction (LspT LspContext IO) (WithSeverity T.Text) -> J.Uri -> Maybe FilePath -> LspM LspContext ()
callGF logger _ Nothing = do
  -- liftIO $ hPutStrLn stderr "No file"
  errorM logger "reactor.handle" "No file"
  pure ()
callGF logger doc (Just filename) = do
  -- mkdir
  -- debugM logger "reactor.handle" "Starting GF"
  debugM logger "reactor.handle" $ "Starting gf for " ++ filename

  liftIO $ createDirectoryIfMissing False outputDir
  cfg <- config <$> getConfig
  -- optOutputDir
  -- optGFODir
  -- let defaultFlags = GF.flag id GF.noOptions
  let opts = GF.modifyFlags $ \flags -> flags
        { GF.optOutputDir = Just outputDir
        , GF.optGFODir = Just outputDir
        , GF.optMode = GF.ModeCompiler
        , GF.optPMCFG = False
        , GF.optGFLibPath = gfLibPath cfg
        -- , GF.optVerbosity = GF.Verbose
        , GF.optVerbosity = GF.Normal
        -- , GF.optStopAfterPhase = Linker -- Default Compile
         }
  -- let flags = defaultFlags
  -- compileSourceFiles
  cEnv <- getCompileEnv
  -- r <- liftIO $ GF.tryIOE $ stdoutToStdErr $ compileModule opts cEnv filename

  -- Change term to prevent GF from outputting colors
  liftIO $ setEnv "TERM" ""
  (errOut, (output, r)) <- liftIO $ captureStdErr $ captureStdout $ E.try @SomeException $ GF.tryIOE $ compileModule opts cEnv filename
  debugM logger "reactor.handle" "Ran GF"
  debugM logger "reactor.handle" $ "Got stderr: " ++ show errOut
  debugM logger "reactor.handle" $ "Got stdout: " ++ show output

  -- Try parsing the warnings
  let warningForest = case traceShowId $ readP_to_S parseForestFinal errOut of
        [(x,"")] -> x
        p -> [Node (0, "Parsing warnings failed: " ++ show p) []]
  debugM logger "parser" $ "Parsed into: " ++ show warningForest
  mapM_ (debugM logger "parser" . show) warningForest
  mapM_ (debugM logger "moreParsed" . show) $ parseWarnings =<< warningForest

  mkDiagnostics logger opts doc warningForest r
  -- liftIO $ hPutStrLn stderr $ "Done with gf for " ++ filename

type IndentTree = Tree (Int, String)
type IndentForest = [IndentTree]

getCompileEnv :: LspM LspContext CompileEnv
getCompileEnv = liftIO . readTVarIO . compileEnv =<< getConfig

setCompileEnv :: CompileEnv -> LspM LspContext ()
setCompileEnv newEnv = do
  -- TODO: This causes race conditions
  -- If possible delay creating new env until inside the atomically
  envV <- compileEnv <$> getConfig
  liftIO $ atomically $ do
    -- TODO: Check that it matches the expected old value
    writeTVar envV newEnv

-- | Make a simple diagnostic from a severity, a range and a string
diagFor :: J.DiagnosticSource -> J.DiagnosticSeverity -> J.Range -> String -> J.Diagnostic
diagFor src severity rng msg = J.Diagnostic
          rng
          (Just severity)  -- severity
          Nothing  -- code
          (Just src) -- source
          (T.pack msg)
          Nothing -- tags
          (Just (J.List []))

-- | Group a list of pairs into pairs of lists grouped by the first element
--
-- >>> groupByFst [(1,'h'),(2,'w'),(1,'e'),(2,'o'),(1,'l'),(2,'r'),(2,'l'),(1,'l'),(2,'d'),(1,'o')]
-- [(1,"hello"),(2,"world")]
groupByFst :: Ord a => [(a, b)] -> [(a, [b])]
groupByFst = map (\xs -> (fst (head xs), map snd xs)) . List.groupBy ((==) `on` fst) . List.sortBy (comparing fst)

-- TODO: Handle multiple error files at the same time
-- This might resolve the issue with errors disappearing

mkSrcName :: Maybe FilePath -> J.NormalizedUri -> J.DiagnosticSource
mkSrcName mbRootPath nuri = "gf-compiler: " <> filepath
  where
    mbFilePath = do
      nfpath <- J.uriToNormalizedFilePath nuri
      rootPath <- mbRootPath
      pure $ FP.makeRelative rootPath $ J.fromNormalizedFilePath nfpath
    altName = J.getUri $ J.fromNormalizedUri nuri
    filepath = maybe altName T.pack mbFilePath

mkDiagnostics :: LogAction (LspT LspContext IO) (WithSeverity T.Text)
  -> GF.Options -> J.Uri -> IndentForest -> Either SomeException (GF.Err CompileEnv)
  -> LspT LspContext IO ()
mkDiagnostics logger _opts _doc _warnings (Left (SomeException inner)) = do
  warningM logger "gf-compiler" $ "Got error of type " ++ show (typeOf inner)
  errorM logger "gf-compiler" $ "" ++ show (E.displayException inner)
  pure ()
mkDiagnostics logger _ doc warningForest (Right (GF.Ok x)) = do
  rootPath <- getRootPath
  let nuri = J.toNormalizedUri doc
  let srcName = mkSrcName rootPath nuri
  setCompileEnv x
  let parsedWarnings = parseWarnings =<< warningForest
  if null parsedWarnings
    then flushDiagnosticsBySource 100 $ Just srcName
    else do
      -- Flush diagnostics always to ensure diagnostics in other files get cleared
      flushDiagnosticsBySource 100 $ Just srcName
      mbdiags <- handleWarnings logger nuri parsedWarnings []
      case mbdiags of
        Nothing -> pure ()
        Just (nuri', diags) -> publishDiagnostics 100 nuri' Nothing (partitionBySource diags)

  pure ()
mkDiagnostics logger _opts doc _warnings (Right (GF.Bad msg)) = do

  rootPath <- getRootPath
  warningM logger "reactor.handle" $ "Got error:\n" <> msg

  -- flushDiagnosticsBySource 100 $ Just "lsp-hello"
  -- sendDiagnostics (T.pack msg) (J.toNormalizedUri doc) (Just 1)
  -- sendDiagnostics "Failed to compile" (J.toNormalizedUri doc) (Just 1)
  let warningForest = readP_to_S parseForestFinal msg
  debugM logger "Warnings raw" . show $ warningForest
  let parsedWarnings = parseWarningsFromString msg
  debugM logger "Warnings parsed" . show $ parsedWarnings

  -- mapM_ (mapM_ (debugM logger "error msg" . show) . fst) $ readP_to_S parseForestFinal msg
  -- debugM logger "" $ show $ msg
  -- liftIO $ mapM_ (mapM_ (hPrint stderr) . fst) $ readP_to_S parseForestFinal msg
  -- let parsedWarnings = parseWarnings =<< warningForest
  -- debugM logger "" $ show $ warningForest
  -- debugM logger "" $ show $ parsedWarnings
  -- debugM logger "" $ show $ J.toNormalizedUri doc
  let
    nuri = J.toNormalizedUri doc
    sourceName = mkSrcName rootPath nuri
    msgs = splitErrors msg
    fileDiags =
      [ (relFile, DiagInfo J.DsError (Just range) msg1 Nothing)
      | msg1 <- msgs
      , let (relFile, range) = maybe (Nothing, defRange) (first Just) . parseErrorMessage $ msg1
      ]
    rangeFor = maybe (Nothing, defRange) (first Just) . parseErrorMessage
    (relFiles, ranges) = unzip $ map rangeFor msgs
    diags = zipWith (diagFor sourceName J.DsError) ranges msgs
  absFiles <- liftIO $ mapM (mapM canonicalizePath) relFiles
  -- absFiles <- liftIO $ mapM (getRealFile opts) relFiles
  debugM logger "" $ "relFiles: " ++ show relFiles
  debugM logger "" $ "absFiles: " ++ show absFiles


  let
    nuris = map (maybe nuri toNuri) absFiles
    nuri' :: J.NormalizedUri
    nuri' = fromMaybe nuri (allEqual nuris) -- TODO: Do something better when they are not all equal
    -- fps = fromMaybe (error "BUG: fromMaybe") <$> map (J.uriToFilePath . J.fromNormalizedUri) nuris
  when (isNothing (allEqual nuris)) $ do
    warningM logger "" "Ignored errors from other files"

  debugM logger "" $ show nuris

  wdiags <- maybe [] snd <$> handleWarnings logger nuri parsedWarnings fileDiags
  debugM logger "errorMsgs" $ "diags:  " ++ show diags
  debugM logger "errorMsgs" $ "wdiags: " ++ show wdiags
  publishDiagnostics 100 nuri' Nothing (partitionBySource wdiags)

data DiagInfo = DiagInfo {
  diagSeverity :: J.DiagnosticSeverity,
  diagRange :: Maybe J.Range,
  diagMsg :: String,
  searchToken :: Maybe String -- ^ A token to search for to replace range
 }
  -- WarnInfo (Maybe J.Range) (String, Maybe String)
  -- | ErrInfo J.Range String

handleWarnings :: LogAction (LspT LspContext IO) (WithSeverity T.Text)
    -> J.NormalizedUri
    -> [(FilePath, Maybe J.Range, (String, Maybe String))]
    -> [(Maybe FilePath, DiagInfo)]
    -> LspT LspContext IO (Maybe (J.NormalizedUri, [J.Diagnostic]))
handleWarnings logger nuriCurrent parsedWarnings errorDiags =  do
      rootPath <- getRootPath
      let srcName = mkSrcName rootPath nuriCurrent
      let diagsWithFiles = [(Just filename, DiagInfo J.DsWarning rng msg pat) | (filename, rng, (msg, pat)) <- parsedWarnings ]
      -- Error diagnostics first to ensure they are included
      case groupByFst $ errorDiags ++ diagsWithFiles of
        [(relFile, diagInfo)] -> do
          nuri' <- liftIO $ maybe (pure nuriCurrent) (fmap toNuri . canonicalizePath) relFile
          --
          mdoc <- getVirtualFile nuri'
          fileText <- case mdoc of
            Just vf -> do
              let fileText = virtualFileText vf
              debugM logger "foo" $ "Found file: " ++ take 100 (show fileText) ++ " ..."
              pure $ Just $ T.unpack fileText -- TODO: Make this more efficient
            Nothing -> do
              debugM logger "foo" $ "Couldn't find file: " ++ show relFile
              pure Nothing
          let diags = [diagFor srcName sev (guessRange rng ident fileText) msg | DiagInfo sev rng msg ident <- diagInfo]
          -- publishDiagnostics 100 nuri' Nothing (partitionBySource diags)
          return $ Just (nuri', diags)
        _ -> Nothing <$ warningM logger "mkDiagnostrics" "Got diagnostics for mutiple files"


allEqual :: Eq a => [a] -> Maybe a
allEqual (x:xs) | all (==x) xs = Just x
allEqual _ = Nothing

toNuri :: FilePath -> J.NormalizedUri
toNuri = J.normalizedFilePathToUri . J.toNormalizedFilePath

defRange :: J.Range
defRange = J.Range (J.Position 0 1) (J.Position 5 1)

splitErrors :: String -> [String]
splitErrors = joinRelated . filter removeWarnings . map unlines . split (keepDelimsL $ dropInitBlank $ whenElt $ \x -> take 1 x /= " ") . lines
  where
    removeWarnings = not . ("Warning: " `List.isInfixOf`)
    joinRelated (x:y:xs) | "\n  **" `List.isPrefixOf` y = (x ++ y): joinRelated xs
    joinRelated (x:xs) = x : joinRelated xs
    joinRelated [] = []

parseWarningsFromString :: String -> [(FilePath, Maybe J.Range, (String, Maybe String))]
parseWarningsFromString errOut = parsedWarnings
  where
    parsedWarnings = parseWarnings =<< warningForest
    warningForest = case readP_to_S parseForestFinal errOut of
        [(x,"")] -> x
        _ -> []

-- TODO: Make this more intelligent by handling more cases and figuring out locations
parseWarnings :: IndentTree -> [(FilePath, Maybe J.Range, (String, Maybe String))]
parseWarnings (Node (0, h) chldrn) | [filename, ""] <- splitOn ":" h =
  [(filename, Nothing, aWarning) | warning <- chldrn, aWarning <- parseWarning warning]
parseWarnings _ = []

-- | Find which line number has a string
--
-- >>> findLineNumber "hi" "foo bar hi\n  hi there"
findLineNumber :: String -> String -> Maybe Int
findLineNumber needle haystack = listToMaybe startThings
  where
    myLines = zip [1..] $ lines haystack
    -- Cases where the first non-space is the desired token
    startThings = [lineNr | (lineNr, str) <- myLines , (frstWord:_) <- [words str], frstWord == needle]
    -- TODO: Handle more cases

guessRange :: Maybe J.Range -> Maybe String -> Maybe String -> J.Range
guessRange origRange ident fileText = fromMaybe defRange $
    origRange
  <|> do
    lineNrI <- findLineNumber <$> ident <*> fileText
    lineNr <- fromIntegral <$> lineNrI
    pure $ mkRange lineNr 1  (lineNr + 1) 1


-- blackColorEscape = "\ESC[39;49m"

-- | Returns a list of warnings and token names they are warning about
parseWarning :: Tree (Int, String) -> [(String, Maybe String)]
-- parseWarning (Node (n, wrn) [])
--   | "Warning:" `List.isPrefixOf` wrn
--   -- Remove color escape codes from GF output
--   , [realWarn,""] <- splitOn blackColorEscape wrn = [realWarn]
parseWarning (Node (_n, wrn) [])
  | "Warning: no linearization type for" `List.isPrefixOf` wrn
  = [(wrn, Just "lincat")]
parseWarning (Node (_n, wrn) [])
  | "Warning: no linearization of" `List.isPrefixOf` wrn
  = [(wrn, Just "lin")]
parseWarning (Node (_n, wrn) [])
  | ("Warning:": itemType : itemName : _) <- words wrn
  , itemType `elem` ["function", "category"]
  = [(wrn, Just itemName)]
parseWarning (Node (_n, wrn) []) | "Warning:" `List.isPrefixOf` wrn = [(wrn, Nothing)]
parseWarning _ = []

parseErrorMessage :: String -> Maybe (FilePath, J.Range)
parseErrorMessage msg = case lines msg of
  (line1:rest) -> case splitOn ":" line1 of
    [_filename,""] -> parseErrorMessage $ unlines $ map (unwords.words) rest
    [filename , lineNr , col , "" ]
      | [(l,"")] <- reads lineNr
      , [(c,"")] <- reads col               -> Just (filename, mkRange l c l (c+1))
    [filename , lineNr , "" ]
      | [(l,"")] <- reads lineNr            -> Just (filename, mkRange l 1 (l+1) 1)
      | [lineS,lineE] <- splitOn "-" lineNr
      , [(l1,"")] <- reads lineS
      , [(l2,"")] <- reads lineE            -> Just (filename, mkRange l1 1 (l2+1) 1)
    _ -> Nothing
  _ -> Nothing

-- | Convert 1-indexed pos to 0-indexed pos
mkPos :: Int -> Int -> J.Position
mkPos l c = J.Position l' c'
  where
    l' = fromIntegral $ l - 1
    c' = fromIntegral $ c - 1
mkRange :: J.UInt -> J.UInt -> J.UInt -> J.UInt -> J.Range
mkRange l1 c1 l2 c2 = J.Range (J.Position l1' c1') (J.Position l2' c2')
  where
    l1' = l1 - 1
    c1' = c1 - 1
    l2' = l2 - 1
    c2' = c2 - 1

-- ---------------------------------------------------------------------

getIndent :: ReadP Int
-- getIndent = length <$> munch (==' ')
getIndent = length . takeWhile (==' ') <$> look

takeSpaces :: ReadP Int
takeSpaces = length <$> munch (== ' ')

-- finalRow :: ReadP

-- indented :: Int -> ReadP a -> ReadP [a]
-- indented minIndent nested = do
--   m <- getIndent
--   -- if n == m
--   _

data Tree a = Node a [Tree a]
  deriving (Show, Eq, Functor, Foldable)

treeToList :: Tree a -> [a]
treeToList (Node a xs) = a : (treeToList =<< xs)

parseTree :: ReadP (Tree (Int,String))
parseTree = node 0

item :: ReadP (Int, String)
item = (,) <$> takeSpaces <*> munch1 (/= '\n')

forestToString :: [Tree (Int, String)] -> String
forestToString = unlines . map mkLine . concatMap toList
  where mkLine (n, s) = replicate n ' ' ++ s

-- TODO: Try merging next line if appropirate
-- TODO: Extract parser to new module

-- parseForest :: ReadP [Tree (Int,String)]
-- -- parseForest = many $ anyIndent ((,) <$> getIndent <* skipSpaces <*> munch1 (/= '\n')) <* eof
-- -- parseForest = handleChildren (-1) ((,) <$> getIndent <* skipSpaces <*> munch1 (/= '\n'))
-- parseForest = do
--   str <- look
--   traceM $ "parseForest: Looking at: " ++ show str
--   f <- manyGreedy anyIndent
--   traceM $ "anyIndent: Got forest: " ++ show f
--   pure f
parseForestFinal :: ReadP [Tree (Int,String)]
parseForestFinal = many anyIndent <* takeSpaces <* eof

anyIndent :: ReadP (Tree (Int, String))
anyIndent = do
  m <- getIndent
  str <- takeWhile (/='\n') <$> look
  traceM $ "anyIndent: Looking at: " ++ show str
  t <- node m
  traceM $ "anyIndent: Got tree: " ++ show t
  pure t

-- | Like 'many', but prefers to take as many as possible and cuts off as soon
-- as soon as any data is consumed by the inner parser
manyGreedy :: ReadP a -> ReadP [a]
manyGreedy p = mg
  where
    mg = sg +++ pure []
    sg = do x <- p; rest <- mg; pure (x:rest)

-- Question: Should we be more lenient and not require exact indent match?

-- | Parse a tree node with indentation exactly == n
node :: Int -> ReadP (Tree (Int, String))
node expectedIndent = emptyNode <++ node'
  where
    emptyNode = do
      i <- takeSpaces
      void (char '\n')
      pure $ Node (i,"") []

    node' = do
      -- str <- takeWhile (/='\n') <$> look
      -- traceM $ replicate expectedIndent ' ' ++ "Looking at: " ++ show str
      m <- getIndent
      -- traceM $ replicate expectedIndent ' ' ++ "Want " ++ show expectedIndent ++ " have " ++ show m
      guard $ m == expectedIndent
      x <- item
      -- traceM $ replicate expectedIndent ' ' ++ "Got item: " ++ show x
      void (char '\n') <++ eof
      c <- handleChildren m <++ pure []
      -- traceM $ replicate expectedIndent ' ' ++ "Got children: " ++ show c
      pure $ Node x c

whileM :: Monad m => m Bool -> m a -> m [a]
whileM p f = go
  where
    go = do
      x <- p
      if x then (:) <$> f <*> go else pure []

handleChildren :: Int -> ReadP [Tree (Int, String)]
handleChildren parentLevel = do
  newIndent <- getIndent
  -- traceM $ replicate parentLevel ' ' ++ "\\ Want > " ++ show parentLevel ++ " have " ++ show newIndent
  guard $ newIndent > parentLevel
  -- traceM $ replicate parentLevel ' ' ++ "Ok!"
  -- str <- takeWhile (/='\n') <$> look
  -- traceM $ replicate parentLevel ' ' ++ "Looking at child: " ++ show str
  c <- whileM (hasIndent newIndent) (node newIndent )
  -- c <- many (node newIndent item)
  -- traceM $ replicate parentLevel ' ' ++ "Got children inner: " ++ show c
  -- str2 <- takeWhile (/='\n') <$> look
  -- traceM $ replicate parentLevel ' ' ++ "Looking at: " ++ show str2
  pure ()
  pure c
  -- if newIndent > parentLevel
  --   then many (node newIndent item)
  --   else pure []

hasIndent :: Int -> ReadP Bool
hasIndent n = (== n) <$> getIndent

testCase1 :: String
testCase1 = "src/swedish/MorphoSwe.gf:31-40:\n  Happened in the renaming of ptPretForms\n   constant not found: funnenx\n   given Predef, Predef, Prelude, DiffSwe, ResSwe, ParamX,\n         CommonScand, MorphoSwe\nsrc/swedish/MorphoSwe.gf:20-29:\n  Happened in the renaming of ptPretAll\n   constant not found: kox\n   given Predef, Predef, Prelude, DiffSwe, ResSwe, ParamX,\n         CommonScand, MorphoSwe"

testCase2 :: String
testCase2 = "grammars/QuestionsEng.gf:\n   grammars/QuestionsEng.gf:35:\n     Happened in linearization of MkPred1\n      unknown label cxn in\n        {atype : AType;\n         cn : {s : Number => Case => Str; g : Gender; lock_CN : {}};\n         n2 : {s : Number => Case => Str; c2 : Str; g : Gender;\n               lock_N2 : {}};\n         v : {s : Order => Agr => {fin : Str; inf : Str}; lock_VPS : {}};\n         v2 : {s : Order => Agr => {fin : Str; inf : Str}; c2 : Str;\n               lock_VPS2 : {}}}"

-- Should search for "mkA = overloaded"
testCase3 :: String
testCase3 = "ParadigmsYrl.gf:\n   ParadigmsYrl.gf:\n     Happened in overloading mkA\n      missing record fields: s, c, v type of ss s\n      expected: {s : ResYrl.PsorForm => Str; c : ResYrl.VClass;\n                 lock_A : {}; v : ResYrl.Verbal}\n      inferred: {s : Str}\n      "

-- split :: Eq a => a -> [a] -> [[a]]
-- split d [] = []
-- split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- ---------------------------------------------------------------------

debugM ::LogAction m (WithSeverity T.Text) -> T.Text -> String -> m ()
debugM logger tag message = logger <& (tag <> ": " <> T.pack message) `WithSeverity` Info

warningM ::LogAction m (WithSeverity T.Text) -> T.Text -> String -> m ()
warningM logger tag message = logger <& (tag <> ": " <> T.pack message) `WithSeverity` Warning

errorM ::LogAction m (WithSeverity T.Text) -> T.Text -> String -> m ()
errorM logger tag message = logger <& (tag <> ": " <> T.pack message) `WithSeverity` Error

captureStdErr :: IO a -> IO (String, a)
captureStdErr = captureHandleString stderr

captureStdout :: IO a -> IO (String, a)
captureStdout = captureHandleString stdout

captureHandleString :: Handle -> IO a -> IO (String, a)
captureHandleString origHandle act = do
  (readEnd, writeEnd) <- Process.createPipe
  -- TODO: Read the actual content
  res <- goBracket act writeEnd origHandle
  output <- hGetContents' readEnd
  pure (output, res)


-- stdoutToStdErr :: IO a -> IO a
-- stdoutToStdErr act = goBracket act stderr stdout

-- | Copy a handle to another within a bracket
goBracket :: IO a -> Handle -> Handle -> IO a
goBracket go tmpHandle h = do
  buffering <- hGetBuffering h
  let redirect = do
        old <- hDuplicate h
        hDuplicateTo tmpHandle h
        return old
      restore old = do
        hDuplicateTo old h
        hSetBuffering h buffering
        hClose old
  E.bracket redirect restore (const go)
