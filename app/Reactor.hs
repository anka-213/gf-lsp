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
module Main (main) where
import           Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import qualified Colog.Core as L
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                     as E
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

import System.Environment (withArgs, setEnv)
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
import System.FilePath (takeBaseName)

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Redundant lambda"   :: String) #-}
-- ---------------------------------------------------------------------
--

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- | The directory to put .gfo files
outputDir :: String
outputDir = ".gf-lsp"

-- TODO: Generate type lenses for functions
-- TODO: Show concrete types on hover
-- TODO: Don't regenerate pgf for each hover


-- ---------------------------------------------------------------------

data LspContext = LspContext { compileEnv :: TVar CompileEnv , config :: Config }
data Config = Config { fooTheBar :: Bool, wibbleFactor :: Int }
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
      { defaultConfig = LspContext { compileEnv = cEnv, config = Config {fooTheBar = False, wibbleFactor = 0 }}
      , onConfigurationChange = \old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right $ old {config = cfg }
      -- , doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env)
      -- , staticHandlers = lspHandlers rin
      , doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env)
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

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: T.Text -> J.NormalizedUri -> Maybe J.Int32 -> LspM LspContext ()
sendDiagnostics msg fileUri version = do
  let
    diags = [J.Diagnostic
              (J.Range (J.Position 0 1) (J.Position 0 5))
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "lsp-hello") -- source
              msg
              Nothing -- tags
              (Just (J.List []))
            ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

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
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

    goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler (LspM LspContext) a -> Handler (LspM LspContext) a
    goNot f = \msg -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle ::  L.LogAction (LspM LspContext) (WithSeverity T.Text) -> Handlers (LspM LspContext)
handle logger = mconcat
  [ notificationHandler J.SInitialized $ \_msg -> do
      debugM logger "reactor.handle" "Processing the Initialized notification"

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
          J.Position _l col = pos
          -- rsp = J.Hover ms (Just range)
          -- ms = J.HoverContents $ J.markedUpContent "lsp-hello" "" -- "Your type info here!"
          -- range = J.Range pos pos
          lineStart = pos {J._character = 0}
          lineRange = J.Range lineStart (lineStart & JL.line +~ 1 )
          fileName = J.uriToFilePath $ doc
      mdoc <- getVirtualFile $ J.toNormalizedUri doc
      case mdoc of
        Just vf@(VirtualFile _version fileVersion _) -> do
          let selectedLine = rangeLinesFromVfs vf lineRange
          let (prefix, postfix) = T.splitAt (fromIntegral col) selectedLine
          -- TODO: Use less naive lexer
          -- TODO: Handle newline!
          let preWord = T.takeWhileEnd (/= ' ') prefix
          let postWord = T.takeWhile (/= ' ') postfix
          let fullWord = preWord <> postWord
          -- debugM logger "reactor.handle" $ "Found the virtual file: " ++ show fileVersion
          -- debugM logger "reactor.handle" $ "Hovering line: " ++ show selectedLine
          -- debugM logger "reactor.handle" $ "For pos: " ++ show pos
          -- debugM logger "reactor.handle" $ "preWord: " ++ show preWord
          -- debugM logger "reactor.handle" $ "postWord: " ++ show postWord
          debugM logger "reactor.handle" $ "Hovering word: " ++ show fullWord
          let range = J.Range (pos & JL.character -~ fromIntegral (T.length preWord))
                              (pos & JL.character +~ fromIntegral (T.length postWord))
          -- debugM logger "reactor.handle" $ "Guessing range: " ++ show range
          (gr, modEnv) <- getCompileEnv
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
            Just moduleName -> do
              debugM logger "hover.handle" $ "For file named: " ++ show moduleName
              debugM logger "hover.handle" $ "Modules available: " ++ show (fst <$> GF.modules gr)
              -- GF.Compile.link converts gr to pgf
              case PGF.readExpr $ T.unpack fullWord of
                Nothing -> do
                  debugM logger "reactor.handle" $ "Invalid expression: " ++ show fullWord
                  responder (Right Nothing)
                Just expr -> do
                  -- TODO: Catch stderr and exceptions
                  absName <- liftIO $ GF.abstractOfConcrete gr $ GF.moduleNameS moduleName
                  pgf <- liftIO $ GF.link opts (absName , gr)
                  case PGF.inferExpr pgf expr of
                    Left errorMessage -> do
                      debugM logger "reactor.handle" $ "Unable to find type of expr: " ++ show fullWord
                      debugM logger "reactor.handle" $ "Got error: " ++ show (PGF.ppTcError errorMessage)
                      responder (Right Nothing)
                    Right (expr', exprType) -> do
                      let message = PGF.showExpr [] expr' ++ " : " ++ PGF.showType [] exprType
                      let ms = J.HoverContents $ J.markedUpContent "lsp-hello" $ T.pack message
                          rsp = J.Hover ms (Just range)
                      responder (Right $ Just rsp)
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
              title = "Apply LSP hello command:" <> head (T.lines _m)
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
  ]

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
  -- optOutputDir
  -- optGFODir
  -- let defaultFlags = GF.flag id GF.noOptions
  let opts = GF.modifyFlags $ \flags -> flags
        { GF.optOutputDir = Just outputDir
        , GF.optGFODir = Just outputDir
        , GF.optMode = GF.ModeCompiler
        , GF.optPMCFG = False
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
  (errOut, (output, r)) <- liftIO $ captureStdErr $ captureStdout $ GF.tryIOE $ compileModule opts cEnv filename
  debugM logger "reactor.handle" "Ran GF"
  debugM logger "reactor.handle" $ "Got stderr: " ++ show errOut
  debugM logger "reactor.handle" $ "Got stdout: " ++ show output

  -- Try parsing the warnings
  let warningForest = case readP_to_S parseForest errOut of
        [(x,"")] -> x
        _ -> []
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
diagFor :: J.DiagnosticSeverity -> J.Range -> String -> J.Diagnostic
diagFor severity rng msg = J.Diagnostic
          rng
          (Just severity)  -- severity
          Nothing  -- code
          (Just "gf-parser") -- source
          (T.pack msg)
          Nothing -- tags
          (Just (J.List []))

-- | Group a list of pairs into pairs of lists grouped by the first element
--
-- >>> groupByFst [(1,'h'),(2,'w'),(1,'e'),(2,'o'),(1,'l'),(2,'r'),(2,'l'),(1,'l'),(2,'d'),(1,'o')]
-- [(1,"hello"),(2,"world")]
groupByFst :: Ord a => [(a, b)] -> [(a, [b])]
groupByFst = map (\xs -> (fst (head xs), map snd xs)) . List.groupBy ((==) `on` fst) . List.sortBy (comparing fst)

mkDiagnostics :: LogAction (LspT LspContext IO) (WithSeverity T.Text)
  -> GF.Options -> J.Uri -> IndentForest -> GF.Err CompileEnv
  -> LspT LspContext IO ()
mkDiagnostics logger _ _doc warningForest (GF.Ok x) = do
  setCompileEnv x
  let parsedWarnings = parseWarnings =<< warningForest
  if null parsedWarnings
    then flushDiagnosticsBySource 100 $ Just "gf-parser"
    else do
      let diagsWithFiles = [(filename, (range, msg)) | (filename, range, msg) <- parsedWarnings ]
      case groupByFst diagsWithFiles of
        [(relFile, diagInfo)] -> do
          absFile <- liftIO $ canonicalizePath relFile
          let nuri' = toNuri absFile
          --
          mdoc <- getVirtualFile nuri'
          fileText <- case mdoc of
            Just vf -> do
              let fileText = virtualFileText vf
              debugM logger "foo" $ "Found file: " ++ show fileText
              pure $ Just $ T.unpack fileText -- TODO: Make this more efficient
            Nothing -> do
              debugM logger "foo" $ "Couldn't find file: " ++ show relFile
              pure Nothing
          let diags = [diagFor J.DsWarning (guessRange range ident fileText) msg | (range, (msg, ident)) <- diagInfo]
          publishDiagnostics 100 nuri' Nothing (partitionBySource diags)
        _ -> warningM logger "mkDiagnostrics" "Got diagnostics for mutiple files"
  pure ()
mkDiagnostics logger _opts doc _warnings (GF.Bad msg) = do

  warningM logger "reactor.handle" $ "Got error:\n" <> T.pack msg

  -- flushDiagnosticsBySource 100 $ Just "lsp-hello"
  -- sendDiagnostics (T.pack msg) (J.toNormalizedUri doc) (Just 1)
  -- sendDiagnostics "Failed to compile" (J.toNormalizedUri doc) (Just 1)
  -- liftIO $ mapM_ (mapM_ (hPrint stderr) . fst) $ readP_to_S parseForest msg
  liftIO $ hPrint stderr msg
  liftIO $ mapM_ (mapM_ (hPrint stderr) . fst) $ readP_to_S parseForest msg
  let
    nuri = J.toNormalizedUri doc
    msgs = splitErrors msg
    range = maybe (Nothing, defRange) (first Just) . parseErrorMessage
    (relFiles, ranges) = unzip $ map range msgs
    diags = zipWith (diagFor J.DsError) ranges msgs
  absFiles <- liftIO $ mapM (mapM canonicalizePath) relFiles
  -- absFiles <- liftIO $ mapM (getRealFile opts) relFiles
  liftIO $ hPrint stderr relFiles
  liftIO $ hPrint stderr absFiles

  let
    nuris = map (maybe nuri toNuri) absFiles
    nuri' :: J.NormalizedUri
    nuri' = fromMaybe nuri (allEqual nuris) -- TODO: Do something better when they are not all equal
    -- fps = fromMaybe (error "BUG: fromMaybe") <$> map (J.uriToFilePath . J.fromNormalizedUri) nuris
  when (isNothing (allEqual nuris)) $ do
    warningM logger "" "Ignored errors from other files"

  liftIO $ hPrint stderr nuris

  publishDiagnostics 100 nuri' Nothing (partitionBySource diags)

allEqual :: Eq a => [a] -> Maybe a
allEqual (x:xs) | all (==x) xs = Just x
allEqual _ = Nothing

toNuri :: FilePath -> J.NormalizedUri
toNuri = J.normalizedFilePathToUri . J.toNormalizedFilePath

defRange :: J.Range
defRange = J.Range (J.Position 0 1) (J.Position 20 5)

splitErrors :: String -> [String]
splitErrors = map unlines . split (keepDelimsL $ dropInitBlank $ whenElt $ \x -> head x /= ' ') . lines

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

-- indented :: Int -> ReadP a -> ReadP [a]
-- indented minIndent nested = do
--   m <- getIndent
--   -- if n == m
--   _

data Tree a = Node a [Tree a]
  deriving Show

treeToList :: Tree a -> [a]
treeToList (Node a xs) = a : (treeToList =<< xs)

parseTree :: ReadP (Tree (Int,String))
parseTree = node 0 ((,) <$> getIndent <* skipSpaces <*> munch1 (/= '\n'))

parseForest :: ReadP [Tree (Int,String)]
-- parseForest = many $ anyIndent ((,) <$> getIndent <* skipSpaces <*> munch1 (/= '\n')) <* eof
-- parseForest = handleChildren (-1) ((,) <$> getIndent <* skipSpaces <*> munch1 (/= '\n'))
parseForest = many parseTree <* eof

anyIndent :: ReadP a -> ReadP (Tree a)
anyIndent x = do
  m <- getIndent
  node m x

node :: Int -> ReadP a -> ReadP (Tree a)
node expectedIndent item = do
  m <- getIndent
  guard $ m == expectedIndent
  x <- item
  void (char '\n') <++ eof
  c <- handleChildren m item <++ pure []
  pure $ Node x c

handleChildren :: Int -> ReadP a -> ReadP [Tree a]
handleChildren parentLevel item = do
  newIndent <- getIndent
  guard $ newIndent > parentLevel
  many (node newIndent item)
  -- if newIndent > parentLevel
  --   then many (node newIndent item)
  --   else pure []

testCase :: String
testCase = "src/swedish/MorphoSwe.gf:31-40:\n  Happened in the renaming of ptPretForms\n   constant not found: funnenx\n   given Predef, Predef, Prelude, DiffSwe, ResSwe, ParamX,\n         CommonScand, MorphoSwe\nsrc/swedish/MorphoSwe.gf:20-29:\n  Happened in the renaming of ptPretAll\n   constant not found: kox\n   given Predef, Predef, Prelude, DiffSwe, ResSwe, ParamX,\n         CommonScand, MorphoSwe"

testCase2 :: String
testCase2 = "grammars/QuestionsEng.gf:\n   grammars/QuestionsEng.gf:35:\n     Happened in linearization of MkPred1\n      unknown label cxn in\n        {atype : AType;\n         cn : {s : Number => Case => Str; g : Gender; lock_CN : {}};\n         n2 : {s : Number => Case => Str; c2 : Str; g : Gender;\n               lock_N2 : {}};\n         v : {s : Order => Agr => {fin : Str; inf : Str}; lock_VPS : {}};\n         v2 : {s : Order => Agr => {fin : Str; inf : Str}; c2 : Str;\n               lock_VPS2 : {}}}"

-- split :: Eq a => a -> [a] -> [[a]]
-- split d [] = []
-- split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- ---------------------------------------------------------------------

debugM ::LogAction m (WithSeverity T.Text) -> T.Text -> String -> m ()
debugM logger tag message = logger <& (tag <> ": " <> T.pack message) `WithSeverity` Info

warningM ::LogAction m (WithSeverity T.Text) -> T.Text -> T.Text -> m ()
warningM logger tag message = logger <& (tag <> ": " <> message) `WithSeverity` Warning

errorM ::LogAction m (WithSeverity T.Text) -> T.Text -> T.Text -> m ()
errorM logger tag message = logger <& (tag <> ": " <> message) `WithSeverity` Error

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
