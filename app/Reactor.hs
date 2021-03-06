{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                     as E
import           Control.Lens hiding (Iso)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import           GHC.Generics (Generic)
import           Language.LSP.Server
import           Language.LSP.Diagnostics
import qualified Language.LSP.Types            as J
import qualified Language.LSP.Types.Lens       as J
import           Language.LSP.VFS
import           System.Exit
import System.Log.Logger
    ( errorM, debugM, removeAllHandlers, Priority(DEBUG), warningM )
import           Control.Concurrent
import           System.Directory (createDirectoryIfMissing, copyFile, canonicalizePath)

import qualified GF
import qualified GF.Infra.Option as GF

import GFExtras

import System.Environment (withArgs)
import System.IO (hPutStrLn, stderr, Handle, stdout, hPrint)
import qualified GF.Support as GF
import qualified GF.Compile as S
import GF.Compiler (linkGrammars)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import GHC.IO.Handle
import Data.List.Split
import Text.ParserCombinators.ReadP
import Control.Arrow (first)

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------
--

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

data LspContext = LspContext { compileEnv :: TVar CompileEnv , config :: Config }
data Config = Config { fooTheBar :: Bool, wibbleFactor :: Int }
  deriving (Generic, J.ToJSON, J.FromJSON, Show)

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  cEnv <- newTVarIO emptyCompileEnv :: IO (TVar CompileEnv)

  let
    serverDefinition = ServerDefinition
      { defaultConfig = LspContext { compileEnv = cEnv, config = Config {fooTheBar = False, wibbleFactor = 0 }}
      , onConfigurationChange = \old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right $ old {config = cfg }
      , doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env)
      , staticHandlers = lspHandlers rin
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = lspOptions
      }

  flip E.finally finalProc $ do
    setupLogger Nothing ["reactor"] DEBUG
    runServer serverDefinition

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = removeAllHandlers
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
sendDiagnostics :: T.Text -> J.NormalizedUri -> Maybe Int -> LspM LspContext ()
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
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debugM "reactor" "Started the reactor"
  forever $ do
    input <- atomically $ readTChan inp
    -- debugM "reactor" "Got request"
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
    -- debugM "reactor" "Completed request"

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TChan ReactorInput -> Handlers (LspM LspContext)
lspHandlers rin = mapHandlers goReq goNot handle
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
handle :: Handlers (LspM LspContext)
handle = mconcat
  [ notificationHandler J.SInitialized $ \_msg -> do
      liftIO $ debugM "reactor.handle" "Processing the Initialized notification"

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
      --         liftIO $ debugM "reactor.handle" "Processing a textDocument/codeLens request"
      --         let cmd = J.Command "Say hello" "lsp-hello-command" Nothing
      --             rsp = J.List [J.CodeLens (J.mkRange 0 0 0 100) (Just cmd) Nothing]
      --         responder (Right rsp)

  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
    -- sendDiagnostics "Example message" (J.toNormalizedUri doc) (Just 0)
    callGF doc fileName

  , notificationHandler J.SWorkspaceDidChangeConfiguration $ \msg -> do
      cfg <- config <$> getConfig
      liftIO $ debugM "configuration changed: " (show (msg,cfg))
      sendNotification J.SWindowShowMessage $
        J.ShowMessageParams J.MtInfo $ "Wibble factor set to " <> T.pack (show (wibbleFactor cfg))

  , notificationHandler J.STextDocumentDidChange $ \msg -> do
    let doc  = msg ^. J.params
                    . J.textDocument
                    . J.uri
                    . to J.toNormalizedUri
    liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
    mdoc <- getVirtualFile doc
    case mdoc of
      Just (VirtualFile _version str _) -> do
        liftIO $ debugM "reactor.handle" $ "Found the virtual file: " ++ show str
      Nothing -> do
        liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc

  , notificationHandler J.STextDocumentDidSave $ \msg -> do
      let doc = msg ^. J.params . J.textDocument . J.uri
          fileName = J.uriToFilePath doc
      liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
      callGF doc fileName
      -- sendDiagnostics "Example message" (J.toNormalizedUri doc) Nothing

  , requestHandler J.STextDocumentRename $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/rename request"
      let params = req ^. J.params
          J.Position l c = params ^. J.position
          newName = params ^. J.newName
      vdoc <- getVersionedTextDoc (params ^. J.textDocument)
      -- Replace some text at the position with what the user entered
      let edit = J.InL $ J.TextEdit (J.mkRange l c l (c + T.length newName)) newName
          tde = J.TextDocumentEdit vdoc (J.List [edit])
          -- "documentChanges" field is preferred over "changes"
          rsp = J.WorkspaceEdit Nothing (Just (J.List [J.InL tde])) Nothing
      responder (Right rsp)

  , requestHandler J.STextDocumentHover $ \req responder -> do
      -- liftIO $ debugM "reactor.handle" "Processing a textDocument/hover request"
      let J.HoverParams doc pos _workDone = req ^. J.params
          J.Position _l _c' = pos
          rsp = J.Hover ms (Just range)
          ms = J.HoverContents $ J.markedUpContent "lsp-hello" "Your type info here!"
          range = J.Range pos pos
          fileName = J.uriToFilePath $ doc ^. J.uri
      -- callGF fileName
      responder (Right $ Just rsp)

  , requestHandler J.STextDocumentDocumentSymbol $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/documentSymbol request"
      let J.DocumentSymbolParams _ _ doc = req ^. J.params
          loc = J.Location (doc ^. J.uri) (J.Range (J.Position 0 0) (J.Position 0 0))
          sym = J.SymbolInformation "lsp-hello" J.SkFunction Nothing Nothing loc Nothing
          rsp = J.InR (J.List [sym])
      responder (Right rsp)

  , requestHandler J.STextDocumentCodeAction $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/codeAction request"
      let params = req ^. J.params
          doc = params ^. J.textDocument
          (J.List diags) = params ^. J.context . J.diagnostics
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m _t _l) = [J.Command title cmd cmdparams]
            where
              title = "Apply LSP hello command:" <> head (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
              cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
              args = J.List
                      [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                      , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m _t _l) = []
          rsp = J.List $ map J.InL $ concatMap makeCommand diags
      responder (Right rsp)

  , requestHandler J.SWorkspaceExecuteCommand $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a workspace/executeCommand request"
      let params = req ^. J.params
          margs = params ^. J.arguments

      liftIO $ debugM "reactor.handle" $ "The arguments are: " ++ show margs
      responder (Right (J.Object mempty)) -- respond to the request

      void $ withProgress "Executing some long running command" Cancellable $ \update ->
        forM [(0 :: Double)..10] $ \i -> do
          update (ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
          liftIO $ threadDelay (1 * 1000000)
  ]

outputDir :: String
outputDir = "generated"

callGF :: J.Uri -> Maybe FilePath -> LspM LspContext ()
callGF _ Nothing = do
  liftIO $ hPutStrLn stderr "No file"
callGF doc (Just filename) = do
  -- mkdir
  liftIO $ debugM "reactor.handle" "Starting GF"
  liftIO $ hPutStrLn stderr $ "Starting gf for " ++ filename

  liftIO $ createDirectoryIfMissing False outputDir
  -- optOutputDir
  -- optGFODir
  let defaultFlags = GF.flag id GF.noOptions
  let opts = GF.modifyFlags $ \flags -> flags
        { GF.optOutputDir = Just outputDir
        , GF.optGFODir = Just outputDir
        , GF.optMode = GF.ModeCompiler
        , GF.optPMCFG = False
        , GF.optVerbosity = GF.Verbose
        -- , GF.optStopAfterPhase = Linker -- Default Compile
         }
  -- let flags = defaultFlags
  -- compileSourceFiles
  cEnv <- getCompileEnv
  r <- liftIO $ GF.tryIOE $ stdoutToStdErr $ compileModule opts cEnv filename
  liftIO $ debugM "reactor.handle" "Ran GF"

  mkDiagnostics opts doc r
  liftIO $ hPutStrLn stderr $ "Done with gf for " ++ filename

getCompileEnv :: LspM LspContext CompileEnv
getCompileEnv = liftIO . readTVarIO . compileEnv =<< getConfig

setCompileEnv :: CompileEnv -> LspM LspContext ()
setCompileEnv newEnv = do
  envV <- compileEnv <$> getConfig
  liftIO $ atomically $ do
    -- TODO: Check that it matches the expected old value
    writeTVar envV newEnv

mkDiagnostics :: GF.Options -> J.Uri -> GF.Err CompileEnv -> LspT LspContext IO ()
mkDiagnostics _ doc (GF.Ok x) = do
  -- setCompileEnv x
  flushDiagnosticsBySource 100 $ Just "gf-parser"
  pure ()
mkDiagnostics opts doc (GF.Bad msg) = do
  liftIO $ warningM "reactor.handle" $ "Got error:\n" ++ msg
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
    diags = zipWith diagFor ranges msgs
    diagFor rng msg = J.Diagnostic
              rng
              (Just J.DsError)  -- severity
              Nothing  -- code
              (Just "gf-parser") -- source
              (T.pack msg)
              Nothing -- tags
              (Just (J.List []))
  absFiles <- liftIO $ mapM (mapM canonicalizePath) relFiles
  -- absFiles <- liftIO $ mapM (getRealFile opts) relFiles
  liftIO $ hPrint stderr relFiles
  liftIO $ hPrint stderr absFiles

  let
    nuris = map (maybe nuri toNuri) absFiles
    nuri' :: J.NormalizedUri
    nuri' = fromMaybe nuri (allEqual nuris) -- TODO: Do something better when they are not all equal
    -- fps = fromMaybe (error "BUG: fromMaybe") <$> map (J.uriToFilePath . J.fromNormalizedUri) nuris

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

parseErrorMessage :: String -> Maybe (FilePath, J.Range)
parseErrorMessage msg = case lines msg of
  (line1:rest) -> case splitOn ":" line1 of
    [filename,""] -> parseErrorMessage $ unlines $ map (unwords.words) rest
    [filename , line , col , "" ]
      | [(l,"")] <- reads line
      , [(c,"")] <- reads col               -> Just (filename, mkRange l c l (c+1))
    [filename , line , "" ]
      | [(l,"")] <- reads line              -> Just (filename, mkRange l 1 (l+1) 1)
      | [lineS,lineE] <- splitOn "-" line
      , [(l1,"")] <- reads lineS
      , [(l2,"")] <- reads lineE            -> Just (filename, mkRange l1 1 (l2+1) 1)
    _ -> Nothing

mkRange :: Int -> Int -> Int -> Int -> J.Range
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

stdoutToStdErr :: IO a -> IO a
stdoutToStdErr act = goBracket act stderr stdout

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