module GFExtras where

-- Copy of GF.Compile, but actually exporting everything

import GF.Compile.GrammarToPGF(mkCanon2pgf)
import GF.Compile.ReadFiles(ModEnv,getOptionsFromFile,getAllFiles,
                            importsOfModule)
import GF.CompileOne(compileOne, OneOutput)

import GF.Grammar.Grammar(Grammar,emptyGrammar,
                          abstractOfConcrete,prependModule, Module)--,msrc,modules

import GF.Infra.Ident(ModuleName,moduleNameS)--,showIdent
import GF.Infra.Option
import GF.Infra.UseIO(IOE,FullPath,liftIO,getLibraryDirectory,putIfVerb,
                      justModuleName,extendPathEnv,putStrE,putPointE, Output (ePutStrLn))
import GF.Data.Operations(raise,(+++),err)

import Control.Monad(foldM,when,(<=<),filterM,liftM)
import GF.System.Directory(doesFileExist,getModificationTime)
import System.FilePath((</>),isRelative,dropFileName)
import qualified Data.Map as Map(empty,insert,elems) --lookup
import Data.List(nub)
import Data.Time(UTCTime)
import GF.Text.Pretty(render,($$),(<+>),nest)

import PGF.Internal(optimizePGF)
import PGF(PGF,defaultProbabilities,setProbabilities,readProbabilitiesFromFile)
import GFTags

{-
-- to compile a set of modules, e.g. an old GF or a .cf file
compileSourceGrammar :: Options -> Grammar -> IOE Grammar
compileSourceGrammar opts gr = do
  cwd <- getCurrentDirectory
  (_,gr',_) <- foldM (\env -> compileSourceModule opts cwd env Nothing)
                     emptyCompileEnv
                     (modules gr)
  return gr'
-}

-- | compile with one module as starting point
-- command-line options override options (marked by --#) in the file
-- As for path: if it is read from file, the file path is prepended to each name.
-- If from command line, it is used as it is.

compileModule :: Options -- ^ Options from program command line and shell command.
              -> CompileEnv -> FilePath -> IOE CompileEnv
compileModule opts1 env@(_,rfs) file =
  do file <- getRealFile file
     opts0 <- getOptionsFromFile file
     let curr_dir = dropFileName file
     lib_dirs <- getLibraryDirectory (addOptions opts0 opts1)
     let opts = addOptions (fixRelativeLibPaths curr_dir lib_dirs opts0) opts1
--     putIfVerb opts $ "curr_dir:" +++ show curr_dir ----
--     putIfVerb opts $ "lib_dir:" +++ show lib_dirs ----
     ps0 <- extendPathEnv opts
     let ps = nub (curr_dir : ps0)
--     putIfVerb opts $ "options from file: " ++ show opts0
--     putIfVerb opts $ "augmented options: " ++ show opts
     putIfVerb opts $ "module search path:" +++ show ps ----
     files <- getAllFiles opts ps rfs file
     putIfVerb opts $ "files to read:" +++ show files ----
     let names = map justModuleName files
     putIfVerb opts $ "modules to include:" +++ show names ----
     foldM (compileOne' opts) env files
  where
    getRealFile file = do
      exists <- doesFileExist file
      if exists
        then return file
        else if isRelative file
               then do
                       lib_dirs <- getLibraryDirectory opts1
                       let candidates = [ lib_dir </> file | lib_dir <- lib_dirs ]
                       putIfVerb opts1 (render ("looking for: " $$ nest 2 candidates))
                       file1s <- filterM doesFileExist candidates
                       case length file1s of
                         0 -> raise (render ("Unable to find: " $$ nest 2 candidates))
                         1 -> do return $ head file1s
                         _ -> do putIfVerb opts1 ("matched multiple candidates: " +++ show file1s)
                                 return $ head file1s
               else raise (render ("File" <+> file <+> "does not exist"))

compileOne' :: Options -> CompileEnv -> FullPath -> IOE CompileEnv
compileOne' opts env@(gr,_) = extendCompileEnv env <=< writeTagFile opts env <=< compileOne opts gr

writeTagFile :: Options -> CompileEnv -> OneOutput -> IO OneOutput
writeTagFile opts env@(srcgr, _) input@(Nothing,modl) = ePutStrLn "No filename" >> pure input
writeTagFile opts env@(srcgr, _) input@(Just file,modl) = ePutStrLn ("Writing tags for: " ++ file) >> input <$ writeMyTags opts srcgr (gf2mygftags opts file) modl

-- auxiliaries

-- | The environment

type CompileEnv = (Grammar,ModEnv)

emptyCompileEnv :: CompileEnv
emptyCompileEnv = (emptyGrammar,Map.empty)

extendCompileEnv (gr,menv) (mfile,mo) =
  do menv2 <- case mfile of
                Just file ->
                  do let (mod,imps) = importsOfModule mo
                     t <- getModificationTime file
                     return $ Map.insert mod (t,imps) menv
                _ -> return menv
     return (prependModule gr mo,menv2)


-- ---------------------------

-- From Compile.hs

getRealFile opts1 file = do
      exists <- doesFileExist file
      if exists
        then return file
        else if isRelative file
               then do
                       lib_dirs <- getLibraryDirectory opts1
                       let candidates = [ lib_dir </> file | lib_dir <- lib_dirs ]
                       putIfVerb opts1 (render ("looking for: " $$ nest 2 candidates))
                       file1s <- filterM doesFileExist candidates
                       case length file1s of
                         0 -> raise (render ("Unable to find: " $$ nest 2 candidates))
                         1 -> do return $ head file1s
                         _ -> do putIfVerb opts1 ("matched multiple candidates: " +++ show file1s)
                                 return $ head file1s
               else raise (render ("File" <+> file <+> "does not exist"))
