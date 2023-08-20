module GFTags where


import GF.Infra.Option
import GF.Infra.UseIO
import GF.Data.Operations
import GF.Grammar

--import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
--import Control.Monad
import GF.Text.Pretty hiding ((<>))
import System.FilePath
import Data.Monoid (Endo (Endo))

-- ---------------------------

type FileLocation = (FilePath, Location)

data Tag
    = LocalTag
       { identifier :: Ident
       , kind :: String
       , location :: FileLocation
       , gfTypeOf :: Maybe String
       }
    | ImportedTag
       { identifier :: Ident
       , moduleName :: ModuleName
       , moduleAlias :: String
       , tagFileName :: String
       }
    deriving (Show, Eq, Ord)

-- From GF.Compile.Tags
type Tags = Map.Map Ident [Tag]

calculateTags :: Options -> Grammar -> (ModuleName, ModuleInfo) -> Tags
calculateTags opts gr mo =
  let imports = Map.fromListWith Set.union $ map (\x -> (identifier x, Set.singleton x))  $ getImports opts gr mo
      locals  = getLocalTags mo
      txt     = Map.unionWith Set.union imports locals
  in fmap Set.toList txt

-- TODO: Don't destruct map just to reconstruct the same map
getLocalTags :: (ModuleName, ModuleInfo) -> Map.Map Ident (Set.Set Tag)
getLocalTags (m,mi) =
  -- foldMap (_ . getLocations . snd) $ Map.toList (jments mi)
  flip Map.mapWithKey (jments mi) $ \i jment ->
       Set.fromList [ LocalTag i k l t | (k,l,t)   <- getLocations jment]
  where
    getLocations :: Info -> [] (String,FileLocation,Maybe String)
    getLocations (AbsCat mb_ctxt)               = maybe (loc "cat")          mb_ctxt
    getLocations (AbsFun mb_type _ mb_eqs _)    = maybe (ltype "fun")        mb_type <>
                                                  maybe (list (loc "def"))   mb_eqs
    getLocations (ResParam mb_params _)         = maybe (loc "param")        mb_params
    getLocations (ResValue mb_type)             = ltype "param-value"        mb_type
    getLocations (ResOper  mb_type mb_def)      = maybe (ltype "oper-type")  mb_type <>
                                                  maybe (loc "oper-def")     mb_def
    getLocations (ResOverload _ defs)           = list (\(x,y) -> ltype "overload-type" x <>
                                                                  loc   "overload-def"  y) defs
    getLocations (CncCat mty md mr mprn _)      = maybe (loc "lincat")       mty <>
                                                  maybe (loc "lindef")       md  <>
                                                  maybe (loc "linref")       mr  <>
                                                  maybe (loc "printname")    mprn
    getLocations (CncFun _ mlin mprn _)         = maybe (loc "lin")          mlin <>
                                                  maybe (loc "printname")    mprn
    getLocations _                              = mempty

    loc kind (L loc _) = singleton (kind,(msrc mi, loc), Nothing)

    ltype kind (L loc ty) = singleton (kind,(msrc mi, loc),Just $ render (ppTerm Unqualified 0 ty))

    -- maybe f (Just x) = f x
    -- maybe f Nothing  = mempty
    maybe = foldMap

    render = renderStyle style{mode=OneLineMode}

    list = foldMap

singleton = (:[])

getImports :: Options -> Grammar -> (ModuleName, ModuleInfo) -> [Tag]
getImports opts gr mo@(m,mi) = concatMap toDep allOpens
  where
    allOpens = [(OSimple m,incl) | (m,incl) <- mextend mi] ++
               [(o,MIAll) | o <- mopens mi]

    toDep (OSimple m,incl)     =
      let Ok mi = lookupModule gr m
      in [ImportedTag id m "" $ gf2mygftags opts (orig mi info)
            | (id,info) <- Map.toList (jments mi), filter incl id]
    toDep (OQualif m1 m2,incl) =
      let Ok mi = lookupModule gr m2
      in [ImportedTag id m2 (render m1) $ gf2mygftags opts (orig mi info)
            | (id,info) <- Map.toList (jments mi), filter incl id]

    filter MIAll          id = True
    filter (MIOnly   ids) id = id `elem` ids
    filter (MIExcept ids) id = id `notElem` ids

    orig mi info =
      case info of
        AnyInd _ m0 -> let Ok mi0 = lookupModule gr m0
                       in msrc mi0
        _           ->    msrc mi

gftagsFile :: FilePath -> FilePath
gftagsFile f = addExtension f "gf-tags1"

gf2mygftags :: Options -> FilePath -> FilePath
gf2mygftags opts file = maybe (gftagsFile (dropExtension file))
                            (\dir -> dir </> gftagsFile (dropExtension (takeFileName file)))
                            (flag optOutputDir opts)

-- Diff-lists for faster append
type DList a = Endo [a]
dlSingle :: a -> DList a
dlSingle x = Endo (x:)

dlToList :: DList a -> [a]
dlToList (Endo x) = x []

dlConcatMap :: Foldable t => (a -> DList a) -> t a -> DList a
dlConcatMap = foldMap
