{-
 - Expands TH splices
 -
 - I have written it as a quick and dirty way to have template haskell in Haste code.
 - Later I have discovered https://github.com/RichiH/git-annex/blob/master/Build/EvilSplicer.hs
 - and it seems that it does the same thing, but better :) I hadn't time to rewrite code
 - that used my th-expander and decided not to migrate on EvilSplicer, but added some functions from there.
 -}

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Utils.THExpander
    ( Env,
      EnvType,
      expand,
      expandToDefault,
      envType,
      ignoredDirs,
      ignoredFiles,
      thModules,
      (^.), --exporting some lens functions to be able to easily modify environments
      (.~),
      (%~),      
      module Utils.THExpander.Types,
      module Utils.THExpander.Envs
    ) where

import Prelude hiding (filter, any, reverse)
import System.Process
import System.File.Tree hiding (mapM_, filterM)
import qualified System.File.Tree as S (filterM)
import Data.Foldable  (toList)
import Control.Monad  (filterM, void)
import Data.List as L
import Data.Map as M
import Data.List.Split
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Char as Chr
import Data.Bool.Kleisli
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import System.FilePath hiding ((<.>))
import Control.Lens.TH
import Control.Lens ((^.), (.~), (%~))
import Control.Monad.IO.Class
import Control.Applicative (liftA)
import Data.Tree (drawTree)
import Data.List.Utils (replace)

import Utils.THExpander.Types
import Utils.THExpander.Envs

type THExpanderMonad a = ReaderT Env IO a

$(makeLenses ''Env)

expandToDefault :: Env -> FilePath -> IO ()
expandToDefault env src = expand env src (src ++ "-expanded")

expand :: Env -> FilePath -> FilePath -> IO ()
expand env src target = runReaderT expand' $ env' where
    env' = (%~) dirsNotToCopy (++[target]) ((.~) expandTo target ((.~) dirToExpand src env))


expand' :: THExpanderMonad ()
expand' = do
    src    <- _dirToExpand <$> ask
    target <- _expandTo    <$> ask
    dir    <- lift . getDirectory $ src
    nc     <- S.filterM notCopy [dir]
    dir'   <- lift . copyTo target . head $ nc
    files  <- filterM shouldBeExpanded $ flatten dir'   
    lift . putStrLn $ "files with templates: " ++ (show files)
    mapM_ expandOne $ files 

dthFile :: FilePath -> FilePath
dthFile = (flip replaceExtension) "th.hs"

generateCompileString :: EnvType -> String
generateCompileString t | t == Stack || t == Cabal = (env t) ++ " exec ghc " ++ " -- " ++ "-dth-dec-file -ddump-splices "
            | t == Plain               = "ghc -dth-dec-file -ddump-splices "
    where env Stack = "stack"
          env Cabal = "cabal"



expandOne :: FilePath -> THExpanderMonad ()
expandOne f = do
    t <- _envType <$> ask
    liftIO $ (putStrLn $ "expanding " ++ f ++ "..")
    let cm =  generateCompileString t ++ f
    liftIO $ putStrLn cm
    liftIO $ callCommand $ cm
    removeTH f
    unqualify . dthFile $ f
    liftIO $ runCommand $ "cat " ++ dthFile f ++ " >> " ++ f    
    liftIO $ putStrLn "done"

isSplice = B.isPrefixOf "$"

hasSplices :: FilePath -> IO Bool
hasSplices f = do
    content <- B.readFile f
    let ls = C.split '\n' content   
    return $ any isSplice ls

removeTH :: FilePath -> THExpanderMonad ()
removeTH f = do
    content <- liftIO $ B.readFile f
    thMods  <- _thModules <$> ask
    let rem = L.map (removeSpliceOrExtension thMods) . C.split '\n'
    liftIO $ B.writeFile f . C.unlines . rem $ content


unqualify :: FilePath -> THExpanderMonad ()
unqualify f = do
    content <- liftIO $ B.readFile f
    qs      <- _qualifyAs <$> ask
    let unqualifyString = head . (flip $ scanr $ uncurry replace) qs
    liftIO $ B.writeFile f . C.unlines . L.map C.pack . L.map unqualifyString . L.map C.unpack . C.split '\n'$ content

isLanguageString :: B.ByteString -> Bool
isLanguageString  = B.isPrefixOf "{-#"

languageLineContent = C.pack . L.reverse . L.dropWhile (not . Chr.isAlpha) . L.reverse . L.dropWhile (not . Chr.isAlpha) . C.unpack

enclose str1 str2 str = str1 `C.append` str `C.append` str2

--from line containing extensions info we should remove TemplateHaskell and from line which is splice we should remove everything
removeSpliceOrExtension :: [String] -> B.ByteString -> B.ByteString
removeSpliceOrExtension mods str | isLanguageString str      = processLanguagePragma str
                     | isSplice str              = "--there was some TH splice."                           
                     | isTHImport mods str       = "--there was some TH import"
                     | otherwise                 = str

processLanguagePragma str = ifNotEmpty (enclose "{-#LANGUAGE " "#-}") . C.intercalate "," . L.filter (not . C.null) . tail . L.filter (not . B.isInfixOf "TemplateHaskell") . C.splitWith p $ languageLineContent str where
    p c = (c == ',') || (c == ' ')
    ifNotEmpty f "" = ""
    ifNotEmpty f s  = f s 


isTHImport mods str = (notEmpty && isImport) && isTHModule where
    notEmpty   = not . L.null . C.words $ str
    isImport   = (=="import") . head . C.words $ str
    isTHModule = elem modul $ L.map C.pack mods
    modul      = head . L.filter notKeyWord . C.words $ str
    notKeyWord = and . (<*>) [(/="qualified"), (/="import")] . pure


shouldBeExpanded :: FilePath -> THExpanderMonad Bool
shouldBeExpanded = allM [lift . (not <.> isDir),
                (kleisify $ (==".hs") . takeExtension),
            notIgnored,
            lift . hasSplices]  
notCopy :: FilePath -> THExpanderMonad Bool
notCopy f = do 
    nDirs  <- _dirsNotToCopy  <$> ask
    src    <- _dirToExpand    <$> ask
    let nDirs' = L.map ((src++[pathSeparator])++) nDirs
    return $ not . or . L.map (flip isPrefixOf $ f) $ nDirs'
 
notIgnored :: FilePath -> THExpanderMonad Bool
notIgnored f = do   
    target <- _expandTo     <$> ask
    iDirs  <- _ignoredDirs  <$> ask
    iFiles <- _ignoredFiles <$> ask
    let inIgnoredDir  = or . L.map (flip isPrefixOf $ removePrefix (target ++ [pathSeparator]) f) $ iDirs
    let isIgnoredFile = (flip elem) iFiles . takeFileName $ f
    return $ not (inIgnoredDir || isIgnoredFile)

removePrefix [] str = str
removePrefix _ []   = []
removePrefix (p:ps) str@(s:ss) | p == s = removePrefix ps ss
                   | otherwise = str
