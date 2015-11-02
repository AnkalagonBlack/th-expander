{-# LANGUAGE OverloadedStrings #-}

module Utils.THExpander
    ( Env,
      EnvType,
      expand,
      cabalEnv,
      stackEnv,
      plainEnv
    ) where

import Prelude hiding (filter, any, reverse)
import System.Process
import System.File.Tree hiding (mapM_, filterM)
import Data.Foldable  (toList)
import Control.Monad  (filterM)
import Data.List as L
import Data.List.Split
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Char as Chr
import Data.Bool.Kleisli
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import System.FilePath hiding ((<.>))

data Env = Env {envType      :: EnvType,
		ignoredFiles :: [FilePath],
		ignoredDirs  :: [FilePath]}

data EnvType = Stack | Cabal | Plain deriving (Eq, Show)

type THExpanderMonad a = ReaderT Env IO a

cabalEnv = Env Cabal [] [".cabal-sandbox"]
stackEnv = Env Stack [] [".stack-work"]
plainEnv = Env Plain [] []

expand :: FilePath -> THExpanderMonad ()
expand d = do	
	dir   <- lift . getDirectory $ d
	dir'  <- lift . copyTo (d ++ "-expanded") $ dir
	lift . putStrLn . show . flatten $ dir'
	files <- filterM shouldBeExpanded $ flatten dir'
	lift . putStrLn $ "total files with templates: " ++ (show . length) files
	eType <- envType <$> ask
	lift . mapM_ (expandOne eType) $ files	

dthFile :: FilePath -> FilePath
dthFile = (flip replaceExtension) "th.hs"

generateCompileString :: EnvType -> String
generateCompileString t | t == Stack || t == Cabal = (env t) ++ " exec ghc " ++ " -- " ++ "-dth-dec-file -ddump-splices "
			| t == Plain               = "ghc -dth-dec-file -ddump-splices "
	where env Stack = "stack"
	      env Cabal = "cabal"

expandOne :: EnvType -> FilePath -> IO ()
expandOne t f = do
	putStrLn $ "expanding " ++ f ++ ".."
	let cm =  generateCompileString t ++ f
	putStrLn cm
	callCommand $ cm
	removeTH f
	runCommand $ "cat " ++ dthFile f ++ " >> " ++ f
	putStrLn "done"

isSplice = B.isPrefixOf "$"

hasSplices :: FilePath -> IO Bool
hasSplices f = do
	content <- B.readFile f
	let ls = C.split '\n' content	
	return $ any isSplice ls

removeTH :: FilePath -> IO ()
removeTH f = do
	content <- B.readFile f
	B.writeFile f . C.unlines . L.map removeSpliceOrExtension . C.split '\n' $ content 	

isLanguageString :: B.ByteString -> Bool
isLanguageString  = B.isPrefixOf "{-#"

languageLineContent = C.pack . L.reverse . L.dropWhile (not . Chr.isAlpha) . L.reverse . L.dropWhile (not . Chr.isAlpha) . C.unpack

enclose str1 str2 str = str1 `C.append` str `C.append` str2

--from line containing extensions info we should remove TemplateHaskell and from line which is splice we should remove everything
removeSpliceOrExtension	:: B.ByteString -> B.ByteString
removeSpliceOrExtension str | isLanguageString str = enclose "{-#" "#-}" . B.concat . L.filter (not . B.isInfixOf "TemplateHaskell") . C.splitWith f $ languageLineContent str
			    | isSplice str         = "--there was TH splice."
			    | otherwise            = str	where
	f c = (c == ',') || (c == ' ')

shouldBeExpanded :: FilePath -> THExpanderMonad Bool
shouldBeExpanded = allM [lift . (not <.> isDir),
		        (kleisify $ (==".hs") . takeExtension),
			notIgnored,
			lift . hasSplices]	
	
notIgnored :: FilePath -> THExpanderMonad Bool
notIgnored f = do
	iDirs <- ignoredDirs <$> ask
	iFiles <- ignoredFiles <$> ask	
	let inIgnoredDir  = or . L.map (`isPrefixOf` f) $ iDirs
	let isIgnoredFile = (flip elem) iFiles . takeFileName $ f
	return $ not (inIgnoredDir || isIgnoredFile)

