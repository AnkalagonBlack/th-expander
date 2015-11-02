{-# LANGUAGE OverloadedStrings #-}

module THExpander
    ( expand
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

data Env = Env {envType      :: EnvType,
		ignoredFiles :: [FilePath],
		ignoredDirs  :: [FilePath]}

data EnvType = Stack | Cabal | Plain	

cabalEnv = Env Cabal [] [".cabal-sandbox"]
stackEnv = Env Stack [] [".stack-work"]
plainEnv = Env Plain [] []

expand :: FilePath -> IO ()
expand d = do	
	dir   <- getDirectory d
	dir'  <- copyTo (d ++ "-expanded") dir
	files <- filterM shouldBeExpanded $ flatten dir'
	putStrLn $ "total files with templates: " ++ (show . length) files
	mapM_ expandOne files	

withoutExtension = reverse . tail . dropWhile (/= '.') . reverse
extension        = reverse . takeWhile (/= '.') . reverse

dthFile :: FilePath -> FilePath
dthFile f = withoutExtension f ++ ".th.hs"

expandOne :: FilePath -> IO ()
expandOne f = do
	putStrLn $ "expanding " ++ f ++ ".."
	callCommand $ "stack exec ./ghcrunner.sh " ++ f
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

shouldBeExpanded :: FilePath -> IO Bool
shouldBeExpanded = allM [isFile,
		        (kleisify $ (=="hs") . extension),
			hasSplices]	
	
