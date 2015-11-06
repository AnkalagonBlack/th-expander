module Utils.THExpander.Types where

data Env = Env {_envType      :: EnvType,
		_ignoredFiles :: [FilePath], --relative to dirToExpand paths of ignored files
		_ignoredDirs  :: [FilePath], --relative to dirToExpand paths of ignored dirs
		_thModules    :: [String],
		_dirToExpand  :: String,
		_expandTo     :: String} deriving (Eq, Show)

data EnvType = Stack | Cabal | Plain deriving (Eq, Show)

