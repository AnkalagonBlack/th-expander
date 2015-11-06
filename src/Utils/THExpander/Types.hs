module Utils.THExpander.Types where

data Env = Env {_envType       :: EnvType,
		_ignoredFiles  :: [FilePath], --relative to dirToExpand paths of ignored files
		_ignoredDirs   :: [FilePath], --relative to dirToExpand paths of ignored dirs
		_dirsNotToCopy :: [FilePath], --relative to dirToExpand paths of dirs that should not be copied to expansion dir
		_thModules     :: [String],
		_dirToExpand   :: String,
		_expandTo      :: String} deriving (Eq, Show)

data EnvType = Stack | Cabal | Plain deriving (Eq, Show)

