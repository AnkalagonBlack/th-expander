module Utils.THExpander.Types where

data Env = Env {_envType      :: EnvType,
		_ignoredFiles :: [FilePath],
		_ignoredDirs  :: [FilePath],
		_thModules    :: [String]} deriving (Eq, Show)

data EnvType = Stack | Cabal | Plain deriving (Eq, Show)

