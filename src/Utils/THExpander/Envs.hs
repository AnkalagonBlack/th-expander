module Utils.THExpander.Envs (
	cabalEnv,
	stackEnv,
	plainEnv
) where

import Utils.THExpander.Types

standardQualifyAs = [("GHC.Base.", ""),
                     ("GHC.Types.", ""),
                     ("Data.Tuple.", ""),
                     ("GHC.IORef.", "")]

cabalEnv = Env {
	_envType       = Cabal,
	_ignoredFiles  =  [],
	_ignoredDirs   =  [".cabal-sandbox"],
	_dirsNotToCopy =  ["build", "dist"],
	_thModules     =  [],
	_dirToExpand   =  "",
	_expandTo      =  "",
    _qualifyAs     =  standardQualifyAs}

stackEnv = Env {
	_envType       = Stack,
	_ignoredFiles  =  [],
	_ignoredDirs   =  [".stack-work"],
	_dirsNotToCopy =  ["build", "dist"],
	_thModules     =  [],
	_dirToExpand   =  "",
	_expandTo      =  "",
    _qualifyAs     = standardQualifyAs}

plainEnv = Env {
	_envType       = Plain,
	_ignoredFiles  =  [],
	_ignoredDirs   =  [],
	_dirsNotToCopy =  [],
	_thModules     =  [],
	_dirToExpand   =  "",
	_expandTo      =  "",
    _qualifyAs     = standardQualifyAs}

