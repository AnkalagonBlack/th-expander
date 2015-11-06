module Utils.THExpander.Envs (
	cabalEnv,
	stackEnv,
	plainEnv
) where

import Utils.THExpander.Types

cabalEnv = Env {
	_envType       = Cabal,
	_ignoredFiles  =  [],
	_ignoredDirs   =  [".cabal-sandbox"],
	_dirsNotToCopy =  ["build", "dist"],
	_thModules     =  [],
	_dirToExpand   =  "",
	_expandTo      =  ""}

stackEnv = Env {
	_envType       = Stack,
	_ignoredFiles  =  [],
	_ignoredDirs   =  [".stack-work"],
	_dirsNotToCopy =  ["build", "dist"],
	_thModules     =  [],
	_dirToExpand   =  "",
	_expandTo      =  ""}

plainEnv = Env {
	_envType       = Plain,
	_ignoredFiles  =  [],
	_ignoredDirs   =  [],
	_dirsNotToCopy =  [],
	_thModules     =  [],
	_dirToExpand   =  "",
	_expandTo      =  ""}

