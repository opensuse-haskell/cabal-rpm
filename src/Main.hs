-- |
-- Module      :  Main
-- Copyright   :  (C) 2007  Bryan O'Sullivan
--                (C) 2012-2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Commands.Depends (depends, Depends (..))
import Commands.Diff (diff)
import Commands.Install (install)
import Commands.Refresh (refresh)
import Commands.RpmBuild (rpmBuild_)
import Commands.Spec (createSpecFile_)
import Commands.Update (update)

import PackageUtils (prepare, PackageData (..), RpmStage (..))
import Setup (parseArgs)

import Control.Exception (bracket)
import System.Directory (removeDirectoryRecursive)
import System.Environment (getArgs)

main :: IO ()
main = do
  (opts, cmd, mpkg) <- getArgs >>= parseArgs
  bracket (prepare mpkg opts)
    (maybe (return ()) removeDirectoryRecursive . workingDir)
    (\pkgdata ->
      case cmd of
        "spec"        -> createSpecFile_ pkgdata opts Nothing
        "srpm"        -> rpmBuild_ pkgdata opts Source
        "prep"        -> rpmBuild_ pkgdata opts Prep
        "local"       -> rpmBuild_ pkgdata opts Binary
        "build"       -> rpmBuild_ pkgdata opts Binary
        "builddep"    -> rpmBuild_ pkgdata opts BuildDep
        "diff"        -> diff pkgdata opts
        "install"     -> install pkgdata opts
        "depends"     -> depends pkgdata Depends
        "refresh"     -> refresh pkgdata opts
        "requires"    -> depends pkgdata Requires
        "missingdeps" -> depends pkgdata Missing
        "update"      -> update pkgdata opts
        "rpm"         -> do
          putStrLn "* Warning the 'rpm' command has been renamed to 'local':"
          putStrLn "* this alias may be removed in a future release."
          rpmBuild_ pkgdata opts Binary
        c -> error $ "Unknown cmd: " ++ c)
