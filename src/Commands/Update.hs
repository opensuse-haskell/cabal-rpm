-- |
-- Module      :  Commands.Update
-- Copyright   :  (C) 2014-2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: update spec file to a new package version

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Update (
  update
  ) where

import Commands.Spec (createSpecFile)
import Distro (detectDistro, Distro(..))
import FileUtils (withTempDirectory)
import PackageUtils (PackageData (..), bringTarball, isGitDir, latestPkg,
                     packageName, packageVersion, prepare, removePrefix,
                     revision)
import Setup (RpmFlags (..))
import SysCmd (cmd_, cmdBool, cmdIgnoreErr, (+-+))
import Control.Applicative ((<$>))
import Control.Monad (when)
import Distribution.PackageDescription (PackageDescription (..))
import Distribution.Simple.Utils (die)
import Data.Maybe (fromMaybe)
import System.Directory (copyFile, createDirectory, getCurrentDirectory,
                         setCurrentDirectory)
import System.FilePath ((</>))

update :: PackageData -> RpmFlags -> IO ()
update pkgdata flags =
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      let pkgDesc = packageDesc pkgdata
          pkg = package pkgDesc
          name = packageName pkg
          ver = packageVersion pkg
          current = name ++ "-" ++ ver
      latest <- latestPkg name
      if current == latest
        then error $ current +-+ "is latest version."
        else do
        bringTarball latest
        gitDir <- getCurrentDirectory >>= isGitDir
        rwGit <- if gitDir then cmdBool "grep -q 'url = ssh://' .git/config" else return False
        when rwGit $
          cmd_ "fedpkg" ["new-sources", latest ++ ".tar.gz"]
        withTempDirectory $ \cwd -> do
          (curspec, _) <- createSpecVersion current spec cwd
          (newspec, revised) <- createSpecVersion latest spec cwd
          patch <- cmdIgnoreErr "diff" ["-u2", "-I - spec file generated by cabal-rpm", "-I Fedora Haskell SIG <haskell@lists.fedoraproject.org>", curspec, newspec] ""
          putStrLn patch
          out <- cmdIgnoreErr "patch" ["-d", cwd, "-p1" ] patch
          putStrLn out
          setCurrentDirectory cwd
          distro <- fromMaybe detectDistro (return <$> rpmDistribution flags)
          let suffix = if distro == SUSE then "" else "%{?dist}"
          cmd_ "sed" ["-i", "-e s/^\\(Release:        \\).*/\\10" ++ suffix ++ "/", spec]
          let newver = removePrefix (name ++ "-") latest
          if distro == SUSE
            then cmd_ "sed" ["-i", "-e s/^\\(Version:        \\).*/\\1" ++ newver ++ "/", spec]
            else cmd_ "rpmdev-bumpspec" ["-c", "update to" +-+ newver, spec]
          when rwGit $ do
            when revised $
              cmd_ "git" ["add", name ++ ".cabal"]
            cmd_ "git" ["commit", "-a", "-m", "update to" +-+ newver]
  where
    createSpecVersion :: String -> String -> FilePath -> IO (FilePath, Bool)
    createSpecVersion pkgver spec topdir = do
      pkgdata' <- prepare (Just pkgver) flags
      let pkgDesc = packageDesc pkgdata'
          revised = revision pkgDesc
      when revised $ do
        let cabalPath = pkgver </> cabalFilename pkgdata'
            name = packageName $ package pkgDesc
            cabalfile = topdir </> name ++ ".cabal"
        copyFile cabalPath cabalfile
      let pkgdata'' = pkgdata' { specFilename = Just spec }
      createDirectory pkgver
      newspec <- createSpecFile pkgdata'' flags (Just pkgver)
      return (newspec, revised)
