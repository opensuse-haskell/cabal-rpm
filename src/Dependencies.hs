-- |
-- Module      :  Dependencies
-- Copyright   :  (C) 2012-2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Dependency info

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Dependencies (
  dependencies,
  missingPackages,
  notInstalled,
  packageDependencies,
  showDep,
  testsuiteDependencies
  ) where

import PackageUtils (packageName)
import SysCmd (cmdBool, repoquery, (+-+))

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Data.List (delete, nub)
import Distribution.Package  (Dependency (..), PackageName (..))
import Distribution.PackageDescription (PackageDescription (..),
                                        allBuildInfo,
                                        BuildInfo (..),
                                        TestSuite (..),
                                        hasExes,
                                        setupDepends)

excludedPkgs :: String -> Bool
excludedPkgs = flip notElem ["Cabal", "base", "ghc-prim", "integer-gmp"]

-- returns list of deps and whether package is self-dependent
buildDependencies :: PackageDescription -> String -> ([String], Bool)
buildDependencies pkgDesc self =
  let bdeps = map depName (buildDepends pkgDesc)
      sdeps = maybe [] (map depName . setupDepends) (setupBuildInfo pkgDesc)
      deps  = nub $ bdeps ++ sdeps
  in
    (filter excludedPkgs (delete self deps), self `elem` deps && hasExes pkgDesc)

depName :: Dependency -> String
depName (Dependency (PackageName n) _) = n

showDep :: String -> String
showDep p = "ghc-" ++ p ++ "-devel"

dependencies :: PackageDescription  -- ^pkg description
                -> ([String], [String], [String], [String], Bool)
                -- ^depends, tools, c-libs, pkgcfg, selfdep
dependencies pkgDesc = (deps, tools, nub clibs, pkgcfgs, selfdep)
  where
    self = packageName $ package pkgDesc
    (deps, selfdep) = buildDependencies pkgDesc self
    buildinfo = allBuildInfo pkgDesc
    tools =  nub $ map depName (concatMap buildTools buildinfo)
    pkgcfgs = nub $ map depName $ concatMap pkgconfigDepends buildinfo
    clibs = concatMap extraLibs buildinfo



resolveLib :: String -> String
resolveLib "adns" = "libadns-devel"
resolveLib "alut" = "freealut-devel"
resolveLib "asound" = "alsa-devel"
resolveLib "blas" = "blas-devel"
resolveLib "bz2" = "libbz2-devel"
resolveLib "clang" = "clang-devel"
resolveLib "crypt" = "glibc-devel"
resolveLib "crypto" = "libopenssl-devel"
resolveLib "curl" = "libcurl-devel"
resolveLib "expat" = "libexpat-devel"
resolveLib "ffi" = "libffi-devel-gcc5"
resolveLib "fftw3" = "fftw3-devel"
resolveLib "fontconfig" = "fontconfig-devel"
resolveLib "freetype" = "freetype2-devel"
resolveLib "gd" = "gd-devel"
resolveLib "GeoIP" = "libGeoIP-devel"
resolveLib "GL" = "Mesa-libGL-devel"
resolveLib "GLU" = "glu-devel"
resolveLib "gmp" = "gmp-devel"
resolveLib "gnutls" = "libgnutls-devel"
resolveLib "icudata" = "libicu-devel"
resolveLib "icui18n" = "libicu-devel"
resolveLib "icuuc" = "libicu-devel"
resolveLib "idn" = "libidn-devel"
resolveLib "IL" = "DevIL-devel"
resolveLib "iw" = "libiw-devel"
resolveLib "jpeg" = "libjpeg62-devel"
resolveLib "lapack" = "lapack-devel"
resolveLib "leveldb" = "leveldb-devel"
resolveLib "lmdb" = "lmdb-devel"
resolveLib "lua" = "lua-devel"
resolveLib "lzma" = "xz-devel"
resolveLib "m" = "glibc-devel"
resolveLib "magic" = "file-devel"
resolveLib "markdown" = "libmarkdown-devel"
resolveLib "mpfr" = "mpfr-devel"
resolveLib "notify" = "libnotify-devel"
resolveLib "openal" = "openal-soft-devel"
resolveLib "pcre" = "pcre-devel"
resolveLib "png" = "libpng16-compat-devel"
resolveLib "pq" = "postgresql94-devel"
resolveLib "pthread" = "glibc-devel"
resolveLib "resolv" = "glibc-devel"
resolveLib "ruby" = "ruby2.2-devel"
resolveLib "sass" = "libsass-devel"
resolveLib "SDL" = "libSDL-devel"
resolveLib "SDL2" = "libSDL2-devel"
resolveLib "SDL_image" = "libSDL_image-devel"
resolveLib "SDL_mixer" = "libSDL_mixer-devel"
resolveLib "sndfile" = "libsndfile-devel"
resolveLib "sqlite3" = "sqlite3-devel"
resolveLib "ssl" = "libopenssl-devel"
resolveLib "stdc++" = "libstdc++-devel"
resolveLib "tag_c" = "libtag-devel"
resolveLib "udev" = "libudev-devel"
resolveLib "X11" = "libX11-devel"
resolveLib "Xcursor" = "libXcursor-devel"
resolveLib "Xext" = "libXext-devel"
resolveLib "Xi" = "libXi-devel"
resolveLib "Xinerama" = "libXinerama-devel"
resolveLib "xml2" = "libxml2-devel"
resolveLib "Xpm" = "libXpm-devel"
resolveLib "Xrandr" = "libXrandr-devel"
resolveLib "Xrender" = "libXrender-devel"
resolveLib "Xss" = "libXss-devel"
resolveLib "Xxf86vm" = "libXxf86vm-devel"
resolveLib "z" = "zlib-devel"
resolveLib s = error ("resolveLib: cannot map library name " ++ show s ++ " to package name")

packageDependencies :: PackageDescription  -- ^pkg description
                    -> ([String], [String], [String], [String], Bool)
                    -- ^depends, tools, c-libs, pkgcfg, selfdep
packageDependencies pkgDesc = (map showDep deps, tools, nub clibs, map showPkgCfg pkgcfgs, selfdep)
  where
    (deps, tools', clibs', pkgcfgs, selfdep) = dependencies pkgDesc
    excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
    mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
    mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
    mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
    mapTools tool = tool
    chrpath = ["chrpath" | selfdep]
    tools = filter excludedTools $ nub $ map mapTools tools' ++ chrpath
    clibs = map resolveLib clibs'
    showPkgCfg p = "pkgconfig(" ++ p ++ ")"

testsuiteDependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^self
                -> [String]         -- ^depends
testsuiteDependencies pkgDesc self =
  map showDep . delete self . filter excludedPkgs . nub . map depName $ concatMap (targetBuildDepends . testBuildInfo) (testSuites pkgDesc)

missingPackages :: PackageDescription -> IO [String]
missingPackages pkgDesc = do
  let (deps, tools, clibs, pkgcfgs, _) = packageDependencies pkgDesc
  pcpkgs <- mapM derefPkg pkgcfgs
  filterM notInstalled $ deps ++ ["ghc-Cabal-devel", "ghc-rpm-macros"] ++ tools ++ clibs ++ pcpkgs

notInstalled :: String -> IO Bool
notInstalled dep =
  fmap not $ cmdBool $ "rpm -q --whatprovides" +-+ shellQuote dep
  where
    shellQuote :: String -> String
    shellQuote (c:cs) = (if c `elem` "()" then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""

derefPkg :: String -> IO String
derefPkg req = do
  res <- singleLine <$> repoquery ["--qf", "%{name}", "--whatprovides"] req
  if null res
    then error $ req +-+ "provider not found by repoquery"
    else return res
  where
    singleLine :: String -> String
    singleLine "" = ""
    singleLine s = (head . lines) s
