{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_graphAlgorithms (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/enzo/prog/haskell/graphAlgorithms/.cabal-sandbox/bin"
libdir     = "/home/enzo/prog/haskell/graphAlgorithms/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/graphAlgorithms-0.1.0.0"
dynlibdir  = "/home/enzo/prog/haskell/graphAlgorithms/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/enzo/prog/haskell/graphAlgorithms/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/graphAlgorithms-0.1.0.0"
libexecdir = "/home/enzo/prog/haskell/graphAlgorithms/.cabal-sandbox/libexec"
sysconfdir = "/home/enzo/prog/haskell/graphAlgorithms/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "graphAlgorithms_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "graphAlgorithms_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "graphAlgorithms_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "graphAlgorithms_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "graphAlgorithms_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "graphAlgorithms_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
