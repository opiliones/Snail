{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_snale (
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

bindir     = "/home/tks/Snale/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/bin"
libdir     = "/home/tks/Snale/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/lib/x86_64-linux-ghc-8.4.4/snale-0.1.0.0-L0Exq3hUWYA2PScQIREQWH"
dynlibdir  = "/home/tks/Snale/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/tks/Snale/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/share/x86_64-linux-ghc-8.4.4/snale-0.1.0.0"
libexecdir = "/home/tks/Snale/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/libexec/x86_64-linux-ghc-8.4.4/snale-0.1.0.0"
sysconfdir = "/home/tks/Snale/.stack-work/install/x86_64-linux/lts-12.16/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "snale_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snale_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "snale_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "snale_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snale_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "snale_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
