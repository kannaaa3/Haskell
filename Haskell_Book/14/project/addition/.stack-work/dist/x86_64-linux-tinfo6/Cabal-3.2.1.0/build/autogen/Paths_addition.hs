{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_addition (
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

bindir     = "/home/kannaaa3/Documents/Code/Haskell/Haskell Book/14/project/addition/.stack-work/install/x86_64-linux-tinfo6/f16897bfe2974301d2496eb3ef99cc115c9bd97d7d23f83c3e21462786e84736/8.10.7/bin"
libdir     = "/home/kannaaa3/Documents/Code/Haskell/Haskell Book/14/project/addition/.stack-work/install/x86_64-linux-tinfo6/f16897bfe2974301d2496eb3ef99cc115c9bd97d7d23f83c3e21462786e84736/8.10.7/lib/x86_64-linux-ghc-8.10.7/addition-0.1.0.0-hhJs8Yinl3B6XoEkOf9Kn"
dynlibdir  = "/home/kannaaa3/Documents/Code/Haskell/Haskell Book/14/project/addition/.stack-work/install/x86_64-linux-tinfo6/f16897bfe2974301d2496eb3ef99cc115c9bd97d7d23f83c3e21462786e84736/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/kannaaa3/Documents/Code/Haskell/Haskell Book/14/project/addition/.stack-work/install/x86_64-linux-tinfo6/f16897bfe2974301d2496eb3ef99cc115c9bd97d7d23f83c3e21462786e84736/8.10.7/share/x86_64-linux-ghc-8.10.7/addition-0.1.0.0"
libexecdir = "/home/kannaaa3/Documents/Code/Haskell/Haskell Book/14/project/addition/.stack-work/install/x86_64-linux-tinfo6/f16897bfe2974301d2496eb3ef99cc115c9bd97d7d23f83c3e21462786e84736/8.10.7/libexec/x86_64-linux-ghc-8.10.7/addition-0.1.0.0"
sysconfdir = "/home/kannaaa3/Documents/Code/Haskell/Haskell Book/14/project/addition/.stack-work/install/x86_64-linux-tinfo6/f16897bfe2974301d2496eb3ef99cc115c9bd97d7d23f83c3e21462786e84736/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "addition_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "addition_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
