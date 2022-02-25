{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_morse (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/kannaaa3/.cabal/bin"
libdir     = "/home/kannaaa3/.cabal/lib/x86_64-linux-ghc-8.10.7/morse-0.1.0.0-inplace-morse"
dynlibdir  = "/home/kannaaa3/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/kannaaa3/.cabal/share/x86_64-linux-ghc-8.10.7/morse-0.1.0.0"
libexecdir = "/home/kannaaa3/.cabal/libexec/x86_64-linux-ghc-8.10.7/morse-0.1.0.0"
sysconfdir = "/home/kannaaa3/.cabal/etc"

getBinDir     = catchIO (getEnv "morse_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "morse_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "morse_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "morse_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "morse_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "morse_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
