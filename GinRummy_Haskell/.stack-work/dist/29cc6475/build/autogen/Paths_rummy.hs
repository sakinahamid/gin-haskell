{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_rummy (
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

bindir     = "C:\\Users\\Sakinah Hamid\\Documents\\Units\\FIT2102\\2020Assignment2_1023_1831\\.stack-work\\install\\cdba748b\\bin"
libdir     = "C:\\Users\\Sakinah Hamid\\Documents\\Units\\FIT2102\\2020Assignment2_1023_1831\\.stack-work\\install\\cdba748b\\lib\\x86_64-windows-ghc-8.8.2\\rummy-0.1.0.0-1pFtqJ27uFSHcahoiT5L6v"
dynlibdir  = "C:\\Users\\Sakinah Hamid\\Documents\\Units\\FIT2102\\2020Assignment2_1023_1831\\.stack-work\\install\\cdba748b\\lib\\x86_64-windows-ghc-8.8.2"
datadir    = "C:\\Users\\Sakinah Hamid\\Documents\\Units\\FIT2102\\2020Assignment2_1023_1831\\.stack-work\\install\\cdba748b\\share\\x86_64-windows-ghc-8.8.2\\rummy-0.1.0.0"
libexecdir = "C:\\Users\\Sakinah Hamid\\Documents\\Units\\FIT2102\\2020Assignment2_1023_1831\\.stack-work\\install\\cdba748b\\libexec\\x86_64-windows-ghc-8.8.2\\rummy-0.1.0.0"
sysconfdir = "C:\\Users\\Sakinah Hamid\\Documents\\Units\\FIT2102\\2020Assignment2_1023_1831\\.stack-work\\install\\cdba748b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rummy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rummy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "rummy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "rummy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rummy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rummy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
