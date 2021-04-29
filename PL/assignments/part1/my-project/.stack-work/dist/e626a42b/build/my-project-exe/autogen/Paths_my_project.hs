{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_my_project (
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

bindir     = "C:\\Users\\nuko\\5sem\\4sem\\PL\\assignments\\part1\\my-project\\.stack-work\\install\\fabae29d\\bin"
libdir     = "C:\\Users\\nuko\\5sem\\4sem\\PL\\assignments\\part1\\my-project\\.stack-work\\install\\fabae29d\\lib\\x86_64-windows-ghc-8.6.5\\my-project-0.1.0.0-9LgkfSHUZcMLMG5hatuVR8-my-project-exe"
dynlibdir  = "C:\\Users\\nuko\\5sem\\4sem\\PL\\assignments\\part1\\my-project\\.stack-work\\install\\fabae29d\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\nuko\\5sem\\4sem\\PL\\assignments\\part1\\my-project\\.stack-work\\install\\fabae29d\\share\\x86_64-windows-ghc-8.6.5\\my-project-0.1.0.0"
libexecdir = "C:\\Users\\nuko\\5sem\\4sem\\PL\\assignments\\part1\\my-project\\.stack-work\\install\\fabae29d\\libexec\\x86_64-windows-ghc-8.6.5\\my-project-0.1.0.0"
sysconfdir = "C:\\Users\\nuko\\5sem\\4sem\\PL\\assignments\\part1\\my-project\\.stack-work\\install\\fabae29d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "my_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "my_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "my_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "my_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "my_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
