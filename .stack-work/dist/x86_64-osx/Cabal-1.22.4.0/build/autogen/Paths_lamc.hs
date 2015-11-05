module Paths_lamc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dylan/Code/lamc/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/bin"
libdir     = "/Users/dylan/Code/lamc/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/lib/x86_64-osx-ghc-7.10.2/lamc-0.1.0.0-5eVrUU1K4dW8f168hbG4XR"
datadir    = "/Users/dylan/Code/lamc/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/share/x86_64-osx-ghc-7.10.2/lamc-0.1.0.0"
libexecdir = "/Users/dylan/Code/lamc/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/libexec"
sysconfdir = "/Users/dylan/Code/lamc/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lamc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lamc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lamc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lamc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lamc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
