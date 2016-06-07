module Paths_ayesod (
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
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ubuntu/workspace/ayesod/.stack-work/install/x86_64-linux/lts-5.18/7.10.3/bin"
libdir     = "/home/ubuntu/workspace/ayesod/.stack-work/install/x86_64-linux/lts-5.18/7.10.3/lib/x86_64-linux-ghc-7.10.3/ayesod-0.0.0-DkZkMMw3zri1cw7A3VoKqM"
datadir    = "/home/ubuntu/workspace/ayesod/.stack-work/install/x86_64-linux/lts-5.18/7.10.3/share/x86_64-linux-ghc-7.10.3/ayesod-0.0.0"
libexecdir = "/home/ubuntu/workspace/ayesod/.stack-work/install/x86_64-linux/lts-5.18/7.10.3/libexec"
sysconfdir = "/home/ubuntu/workspace/ayesod/.stack-work/install/x86_64-linux/lts-5.18/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ayesod_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ayesod_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ayesod_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ayesod_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ayesod_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
