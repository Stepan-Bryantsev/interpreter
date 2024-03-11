{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_s_interpreter (
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
bindir     = "/Users/stepanmac/McMaster/Generative programming/project/s-interpreter/.stack-work/install/aarch64-osx/0ed32a198390936c7d0906da1281b45fc164716575eeb0c94ed91e412912513e/9.6.4/bin"
libdir     = "/Users/stepanmac/McMaster/Generative programming/project/s-interpreter/.stack-work/install/aarch64-osx/0ed32a198390936c7d0906da1281b45fc164716575eeb0c94ed91e412912513e/9.6.4/lib/aarch64-osx-ghc-9.6.4/s-interpreter-0.1.0.0-KllsoN5vXoHDJw2JXlYInq-s-interpreter-lib"
dynlibdir  = "/Users/stepanmac/McMaster/Generative programming/project/s-interpreter/.stack-work/install/aarch64-osx/0ed32a198390936c7d0906da1281b45fc164716575eeb0c94ed91e412912513e/9.6.4/lib/aarch64-osx-ghc-9.6.4"
datadir    = "/Users/stepanmac/McMaster/Generative programming/project/s-interpreter/.stack-work/install/aarch64-osx/0ed32a198390936c7d0906da1281b45fc164716575eeb0c94ed91e412912513e/9.6.4/share/aarch64-osx-ghc-9.6.4/s-interpreter-0.1.0.0"
libexecdir = "/Users/stepanmac/McMaster/Generative programming/project/s-interpreter/.stack-work/install/aarch64-osx/0ed32a198390936c7d0906da1281b45fc164716575eeb0c94ed91e412912513e/9.6.4/libexec/aarch64-osx-ghc-9.6.4/s-interpreter-0.1.0.0"
sysconfdir = "/Users/stepanmac/McMaster/Generative programming/project/s-interpreter/.stack-work/install/aarch64-osx/0ed32a198390936c7d0906da1281b45fc164716575eeb0c94ed91e412912513e/9.6.4/etc"

getBinDir     = catchIO (getEnv "s_interpreter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "s_interpreter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "s_interpreter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "s_interpreter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "s_interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "s_interpreter_sysconfdir") (\_ -> return sysconfdir)



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
