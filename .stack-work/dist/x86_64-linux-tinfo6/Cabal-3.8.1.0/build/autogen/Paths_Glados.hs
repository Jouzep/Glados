{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Glados (
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
bindir     = "/root/Epitech/TEK3/B-FUN-500-PAR-5-2-glados-joseph.yu/.stack-work/install/x86_64-linux-tinfo6/9040aa716f74fc4ba4b192ae6da3bc0e6ed559bf631b3cefceaa86748b4f9bf6/9.4.8/bin"
libdir     = "/root/Epitech/TEK3/B-FUN-500-PAR-5-2-glados-joseph.yu/.stack-work/install/x86_64-linux-tinfo6/9040aa716f74fc4ba4b192ae6da3bc0e6ed559bf631b3cefceaa86748b4f9bf6/9.4.8/lib/x86_64-linux-ghc-9.4.8/Glados-0.1.0.0-29W5hMCvwrDJyVPIONzq0T"
dynlibdir  = "/root/Epitech/TEK3/B-FUN-500-PAR-5-2-glados-joseph.yu/.stack-work/install/x86_64-linux-tinfo6/9040aa716f74fc4ba4b192ae6da3bc0e6ed559bf631b3cefceaa86748b4f9bf6/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/root/Epitech/TEK3/B-FUN-500-PAR-5-2-glados-joseph.yu/.stack-work/install/x86_64-linux-tinfo6/9040aa716f74fc4ba4b192ae6da3bc0e6ed559bf631b3cefceaa86748b4f9bf6/9.4.8/share/x86_64-linux-ghc-9.4.8/Glados-0.1.0.0"
libexecdir = "/root/Epitech/TEK3/B-FUN-500-PAR-5-2-glados-joseph.yu/.stack-work/install/x86_64-linux-tinfo6/9040aa716f74fc4ba4b192ae6da3bc0e6ed559bf631b3cefceaa86748b4f9bf6/9.4.8/libexec/x86_64-linux-ghc-9.4.8/Glados-0.1.0.0"
sysconfdir = "/root/Epitech/TEK3/B-FUN-500-PAR-5-2-glados-joseph.yu/.stack-work/install/x86_64-linux-tinfo6/9040aa716f74fc4ba4b192ae6da3bc0e6ed559bf631b3cefceaa86748b4f9bf6/9.4.8/etc"

getBinDir     = catchIO (getEnv "Glados_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Glados_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Glados_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Glados_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Glados_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Glados_sysconfdir") (\_ -> return sysconfdir)




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
