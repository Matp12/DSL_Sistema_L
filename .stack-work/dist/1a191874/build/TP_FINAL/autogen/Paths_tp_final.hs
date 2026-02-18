{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tp_final (
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
bindir     = "C:\\Users\\mateo\\OneDrive\\Escritorio\\LCC\\LCC_6to_cuatri\\AnalisisLenguajesProg\\TP_FINAL\\.stack-work\\install\\6135d9ec\\bin"
libdir     = "C:\\Users\\mateo\\OneDrive\\Escritorio\\LCC\\LCC_6to_cuatri\\AnalisisLenguajesProg\\TP_FINAL\\.stack-work\\install\\6135d9ec\\lib\\x86_64-windows-ghc-9.10.3-b42a\\tp-final-0.1.0.0-5D0T16CMZbS9RemsBN9ed2-TP_FINAL"
dynlibdir  = "C:\\Users\\mateo\\OneDrive\\Escritorio\\LCC\\LCC_6to_cuatri\\AnalisisLenguajesProg\\TP_FINAL\\.stack-work\\install\\6135d9ec\\lib\\x86_64-windows-ghc-9.10.3-b42a"
datadir    = "C:\\Users\\mateo\\OneDrive\\Escritorio\\LCC\\LCC_6to_cuatri\\AnalisisLenguajesProg\\TP_FINAL\\.stack-work\\install\\6135d9ec\\share\\x86_64-windows-ghc-9.10.3-b42a\\tp-final-0.1.0.0"
libexecdir = "C:\\Users\\mateo\\OneDrive\\Escritorio\\LCC\\LCC_6to_cuatri\\AnalisisLenguajesProg\\TP_FINAL\\.stack-work\\install\\6135d9ec\\libexec\\x86_64-windows-ghc-9.10.3-b42a\\tp-final-0.1.0.0"
sysconfdir = "C:\\Users\\mateo\\OneDrive\\Escritorio\\LCC\\LCC_6to_cuatri\\AnalisisLenguajesProg\\TP_FINAL\\.stack-work\\install\\6135d9ec\\etc"

getBinDir     = catchIO (getEnv "tp_final_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tp_final_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tp_final_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tp_final_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tp_final_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tp_final_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
