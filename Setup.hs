{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.IORef
import Data.List ( nub )
import Data.Version ( showVersion )
import Distribution.Package ( PackageName(PackageName), PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..), hsSourceDirs)
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, ComponentLocalBuildInfo(), LocalBuildInfo(), componentPackageDeps )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag, buildDistPref, defaultDistPref, fromFlagOrDefault )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Verbosity ( Verbosity )
import System.Directory ( canonicalizePath )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi flags
     buildHook simpleUserHooks pkg lbi hooks flags
  }

--  Very ad-hoc implementation of difference lists
singletonDL :: a -> [a] -> [a]
singletonDL = (:)

emptyDL :: [a] -> [a]
emptyDL = id

listDL :: [a] -> [a] -> [a]
listDL = foldl (\acc x -> acc `appendDL` singletonDL x) emptyDL

appendDL :: ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a]
appendDL x y = x . y

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> BuildFlags -> IO ()
generateBuildModule verbosity pkg lbi flags = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withTestLBI pkg lbi $ \suite suitelbi -> do
    srcDirs <- mapM canonicalizePath $ hsSourceDirs $ testBuildInfo suite
    distDir <- canonicalizePath $ fromFlagOrDefault defaultDistPref $ buildDistPref flags
    contents <- newIORef $ listDL
      [ "module Build_" ++ map fixchar (testName suite) ++ " where"
      , "getDistDir :: FilePath"
      , "getDistDir = " ++ show distDir
      , "getSrcDirs :: [FilePath]"
      , "getSrcDirs = " ++ show srcDirs
      ]
    withLibLBI pkg lbi $ \_ liblbi ->
      modifyIORef contents (appendDL $ depsDef liblbi suitelbi)
    contents' <- fmap ($ []) $ readIORef contents
    rewriteFile (map fixchar $ dir </> "Build_" ++ testName suite ++ ".hs") $ unlines contents'
  where
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)
    depsDef librarylbi suitelbi = listDL
      [ "deps :: [String]"
      , "deps = " ++ show (formatdeps $ testDeps librarylbi suitelbi)
      ]
    fixchar '-' = '_'
    fixchar c = c

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys
