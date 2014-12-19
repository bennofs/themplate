{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Error
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Configurator
import           Data.Configurator.Types(Config())
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative
import           Pattern
import           Prelude hiding (lookup)
import           System.Directory
import           System.Exit
import           System.FilePath

-- | This data type represents the result of parsing the command line options.
data Cmd = Init String String Bool
         | List

-- | Command line argument parser for the 'init' subcommand. The init command can be used to instantiate templates.
initCmd :: ParserInfo Cmd
initCmd = info (helper <*> parser) (fullDesc <> progDesc "Use an installed template.")
  where nameOpt = strArgument (metavar "NAME" <> help "The project name")
        templatesOpt = strArgument (metavar "TEMPLATE" <> help "Name of the template which to use for this project")
        amendOpt = switch (short 'a' <> long "amend" <> help "Don't fail when a file exists. Instead, ignore it and continue.")
        parser = Init <$> nameOpt <*> templatesOpt <*> amendOpt

-- | Command line argument parser for the 'list' subcommand. The list subcommand prints all installed templates.
listCmd :: ParserInfo Cmd
listCmd = info (helper <*> pure List) (briefDesc <> progDesc "List all installed templates")

-- | Top level command line argument parser. Aggregates all subcommands.
themplateCmds :: Parser Cmd
themplateCmds = subparser $ mconcat
  [ command "init"      initCmd
  , command "list"      listCmd
  ]

-- | The parser info for this program. Specifies help and other information.
optsInfo :: ParserInfo Cmd
optsInfo = info (helper <*> themplateCmds) $ mconcat
  [ fullDesc
  , progDesc "Use project patterns to quickly create a new project"
  ]

-- | Get a list of all available templates in the given directory. The list of available templates
-- is just the list of all directories under the given path.
availableTemplates :: String -> IO [String]
availableTemplates appData = getDirectoryContents appData >>= filterM f
  where f x
          | x `elem` [".",".."] = return False
          | otherwise = doesDirectoryExist $ appData </> x

-- | Instantiate a template, copying the files and resolving all conditionals and variables.
-- The first argument is the project name, the second argument the directory of the template and the third argument is
-- the target directory, to which the files get copied.
instantiateTemplate :: String -> Bool -> Config -> FilePath -> FilePath -> EitherT T.Text IO ()
instantiateTemplate proj amend c temp target = do
  contents <- lift $ filter (not . flip elem [".",".."]) <$> getDirectoryContents temp
  dirs <- lift $ filterM (doesDirectoryExist . (temp </>)) contents
  files <- lift $ filterM (doesFileExist . (temp </>)) contents

  forM_ files $ \file -> do
    let lookupVar "project.name" = return $ Just $ T.pack proj
        lookupVar x = lookup c x
    let substitute = fmapLT (mappend $ T.pack file <> ":Error: Undeclared variable ") . substituteBetween "{{" "}}" (evalPattern lookupVar)

    content <- lift $ T.readFile (temp </> file)
    content' <- substitute content
    file' <- fmap T.unpack $ substitute $ T.pack file

    e <- lift $ doesFileExist (target </> file')
    when (e && not amend) $ left $ ":Error: Target file " <> T.pack (target </> file') <> " does already exist."
    unless e $ lift $ do
      T.writeFile (target </> file') content'
      perm <- getPermissions (temp </> file)
      perm' <- getPermissions (target </> file')
      setPermissions (target </> file') $ perm' { executable = executable perm }

  forM_ dirs $ \dir -> do
    lift $ createDirectoryIfMissing True $ target </> dir
    instantiateTemplate proj amend c (temp </> dir) (target </> dir)

-- | Copy the contents of a directory to another directory
copyDirContents :: String -> String -> IO ()
copyDirContents source target = do
  contents <- filter (not . flip elem ["..","."]) <$> getDirectoryContents source
  dirs <- filterM (doesDirectoryExist . (source </>)) contents
  files <- filterM (doesFileExist . (source </>)) contents
  forM_ files $ \file -> copyFile (source </> file) (target </> file)
  forM_ dirs $ \dir -> do
    createDirectoryIfMissing False (target </> dir)
    copyDirContents (source </> dir) (target </> dir)

-- | Top level subcommand handler. Switches on the subcommand and performs the given action.
-- Takes 3 arguments: The configuration, the path to the appUserDataDirectory and the command.
handleCmd :: Config -> String -> Cmd -> IO ()
handleCmd _ appData List = mapM_ putStrLn =<< availableTemplates appData
handleCmd c appData (Init proj temp amend) = do
  e <- doesDirectoryExist (appData </> temp)
  unless e $ do
    putStrLn $ "Error: Template " <> temp <> " is not installed."
    exitFailure
  targetExists <- doesDirectoryExist proj
  unless targetExists $ createDirectory proj
  projPath <- canonicalizePath proj
  res <- runEitherT (instantiateTemplate proj amend c (appData </> temp) projPath) `E.catch` \(exc :: E.SomeException) ->
    unless targetExists (removeDirectoryRecursive projPath) >> E.throwIO exc
  case res of
    Left m -> do
      T.putStrLn m
      unless targetExists $ removeDirectoryRecursive projPath
      exitFailure
    Right () -> exitSuccess

main :: IO ()
main = do
  cmd <- execParser optsInfo
  appData <- getAppUserDataDirectory "themplate"
  createDirectoryIfMissing True appData
  config <- load [Required $ appData </> "config.cfg"]
  handleCmd config appData cmd
