module IO (
  filesAndFolders
) where

{-- TODO: Take a look at System.FilePath.Find instead. --}

import qualified Control.Monad as Monad
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

type File = String
type Folder = String

filesAndFolders :: FilePath -> IO ([File], [Folder])
filesAndFolders initialDirectory = do
  names <- Directory.getDirectoryContents initialDirectory
  Monad.foldM (partition initialDirectory) ([], []) names

partition :: FilePath -> ([File], [Folder]) -> FilePath -> IO ([File], [Folder])
partition path (files, folders) name =
  let
    filePath = FilePath.joinPath([path, name])
  in
    case name of
      "."   -> return (files, folders)
      ".."  -> return (files, folders)
      _     -> do
        exists <- Directory.doesDirectoryExist filePath
        return $ if exists
          then (files, name : folders)
          else (name : files, folders)

