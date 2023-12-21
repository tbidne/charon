{-# LANGUAGE QuasiQuotes #-}

-- | Exceptions specific to the default trash backend.
module Charon.Backend.Default.Exception
  ( TrashDirFilesNotFoundE (..),
    TrashDirInfoNotFoundE (..),
    pathFiles,
    pathInfo,
  )
where

import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashHome
      ),
  )
import Charon.Prelude

-- | Trash path dir not found error.
newtype TrashDirFilesNotFoundE = MkTrashDirFilesNotFoundE (PathI TrashHome)
  deriving stock (Show)

instance Exception TrashDirFilesNotFoundE where
  displayException (MkTrashDirFilesNotFoundE (MkPathI th)) =
    mconcat
      [ "The trash files directory was not found at '",
        decodeOsToFpDisplayEx files,
        "' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. charon empty -f)."
      ]
    where
      files = th </> pathFiles

-- | Trash info dir not found error.
newtype TrashDirInfoNotFoundE = MkTrashDirInfoNotFoundE (PathI TrashHome)
  deriving stock (Show)

instance Exception TrashDirInfoNotFoundE where
  displayException (MkTrashDirInfoNotFoundE (MkPathI th)) =
    mconcat
      [ "The trash info directory was not found at '",
        decodeOsToFpDisplayEx info,
        "' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. charon empty -f)."
      ]
    where
      info = th </> pathInfo

pathFiles :: OsPath
pathFiles = [osp|files|]

pathInfo :: OsPath
pathInfo = [osp|info|]
