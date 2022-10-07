{-# LANGUAGE TemplateHaskell #-}

-- | Provides the 'Command' type.
--
-- @since 0.1
module SafeRm.Runner.Command
  ( Command (..),
    _Delete,
    _DeletePerm,
    _Empty,
    _Restore,
    _List,
    _Metadata,
  )
where

import Optics.TH (makePrisms)
import SafeRm.Data.Paths (PathI, PathIndex (OriginalPath, TrashName))
import SafeRm.Prelude

-- | Action to run.
--
-- @since 0.1
data Command
  = -- | Deletes a path.
    --
    -- @since 0.1
    Delete !(NonEmpty (PathI OriginalPath))
  | -- | Permanently deletes a path from the trash.
    --
    -- @since 0.1
    DeletePerm
      !Bool
      !(NonEmpty (PathI TrashName))
  | -- | Empties the trash.
    --
    -- @since 0.1
    Empty !Bool
  | -- | Restores a path.
    --
    -- @since 0.1
    Restore (NonEmpty (PathI TrashName))
  | -- | List all trash contents.
    --
    -- @since 0.1
    List
  | -- | Prints trash metadata.
    --
    -- @since 0.1
    Metadata
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

makePrisms ''Command
