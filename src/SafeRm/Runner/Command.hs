{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' type.
--
-- @since 0.1
module SafeRm.Runner.Command
  ( -- * Types
    Command (..),

    -- * Optics
    _Delete,
    _DeletePerm,
    _Empty,
    _Restore,
    _List,
    _Metadata,
  )
where

import SafeRm.Data.Paths (PathI, PathIndex (OriginalPath, TrashName))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Prelude
import SafeRm.Runner.Command.List (ListCmd)
import SafeRm.Runner.Stage (AdvanceStage (..), Stage (..))

instance AdvanceStage (Command Stage1) where
  type NextStage (Command Stage1) = Command Stage2

  advanceStage (Delete paths) = Delete paths
  advanceStage (DeletePerm force paths) = DeletePerm force paths
  advanceStage (Empty b) = Empty b
  advanceStage (Restore paths) = Restore paths
  advanceStage Metadata = Metadata
  advanceStage (List cfg) = List $ advanceStage cfg

-- | Action to run.
--
-- @since 0.1
type Command :: Stage -> Type
data Command s
  = -- | Deletes a path.
    --
    -- @since 0.1
    Delete !(UniqueSeq (PathI OriginalPath))
  | -- | Permanently deletes a path from the trash.
    --
    -- @since 0.1
    DeletePerm
      !Bool
      !(UniqueSeq (PathI TrashName))
  | -- | Empties the trash.
    --
    -- @since 0.1
    Empty !Bool
  | -- | Restores a path.
    --
    -- @since 0.1
    Restore (UniqueSeq (PathI TrashName))
  | -- | List all trash contents.
    --
    -- @since 0.1
    List !(ListCmd s)
  | -- | Prints trash metadata.
    --
    -- @since 0.1
    Metadata

-- | @since 0.1
makePrisms ''Command

-- | @since 0.1
deriving stock instance Eq (Command Stage1)

-- | @since 0.1
deriving stock instance Show (Command Stage1)

-- | @since 0.1
deriving stock instance Eq (Command Stage2)

-- | @since 0.1
deriving stock instance Show (Command Stage2)
