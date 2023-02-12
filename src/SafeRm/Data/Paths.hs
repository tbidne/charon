{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for distinguishing path types.
--
-- @since 0.1
module SafeRm.Data.Paths
  ( -- * Types
    PathI (..),
    PathIndex (..),

    -- * Functions

    -- ** Specific
    isRoot,
    isRoot',

    -- ** General
    -- $general
    showPaths,
    reindex,
    (<//>),
    (<//),
    (//>),
    applyPathI,
    liftPathI,
    liftPathI',
    liftPathIF,
    liftPathIF',
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import SafeRm.Prelude

-- | Types of filepaths used in SafeRm.
--
-- @since 0.1
data PathIndex
  = -- | The trash directory.
    --
    -- @since 0.1
    TrashHome
  | -- | The name corresponding to some file/directory in the trash directory.
    --
    -- @since 0.1
    TrashName
  | -- | The original path for some file/directory in the trash directory.
    --
    -- @since 0.1
    OriginalPath
  | -- | The full trash path i.e. @\<trash-home\>\/paths\/'\<trash-name\>@.
    --
    -- @since 0.1
    TrashPath
  | -- | The full trash info path i.e. @\<trash-home\>\/info\/'\<trash-name\>.info@.
    --
    -- @since 0.1
    TrashInfoPath
  | -- | The trash log file.
    --
    -- @since 0.1
    TrashLog
  | -- | The directory to the trash files themselves i.e. <trash>/paths.
    --
    -- @since 0.1
    TrashPathDir
  | -- | The directory to the trash info files i.e. <trash>/info.
    --
    -- @since 0.1
    TrashInfoDir

-- | Indexed 'FilePath' so that we can prevent mixing up different filepaths.
--
-- @since 0.1
type PathI :: PathIndex -> Type
newtype PathI (i :: PathIndex) = MkPathI
  { unPathI :: FilePath
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable,
      -- | @since 0.1
      NFData,
      -- | @since 0.1
      Serialise
    )
  deriving
    ( -- | @since 0.1
      IsString,
      -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup
    )
    via FilePath

makeFieldLabelsNoPrefix ''PathI

-- | Modifies the index.
--
-- @since 0.1
reindex :: PathI i -> PathI j
reindex = liftPathI id

-- | Lifts a 'FilePath' transformation to 'PathI', allowing for the index to
-- change.
--
-- @since 0.1
liftPathI :: (FilePath -> FilePath) -> PathI i -> PathI j
liftPathI f (MkPathI fp) = MkPathI (f fp)

-- | 'liftPathI' specialized to the same index. This should be preferred
-- as the former is easier to use incorrectly.
--
-- @since 0.1
liftPathI' :: (FilePath -> FilePath) -> PathI i -> PathI i
liftPathI' = liftPathI

-- | Lifts an effectful 'FilePath' transformation to 'PathI'.
--
-- @since 0.1
liftPathIF ::
  (Functor f, HasCallStack) =>
  (HasCallStack => FilePath -> f FilePath) ->
  PathI i ->
  f (PathI j)
liftPathIF f = fmap MkPathI . applyPathI f

-- | 'liftPathIF' specialized to the same index. This should be preferred
-- as the former is easier to use incorrectly.
--
-- @since 0.1
liftPathIF' ::
  Functor f =>
  (HasCallStack => FilePath -> f FilePath) ->
  PathI i ->
  f (PathI i)
liftPathIF' = liftPathIF

-- | Lifts a 'FilePath' function to 'PathI'.
--
-- @since 0.1
applyPathI :: HasCallStack => (HasCallStack => FilePath -> a) -> PathI i -> a
applyPathI f = f . view #unPathI

-- | '(</>)' lifted to 'PathI'. Notice the index can change, so take care.
--
-- @since 0.1
(<//>) :: PathI i1 -> PathI i2 -> PathI i3
MkPathI x <//> MkPathI y = MkPathI (x </> y)

infixr 5 <//>

-- | @since 0.1
(<//) :: PathI i1 -> Path -> PathI i2
MkPathI x <// y = MkPathI (x </> y)

infixl 5 <//

-- | @since 0.1
(//>) :: Path -> PathI i1 -> PathI i2
(//>) = flip (<//)

infixl 5 //>

-- | Returns true if the path is the root.
--
-- @since 0.1
isRoot :: PathI i -> Bool
isRoot = isRoot' . view #unPathI

-- | Returns true if the path is the root.
--
-- @since 0.1
isRoot' :: FilePath -> Bool
isRoot' = (== "/") . T.strip . T.pack

-- | Pretty-print a list of paths.
--
-- >>> showPaths ["one", "two"]
-- "one, two"
--
-- @since 0.1
showPaths :: [PathI a] -> String
showPaths = L.intercalate ", " . fmap (view #unPathI)

-- $general
-- These functions allows for lifting arbitrary 'FilePath' functions onto our
-- 'PathI'. Note that this can easily invalidate any invariants we would
-- like to hold (e.g. appending a path can turn a directory into a file),
-- so caution must be exercised.
