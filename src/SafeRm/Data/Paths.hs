-- | Provides functionality for distinguishing path types.
--
-- @since 0.1
module SafeRm.Data.Paths
  ( -- * Types
    PathI (..),
    _MkPathI,
    PathIndex (..),

    -- * Functions

    -- ** Specific
    indexToHome,
    getTrashAndIndex,

    -- ** General
    -- $general
    (<//>),
    applyPathI,
    liftPathI,
    liftPathI',
    liftPathIF,
    liftPathIF',
  )
where

import Data.Csv (FromField, ToField)
import SafeRm.Prelude
import System.Directory qualified as Dir
import System.FilePath qualified as FP

-- | Types of filepaths used in SafeRm.
--
-- @since 0.1
data PathIndex
  = -- | The trash directory.
    --
    -- @since 0.1
    TrashHome
  | -- | The trash index file.
    --
    -- @since 0.1
    TrashIndex
  | -- | The name corresponding to some file/directory in the trash directory.
    --
    -- @since 0.1
    TrashName
  | -- | The original path for some file/directory in the trash directory.
    --
    -- @since 0.1
    OriginalPath
  | -- | The full trash path i.e. @'TrashHome' '</>' 'TrashName'@
    --
    -- @since 0.1
    TrashPath

-- | Indexed 'FilePath' so that we can prevent mixing up different filepaths.
--
-- @since 0.1
type PathI :: PathIndex -> Type
newtype PathI (i :: PathIndex) = MkPathI FilePath
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
      NFData
    )
  deriving
    ( -- | @since 0.1
      FromField,
      -- | @since 0.1
      IsString,
      -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      ToField
    )
    via FilePath

-- | @since 0.1
_MkPathI :: Iso' (PathI i) FilePath
_MkPathI = iso (\(MkPathI fp) -> fp) MkPathI

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
liftPathIF :: Functor f => (FilePath -> f FilePath) -> PathI i -> f (PathI j)
liftPathIF f = fmap MkPathI . applyPathI f

-- | 'liftPathIF' specialized to the same index. This should be preferred
-- as the former is easier to use incorrectly.
--
-- @since 0.1
liftPathIF' :: Functor f => (FilePath -> f FilePath) -> PathI i -> f (PathI i)
liftPathIF' = liftPathIF

-- | Lifts a 'FilePath' function to 'PathI'.
--
-- @since 0.1
applyPathI :: (FilePath -> a) -> PathI i -> a
applyPathI f = f . view _MkPathI

-- | Returns the trash index's home directory.
--
-- @since 0.1
indexToHome :: PathI TrashIndex -> PathI TrashHome
indexToHome (MkPathI fp) = MkPathI $ FP.takeDirectory fp

-- | Returns @(trashHome, indexPath)@, where @trashHome@ is either the
-- user-supplied path or the default, if none is given.
--
-- @since 0.1
getTrashAndIndex ::
  Maybe (PathI TrashHome) -> IO (PathI TrashHome, PathI TrashIndex)
getTrashAndIndex mfp = do
  res <- trashOrDefault mfp
  pure (res, liftPathI (</> ".index.csv") res)

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
--
-- @since 0.1
trashOrDefault :: Maybe (PathI TrashHome) -> IO (PathI TrashHome)
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the default trash directory.
--
-- @since 0.1
getTrashHome :: IO (PathI TrashHome)
getTrashHome = MkPathI . (</> ".trash") <$> Dir.getHomeDirectory

-- | '(</>)' lifted to 'PathI'. Notice the index can change, so take care.
--
-- @since 0.1
(<//>) :: PathI i1 -> PathI i2 -> PathI i3
MkPathI x <//> MkPathI y = MkPathI (x </> y)

infixr 5 <//>

-- $general
-- These functions allows for lifting arbitrary 'FilePath' functions onto our
-- 'PathI'. Note that this can easily invalidate any invariants we would
-- like to hold (e.g. appending a path can turn a directory into a file),
-- so caution must be exercised.
