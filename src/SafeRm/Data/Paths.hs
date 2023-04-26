{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for distinguishing path types.
module SafeRm.Data.Paths
  ( -- * Types
    PathI (..),
    PathIndex (..),

    -- * Functions

    -- ** Specific
    isEmpty,
    isRoot,
    isRoot',
    toText,

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
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Prelude

-- | Types of filepaths used in SafeRm.
data PathIndex
  = -- TRASH DIRECTORY PATHS

    -- | The trash directory.
    TrashHome
  | -- | The trash log file.
    TrashLog
  | -- | The directory to the trash files themselves i.e. <trash>/files.
    TrashDirFiles
  | -- | The directory to the trash info files i.e. <trash>/info.
    TrashDirInfo
  | -- TRASH ENTRY PATHS

    -- | The full trash path i.e. @\<trash-home\>\/files\/'\<trash-name\>@.
    TrashEntryPath
  | -- | The full trash info path i.e. @\<trash-home\>\/info\/'\<trash-name\>.trashinfo@.
    TrashEntryInfo
  | -- TRASH ENTRY FIELD TYPES

    -- | The name corresponding to some file/directory in the trash directory.
    TrashEntryFileName
  | -- | The original path for some file/directory in the trash directory.
    TrashEntryOriginalPath

-- | Indexed 'FilePath' so that we can prevent mixing up different filepaths.
type PathI :: PathIndex -> Type
newtype PathI (i :: PathIndex) = MkPathI
  { unPathI :: FilePath
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable, NFData)
  deriving (IsString, Monoid, Semigroup) via FilePath

makeFieldLabelsNoPrefix ''PathI

instance Serialize (PathI i) where
  type DecodeExtra (PathI i) = ()

  -- encode = C8.pack . view #unPathI
  encode = encodeUtf8 . view (#unPathI % packed)

  decode _ bs = bimap displayException (MkPathI . T.unpack) (decodeUtf8 bs)

-- | Modifies the index.
reindex :: PathI i -> PathI j
reindex = liftPathI id

-- | Lifts a 'FilePath' transformation to 'PathI', allowing for the index to
-- change.
liftPathI :: (FilePath -> FilePath) -> PathI i -> PathI j
liftPathI f (MkPathI fp) = MkPathI (f fp)

-- | 'liftPathI' specialized to the same index. This should be preferred
-- as the former is easier to use incorrectly.
liftPathI' :: (FilePath -> FilePath) -> PathI i -> PathI i
liftPathI' = liftPathI

-- | Lifts an effectful 'FilePath' transformation to 'PathI'.
liftPathIF ::
  (Functor f, HasCallStack) =>
  ((HasCallStack) => FilePath -> f FilePath) ->
  PathI i ->
  f (PathI j)
liftPathIF f = fmap MkPathI . applyPathI f

-- | 'liftPathIF' specialized to the same index. This should be preferred
-- as the former is easier to use incorrectly.
liftPathIF' ::
  (Functor f) =>
  ((HasCallStack) => FilePath -> f FilePath) ->
  PathI i ->
  f (PathI i)
liftPathIF' = liftPathIF

-- | Lifts a 'FilePath' function to 'PathI'.
applyPathI :: (HasCallStack) => ((HasCallStack) => FilePath -> a) -> PathI i -> a
applyPathI f = f . view #unPathI

-- | '(</>)' lifted to 'PathI'. Notice the index can change, so take care.
(<//>) :: PathI i1 -> PathI i2 -> PathI i3
MkPathI x <//> MkPathI y = MkPathI (x </> y)

infixr 5 <//>

(<//) :: PathI i1 -> Path -> PathI i2
MkPathI x <// y = MkPathI (x </> y)

infixl 5 <//

(//>) :: Path -> PathI i1 -> PathI i2
(//>) = flip (<//)

infixl 5 //>

-- | Returns true if the paths is empty. Note that whitespace is __not__
-- considered empty as we are trying to prevent deleting "" (which gets
-- turned into the current working directory). But posix filepaths can be
-- whitespace (e.g. " "), and that is fine.
isEmpty :: PathI i -> Bool
isEmpty = null . view #unPathI

-- | Returns true if the path is the root.
isRoot :: PathI i -> Bool
isRoot = isRoot' . view #unPathI

-- | Returns true if the path is the root.
isRoot' :: FilePath -> Bool
#if WINDOWS
isRoot' = f . T.unpack . T.strip . T.pack
  where
    f (_ : ':' : rest) = null rest || rest == "\\"
    f _ = False
#else
isRoot' = (== "/") . T.strip . T.pack
#endif

-- | Pretty-print a list of paths.
--
-- >>> showPaths ["one", "two"]
-- "one, two"
showPaths :: [PathI a] -> String
showPaths = L.intercalate ", " . fmap (view #unPathI)

toText :: PathI i -> Text
toText = T.pack . view #unPathI

-- $general
-- These functions allows for lifting arbitrary 'FilePath' functions onto our
-- 'PathI'. Note that this can easily invalidate any invariants we would
-- like to hold (e.g. appending a path can turn a directory into a file),
-- so caution must be exercised.
