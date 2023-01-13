-- | Prelude for integration test suite.
--
-- @since 0.1
module Integration.Prelude
  ( module X,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,
  )
where

import Hedgehog as X
  ( Gen,
    MonadGen,
    MonadTest,
    Property,
    annotate,
    annotateShow,
    assert,
    failure,
    forAll,
    property,
    withTests,
    (===),
  )
import SafeRm.FileUtils as X
import SafeRm.Prelude as X
import Test.Tasty as X (TestTree, askOption, testGroup)
import Test.Tasty.Hedgehog as X (testPropertyNamed)

-- | Asserts that files exist.
--
-- @since 0.1
assertFilesExist :: (Foldable f, MonadIO m, MonadTest m) => f FilePath -> m ()
assertFilesExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ doesFileExist p
    annotate $ "Expected file to exist: " <> p
    assert exists

-- | Asserts that files do not exist.
--
-- @since 0.1
assertFilesDoNotExist :: (Foldable f, MonadIO m, MonadTest m) => f FilePath -> m ()
assertFilesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ doesFileExist p
    annotate $ "Expected file not to exist: " <> p
    assert (not exists)
