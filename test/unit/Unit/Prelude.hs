{-# LANGUAGE QuasiQuotes #-}

-- | Prelude for unit test suite.
module Unit.Prelude
  ( module X,
    TextMatch (..),
    assertFalse,
    assertMatches,
    getDefaultTrash,
  )
where

import Hedgehog as X
  ( Gen,
    MonadGen,
    MonadTest,
    Property,
    TestLimit,
    annotate,
    annotateShow,
    assert,
    failure,
    forAll,
    property,
    withTests,
    (===),
  )
import SafeRm.Prelude as X
import Test.Tasty as X (TestTree, askOption, testGroup)
import Test.Tasty.Golden as X (goldenVsFile)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import Test.Utils (TextMatch (..), assertMatches)

getDefaultTrash :: IO OsPath
getDefaultTrash = (</> [osp|.trash|]) <$> getHomeDirectory

assertFalse :: String -> Bool -> IO ()
assertFalse d = assertBool d . not
