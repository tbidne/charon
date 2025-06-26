{-# LANGUAGE QuasiQuotes #-}

-- | Prelude for unit test suite.
module Unit.Prelude
  ( module X,
    assertFalse,
    getDefaultTrash,
  )
where

import Charon.Prelude as X
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
import Test.Tasty as X (TestTree, askOption, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import Test.Utils as X (TextMatch, assertMatches, goldenDiffCustom)

getDefaultTrash :: IO OsPath
getDefaultTrash = (</> [osp|.trash|]) <$> getHomeDirectory

assertFalse :: String -> Bool -> IO ()
assertFalse d = assertBool d . not
