-- | Prelude for unit test suite.
--
-- @since 0.1
module Unit.Prelude
  ( module X,
    TextMatch (..),
    assertFalse,
    assertMatches,
    getDefaultTrash,
    diff,
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
import Test.Tasty.Golden as X (goldenVsStringDiff)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import Test.Utils (TextMatch (..), assertMatches)

getDefaultTrash :: IO FilePath
getDefaultTrash = (</> ".trash") <$> getHomeDirectory

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

-- | @since 0.1
assertFalse :: String -> Bool -> IO ()
assertFalse d = assertBool d . not
