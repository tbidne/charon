-- | Unit tests for SafeRm.Utils
module Unit.Utils
  ( tests,
  )
where

import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm.Utils qualified as Utils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Utils"
    [ testMatchesWildcards,
      testStripInfix,
      percentEncodeRoundTrip
    ]

testMatchesWildcards :: TestTree
testMatchesWildcards = testCase "matchesWildcards" $ do
  assertBool "* ~ any" $ Utils.matchesWildcards "*" "foo"
  assertBool "*X ~ X" $ Utils.matchesWildcards "*foo" "foo"
  assertBool "X* ~ X" $ Utils.matchesWildcards "foo*" "foo"

  assertBool "*A* ~ A*" $ Utils.matchesWildcards "*f*" "foo"
  assertBool "*AB* ~ AB*" $ Utils.matchesWildcards "*fo*" "foo"
  assertBool "*AB* ~ AB*" $ Utils.matchesWildcards "*fo*" "foo"

  assertBool "*A* ~ *A*" $ Utils.matchesWildcards "*bar*" "foobarbaz"
  assertBool "A*B* ~ ABC" $ Utils.matchesWildcards "foo*bar*" "foobarbaz"
  assertBool "A*B*C* ~ ABXCY" $ Utils.matchesWildcards "foo*b*r*" "foobarbaz"
  assertBool "A**B***C ~ ABC" $ Utils.matchesWildcards "fo**bar***z" "foobarbaz"

  assertFalse "A*B*C /~ ADC" $ Utils.matchesWildcards "foo*x*baz" "foobarbaz"
  assertFalse "*A* /~ B" $ Utils.matchesWildcards "*x*" "foobarbaz"
  assertFalse "A*B /~ ABC" $ Utils.matchesWildcards "foo*bar" "foobarbaz"

testStripInfix :: TestTree
testStripInfix = testCase "stripInfix" $ do
  -- trivial cases
  Just ("", "foo") @=? Utils.stripInfix "" "foo"
  Just ("", "") @=? Utils.stripInfix "" ""
  Just ("", "") @=? Utils.stripInfix "foo" "foo"

  -- prefix
  Just ("", "bar") @=? Utils.stripInfix "foo" "foobar"

  -- suffix
  Just ("foo", "") @=? Utils.stripInfix "bar" "foobar"

  -- infix
  Just ("foo", "baz") @=? Utils.stripInfix "bar" "foobarbaz"

  -- first infix
  Just ("foo", "blahbarbaz") @=? Utils.stripInfix "bar" "foobarblahbarbaz"

  -- complex
  case Utils.stripInfix "first" "firstsecondthird" of
    Just ("", suffix@"secondthird") ->
      -- previously, we had a bug where this would erroneously be
      -- Just ("se","second") because we failed to take the offset into
      -- account. I.e. for 2nd elem's first index we had
      --
      --     TI.text arr (x + plen)
      --
      -- instead of the correct
      --
      --     TI.text arr (x + off + plen)
      Just ("se", "dthird") @=? Utils.stripInfix "con" suffix
    other ->
      assertFailure
        $ "first `stripInfix` firstsecondthird returned unexpected: "
        <> show other

  -- failures
  Nothing @=? Utils.stripInfix "aa" "foobar"
  Nothing @=? Utils.stripInfix "perm-delete" "foobar"

percentEncodeRoundTrip :: TestTree
percentEncodeRoundTrip =
  testPropertyNamed "percentDecode . percentEncode ~ id" "percentEncodeRoundTrip" $ do
    property $ do
      path <- forAll genPath
      let pathEncoded = Utils.percentEncode path

      annotateShow pathEncoded

      path === Utils.percentDecode pathEncoded

genPath :: Gen ByteString
genPath = encodeUtf8 . T.pack <$> genString

genString :: Gen String
genString = Gen.string (Range.linear 1 100) genPathChar

genPathChar :: Gen Char
genPathChar = Gen.filter (/= '\NUL') Gen.unicode
