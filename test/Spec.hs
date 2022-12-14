import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

tests :: Test
tests =
  TestList [testTmp]

testTmp :: Test
testTmp =
  TestCase (assertEqual "String" (5 + 5) 10)