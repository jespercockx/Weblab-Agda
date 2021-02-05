
import Control.Monad

import Data.Char

import System.Exit
import System.Process

import qualified Text.XML.Light as XML

-- Test name + full code
type TestCase = (String, [String])

-- Test name + either error or nothing
type TestResult = (String, Either String ())

outputFile = "output/results.xml"

-- Run the tests on the solution in the current directory.
-- Precondition: files `library.agda`, `solution.agda`, and `test.agda` are present.
main :: IO ()
main = do

  -- First check if the solution typechecks
  (exit, out, err) <- readProcessWithExitCode "agda" ["solution.agda"] ""
  case exit of
    ExitSuccess{} -> writeResults outputFile =<< runTests
    ExitFailure{} -> writeResults outputFile [("typechecking",Left out)]

runTests :: IO [TestResult]
runTests = do
  tst <- readFile "test.agda"
  let tests = getTests tst

  forM tests $ \(testName,testBody) -> do

    let testFile = testName++".agda"
  
    writeFile testFile $ unlines $
      [ "open import library"
      , "open import solution"
      , ""
      ] ++ testBody

    (exit, out, err) <- readProcessWithExitCode "agda" [testFile] ""

    case exit of
      ExitSuccess{} -> return (testName, Right ())
      ExitFailure{} -> return (testName, Left out)

-- Get the names and code of all tests in the file with the
-- given content (names starting with `test-`).
getTests :: String -> [TestCase]
getTests fileContents = loop id $ lines fileContents
  where
    loop :: ([String] -> [String]) -> [String] -> [TestCase]
    loop prev []          = []
    loop prev (curr:rest) =
      let name = defName curr in
      if (take 5 name == "test-") then 
        let (test,rest') = span (\l -> null l || isSpace (head l) || defName l == name) rest
        in  (name , prev (curr:test)) : loop prev rest' 
      else loop (prev . (curr:)) rest

    defName = takeWhile $ not . isSpace


formatResult :: TestResult -> XML.Element
formatResult (testName,r) = case r of
  Right _  -> XML.unode "testcase" ()
  Left err -> XML.unode "testcase" $ XML.unode "failure" $
                    unlines [ "Test " ++ testName ++ " failed!"
                            , err
                            ]

formatResults :: [TestResult] -> XML.Element
formatResults rs =
  XML.unode "testsuites" $
  XML.unode "testsuite" $
  map formatResult rs

writeResults :: FilePath -> [TestResult] -> IO ()
writeResults outputFile rs =
  writeFile outputFile $
  XML.ppTopElement $
  formatResults rs
