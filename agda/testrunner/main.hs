
import Control.Monad

import Data.Char

import System.Exit
import System.IO
import System.Process
import Text.Read ( readMaybe )

import qualified Text.XML.Light as XML

-- Test name + weight + full code
type TestCase = (String, Int, [String])

-- Test name + weight + either error or nothing
type TestResult = (String, Int, Either String ())

outputFile = "output/results.xml"

-- Run the tests on the solution in the current directory.
-- Precondition: files `library.agda`, `solution.agda`, and `test.agda` are present.
main :: IO ()
main = do
  -- Add --allow-unsolved-metas flag to solution
  sol <- readFileUTF8 "solution.agda"
  writeFileUTF8 "Solution.agda" $ unlines ["{-# OPTIONS --allow-unsolved-metas #-}"] ++ sol

  -- First check if the solution typechecks
  (exit, out, err) <- readProcessWithExitCode "agda" ["Solution.agda"] ""
  case exit of
    ExitSuccess{} -> writeResults outputFile =<< runTests
    ExitFailure{} -> writeResults outputFile [("typechecking failed",1,Left out)]

runTests :: IO [TestResult]
runTests = do
  tst <- readFileUTF8 "test.agda"
  let tests = getTests tst

  forM tests $ \(testName,testWeight,testBody) -> do

    let testFile = testName++".agda"

    writeFileUTF8 testFile $ unlines $
      [ "open import library"
      , "open import Solution"
      , ""
      ] ++ testBody

    (exit, out, err) <- readProcessWithExitCode "agda" [testFile] ""

    case exit of
      ExitSuccess{} -> return (testName, testWeight, Right ())
      ExitFailure{} -> return (testName, testWeight, Left out)

-- Get the names and code of all tests in the file with the given
-- content (names starting with `test-` or `test-x-` for a test case
-- with weight x).
getTests :: String -> [TestCase]
getTests fileContents = loop id $ lines fileContents
  where
    loop :: ([String] -> [String]) -> [String] -> [TestCase]
    loop prev []          = []
    loop prev (curr:rest) =
      let name = defName curr in
      if (take 5 name == "test-") then
        let (test,rest') = span (\l -> null l || isSpace (head l) || defName l == name) rest
        in  (name , getWeight name , prev (curr:test)) : loop prev rest'
      else loop (prev . (curr:)) rest

    defName = takeWhile $ not . isSpace

    getWeight name = case readMaybe (takeWhile (/= '-') (drop 5 name)) of
      Just (n :: Int) -> n
      Nothing         -> 1

nameAttr :: String -> XML.Attr
nameAttr n = XML.Attr (XML.unqual "name") n

weightAttr :: Int -> XML.Attr
weightAttr w = XML.Attr (XML.unqual "weight") (show w)

formatResult :: TestResult -> XML.Element
formatResult (testName,testWeight,r) = case r of
  Right _  -> XML.unode "testcase" attributes
  Left err -> XML.unode "testcase" (attributes, failureNode err)

  where
    attributes = [nameAttr testName , weightAttr testWeight]
    failureNode err = XML.unode "failure" $
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
  writeFileUTF8 outputFile $
  XML.ppTopElement $
  formatResults rs

readFileUTF8 :: String -> IO String
readFileUTF8 filename = do
  inputHandle <- openFile filename ReadMode
  hSetEncoding inputHandle utf8
  fileContents <- hGetContents inputHandle
  return fileContents

writeFileUTF8 :: String -> String -> IO ()
writeFileUTF8 filename content = do
  outputHandle <- openFile filename WriteMode
  hSetEncoding outputHandle utf8
  hPutStr outputHandle content
  hClose outputHandle
