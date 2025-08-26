module Main where

import Test.HUnit
import Lib
import System.Exit (exitFailure, exitSuccess)

-- Test cases with proper type annotations
test1 :: Test
test1 = TestCase (assertEqual "choice 0" digits (choice '0'))

test2 :: Test
test2 = TestCase (assertEqual "choice 5" ['5'] (choice '5'))

test3 :: Test
test3 = TestCase (assertEqual "choices 2x2" 
    (map (map choice) ["10", "04"]) 
    (choices ["10", "04"]))

test4 :: Test
test4 = TestCase (assertEqual "rows 2x2" 
    ["12", "34"]  -- Type signatures not needed since we're using the rows function which returns Grid
    (rows ["12", "34"]))

test5 :: Test
test5 = TestCase (assertEqual "cols 2x2" 
    ["13", "24"]  -- Type signatures not needed since we're using the cols function which returns Grid
    (cols ["12", "34"]))

test5b :: Test
test5b = TestCase (assertEqual "cols 3x3" 
    ["147", "258", "369"]  -- Type signatures not needed since we're using the cols function which returns Grid
    (cols ["123", "456", "789"]))

test6 :: Test
test6 = TestCase (assertEqual "cp 2x2" 
    [["1","3"], ["1","4"], ["2","3"], ["2","4"]]
    (cp [["1","2"], ["3","4"]]))

test7 :: Test
test7 = TestCase (assertEqual "cb 2x2" 
    [["1","3"], ["1","4"], ["2","3"], ["2","4"]]
    (cb [["1","2"], ["3","4"]]))

test8 :: Test
test8 = TestCase (assertEqual "expand simple" 
    [["1","23"]]  -- expected result: list of Grids, where Grid is [Row] and each Row is [Digit]
    (expand [[['1']], [['2'],['3']]]))  -- input: Matrix of [Digit]

test9 :: Test
test9 = TestCase (assertEqual "completitions 2x2" 
    [["12", "31"], ["12", "32"], ["12", "33"], ["12", "34"], ["12", "35"], ["12", "36"], ["12", "37"], ["12", "38"], ["12", "39"]]  -- all possible completions
    (completitions ["12", "30"]))  -- input: grid with one empty cell (marked as '0')

test10 :: Test
test10 = TestCase (assertEqual "completitions 9x9 simple" 
    -- For each empty cell (marked as 0), all digits 1-9 are tried
    [ [ "123456789"  -- these rows are fixed
      , "456789123"
      , "789123456"
      , "234567891"
      , "567891234"
      , "891234567"
      , "345678912"
      , "678912345"
      , "912345671"  -- last digit varies from 1-9
      ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345672" ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345673" ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345674" ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345675" ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345676" ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345677" ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345678" ]
    , [ "123456789", "456789123", "789123456", "234567891", "567891234"
      , "891234567", "345678912", "678912345", "912345679" ]
    ] 
    (completitions 
     [ "123456789"  -- input grid with one empty cell
     , "456789123"
     , "789123456"
     , "234567891"
     , "567891234"
     , "891234567"
     , "345678912"
     , "678912345"
     , "912345670"  -- last digit is empty (0)
     ]))

test11 :: Test
test11 = TestCase (assertEqual "completitions with multiple blanks" 
    -- With two adjacent empty cells (0), we expect all combinations of digits 1-9
    -- This will generate 81 possibilities (9 choices for each empty cell)
    [ ["12", "11"]  -- first few expected completions
    , ["12", "12"]
    , ["12", "13"]
    , ["12", "14"]
    ]  -- checking just the first 4 combinations
    (take 4 $ completitions ["12", "00"]))

test12 :: Test
test12 = TestCase (assertEqual "completitions with scattered blanks" 
    -- A 3x3 grid with three blank cells in different positions
    -- Will generate 729 possibilities (9 choices for each of 3 blank cells: 9^3)
    [ ["112", "341", "516"]  -- first completion
    , ["112", "341", "526"]  -- second completion
    , ["112", "341", "536"]  -- third completion
    , ["112", "341", "546"]  -- fourth completion
    , ["112", "341", "556"]  -- fifth completion
    ]  -- checking just the first 5 of 729 possible completions
    (take 5 $ completitions 
     [ "102"  -- 0 in middle position
     , "340"  -- 0 in last position
     , "506"  -- 0 in middle position
     ]))

test13 :: Test
test13 = TestCase (assertEqual "group numbers by 3" 
    [["123", "456", "789"], ["abc", "def", "ghi"]]
    (group ["123", "456", "789", "abc", "def", "ghi"]))

test14 :: Test
test14 = TestCase (assertEqual "group empty list"
    ([] :: [[String]])
    (group []))

test15 :: Test
test15 = TestCase (assertEqual "group with negative size"
    ([["123", "456", "789"]])
    (group ["123", "456", "789"]))


test16 :: Test
test16 = TestCase (assertEqual "ungroup nested list" 
    ["123", "456", "789", "abc", "def", "ghi"]
    (ungroup [["123", "456", "789"], ["abc", "def", "ghi"]]))

test17 :: Test
test17 = TestCase (assertEqual "ungroup empty list"
    ([] :: [String])
    (ungroup []))

tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, 
                 test13, test14, test15, test16, test17]

main :: IO ()
main = do
    results <- runTestTT tests
    if failures results > 0 || errors results > 0
        then exitFailure
        else exitSuccess
