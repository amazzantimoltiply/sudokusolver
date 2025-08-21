module Main where

import Criterion.Main
import Lib

-- Sample grids for benchmarking
grid2x2_one_empty :: [String]
grid2x2_one_empty = ["12", "30"]  -- one empty cell

grid2x2_two_empty :: [String]
grid2x2_two_empty = ["10", "02"]  -- two empty cells

grid2x2_all_empty :: [String]
grid2x2_all_empty = ["00", "00"]  -- all empty cells

grid3x3_scattered :: [String]
grid3x3_scattered = ["102",   -- one empty cell in middle
                    "340",    -- one empty cell at end
                    "506"]    -- one empty cell in middle

grid3x3_diagonal :: [String]
grid3x3_diagonal = ["051",    -- empty cells on diagonal
                    "203",
                    "130"]

grid3x3_all_filled :: [String]
grid3x3_all_filled = ["123",  -- no empty cells
                      "456",
                      "789"]

grid9x9_one_empty :: [String]
grid9x9_one_empty = [ "123456789"
                    , "456789123"
                    , "789123456"
                    , "234567891"
                    , "567891234"
                    , "891234567"
                    , "345678912"
                    , "678912345"
                    , "912345670"  -- one empty cell at end
                    ]

grid9x9_scattered :: [String]
grid9x9_scattered = [ "123456780"  -- scattered empty cells
                    , "456789120"
                    , "789123456"
                    , "234067891"
                    , "567891234"
                    , "891234567"
                    , "345678012"
                    , "678912345"
                    , "912345678"
                    ]

grid9x9_diagonal :: [String]
grid9x9_diagonal = [ "023456789"  -- empty cells on diagonal
                    , "450789123"
                    , "789023456"
                    , "234560891"
                    , "567891034"
                    , "891234507"
                    , "345678902"
                    , "678912305"
                    , "912345678"
                    ]

main :: IO ()
main = defaultMain [
    bgroup "basic operations" [
        bgroup "choice" [
            bench "empty cell" $ whnf choice '0',
            bench "filled cell" $ whnf choice '5'
        ],
        bgroup "rows" [
            bench "2x2 grid" $ nf rows grid2x2_one_empty,
            bench "3x3 grid" $ nf rows grid3x3_scattered,
            bench "9x9 grid" $ nf rows grid9x9_one_empty
        ],
        bgroup "cols" [
            bench "2x2 grid" $ nf cols grid2x2_one_empty,
            bench "3x3 grid" $ nf cols grid3x3_scattered,
            bench "9x9 grid" $ nf cols grid9x9_one_empty
        ]
    ],
    
    bgroup "cartesian products" [
        bgroup "cp" [
            bench "2x2 lists" $ nf cp [["1","2"], ["3","4"]],
            bench "3x3 lists" $ nf cp [["1","2","3"], ["4","5","6"], ["7","8","9"]],
            bench "empty list" $ nf cp ([] :: [[Int]]),
            bench "single list" $ nf cp [["1","2","3"]]
        ],
        bgroup "cb" [
            bench "2x2 blocks" $ nf cb [["1","2"], ["3","4"]],
            bench "3x3 blocks" $ nf cb [["1","2","3"], ["4","5","6"], ["7","8","9"]],
            bench "empty block" $ nf cb ([] :: [[Int]]),
            bench "single block" $ nf cb [["1","2","3"]]
        ]
    ],

    bgroup "grid completion" [
        bgroup "expand" [
            bench "2x2 simple" $ nf expand [[['1']], [['2'],['3']]],
            bench "2x2 complex" $ nf expand [[['1'],['2']], [['3'],['4']], [['5'],['6']]],
            bench "empty grid" $ nf expand ([] :: [[[Char]]]),
            bench "single row" $ nf expand [[['1'],['2'],['3']]]
        ],
        bgroup "completitions 2x2" [
            bench "one empty" $ nf completitions grid2x2_one_empty,
            bench "two empty" $ nf completitions grid2x2_two_empty,
            bench "all empty" $ nf completitions grid2x2_all_empty
        ],
        bgroup "completitions 3x3" [
            bench "scattered empty" $ nf completitions grid3x3_scattered,
            bench "diagonal empty" $ nf completitions grid3x3_diagonal,
            bench "all filled" $ nf completitions grid3x3_all_filled
        ],
        bgroup "completitions 9x9" [
            bench "one empty" $ nf completitions grid9x9_one_empty,
            bench "scattered empty" $ nf completitions grid9x9_scattered,
            bench "diagonal empty" $ nf completitions grid9x9_diagonal
        ]
    ]
  ]
