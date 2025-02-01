{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HqLiteSpec where

import Control.Monad.State
import GHC.IO.Handle
import HqLite
import HqLite.Table (createTable)
import HqLite.Table.Types (Table)
import System.Directory (removeFile)
import System.IO (openTempFile, stdin)
import System.IO.Silently (capture_)
import System.IO.Temp (withTempDirectory)
import Test.Hspec
import Data.List (stripPrefix)

withInput :: FilePath -> String -> IO a -> IO ()
withInput tempDir input action = do
    (tempName, tempHandle) <- openTempFile tempDir "test-input"
    hPutStr tempHandle input
    hSeek tempHandle AbsoluteSeek 0 -- Reset file pointer to beginning
    hDuplicateTo tempHandle stdin -- Redirect stdin to our temp file
    _ <- action
    hClose tempHandle
    removeFile tempName

-- Helper function to run the REPL with input
runReplWithInput :: FilePath -> String -> Table -> IO String
runReplWithInput tempDir input table = capture_ $ do
    withInput tempDir input $ execStateT replLoop table

-- Test suite
spec :: Spec
spec = do
    describe "REPL" $ do
        it "handles meta commands" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                output <- runReplWithInput dir ".exit\n" table
                output `shouldBe` "db> Bye!\n"

        it "inserts and selects rows" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let cmd =
                        unlines
                            [ "insert 1 user1 user1@example.com"
                            , "select"
                            , ".exit"
                            ]

                putStrLn cmd
                output <- runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "db> Row inserted!"
                        , "db> (1, user1, user1@example.com)"
                        , "db> Bye!"
                        ]

        it "handles invalid commands" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                output <- runReplWithInput dir "INVALID COMMAND\n.exit\n" table
                output
                    `shouldBe` unlines
                        [ "db> Invalid SQL command: INVALID COMMAND"
                        , "db> Bye!"
                        ]

        it "handles duplicates" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                output <- runReplWithInput dir "insert 1 user1 hello@email.com\ninsert 1 diff hello@gmail.com\n.exit\n" table
                output
                    `shouldBe` unlines
                        [ "db> Row inserted!"
                        , "db> Error: Cannot insert row. Row with existing key already found!"
                        , "db> Bye!"
                        ]

        it "inserts in sorted order" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let cmd =
                        unlines
                            [ "insert 1 a b"
                            , "insert 6 c d"
                            , "insert 2 e f"
                            , "insert 5 g h"
                            , "insert 3 i j"
                            , "select"
                            , ".exit"
                            ]
                output <- (unlines <$> cleanOutput) . lines <$> runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "(1, a, b)"
                        , "(2, e, f)"
                        , "(3, i, j)"
                        , "(5, g, h)"
                        , "(6, c, d)"
                        , "Bye!"
                        ]

        it "inserts and splits root" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let cmd =
                        unlines
                            [ "insert 1 a b"
                            , "insert 2 a b"
                            , "insert 3 a b"
                            , "insert 4 a b"
                            , "insert 5 a b"
                            , "insert 6 a b"
                            , "insert 7 a b"
                            , "insert 8 a b"
                            , "insert 9 a b"
                            , "insert 10 a b"
                            , "insert 11 a b"
                            , "insert 12 a b"
                            , "insert 13 a b"
                            , ".tree"
                            , ".exit"
                            ]
                output <- (unlines <$> cleanOutput) . lines <$> runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "- internal (size 1) [6]"
                        , "  - leaf (size 6)"
                        , "    - 1"
                        , "    - 2"
                        , "    - 3"
                        , "    - 4"
                        , "    - 5"
                        , "    - 6"
                        , "  - leaf (size 7)"
                        , "    - 7"
                        , "    - 8"
                        , "    - 9"
                        , "    - 10"
                        , "    - 11"
                        , "    - 12"
                        , "    - 13"
                        , "Bye!"
                        ]
        it "insert with split nodes works" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let insertCommands =
                        [ "insert 2 a b"
                        , "insert 4 a b"
                        , "insert 6 a b"
                        , "insert 8 a b"
                        , "insert 10 a b"
                        , "insert 12 a b"
                        , "insert 14 a b"
                        , "insert 16 a b"
                        , "insert 18 a b"
                        , "insert 20 a b"
                        , "insert 22 a b"
                        , "insert 24 a b"
                        , "insert 26 a b"
                        , "insert 28 a b"
                        , "insert 1 a b"  
                        ]
                let cmd = unlines $ insertCommands ++ [".tree", ".exit"]
                output <- (unlines <$> cleanOutput) . lines <$> runReplWithInput dir cmd table
                output `shouldBe` unlines
                    [ "- internal (size 1) [12]"
                    , "  - leaf (size 7)"
                    , "    - 1"
                    , "    - 2"
                    , "    - 4"
                    , "    - 6"
                    , "    - 8"
                    , "    - 10"
                    , "    - 12"
                    , "  - leaf (size 8)"
                    , "    - 14"
                    , "    - 16"
                    , "    - 18"
                    , "    - 20"
                    , "    - 22"
                    , "    - 24"
                    , "    - 26"
                    , "    - 28"
                    , "Bye!"
                    ]
        it "runs select properly" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let cmd =
                        unlines
                            [ "insert 2 a b"
                            , "insert 4 c d"
                            , "insert 6 e f"
                            , "insert 8 g h"
                            , "insert 10 i j"
                            , "insert 12 k l"
                            , "insert 14 m n"
                            , "insert 16 o p"
                            , "insert 18 q r"
                            , "insert 20 s t"
                            , "insert 22 u v"
                            , "insert 24 w x"
                            , "insert 26 y z"
                            , "insert 28 aa bb"
                            , "insert 1 cc dd"
                            , "select"
                            , ".exit"
                            ]
                output <- (unlines <$> cleanOutput) . lines <$> runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "(1, cc, dd)"
                        , "(2, a, b)"
                        , "(4, c, d)"
                        , "(6, e, f)"
                        , "(8, g, h)"
                        , "(10, i, j)"
                        , "(12, k, l)"
                        , "(14, m, n)"
                        , "(16, o, p)"
                        , "(18, q, r)"
                        , "(20, s, t)"
                        , "(22, u, v)"
                        , "(24, w, x)"
                        , "(26, y, z)"
                        , "(28, aa, bb)"
                        , "Bye!"
                        ]
        it "splits left leaf nodes with parent node that is internal" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let insertCommands =
                        [ "insert 100 a a"
                        , "insert 102 a b"
                        , "insert 103 a b"
                        , "insert 104 a b"
                        , "insert 105 a b"
                        , "insert 106 a b"
                        , "insert 107 a b"
                        , "insert 108 a b"
                        , "insert 1 a b"
                        , "insert 2 a b"
                        , "insert 3 a b"
                        , "insert 4 a b"
                        , "insert 5 a b"
                        , "insert 6 a b"
                        , "insert 7 a b"
                        , "insert 8 a b"
                        , "insert 9 a b"
                        , "insert 10 a b"
                        , "insert 11 a b"
                        , "insert 12 a b"
                        , "insert 13 a b"
                        , "insert 14 a b"
                        , "insert 15 a b"
                        , "insert 16 a b"
                        ]
                let cmd = unlines $ insertCommands ++ [".tree", ".exit"]
                output <- (unlines <$> cleanOutput) . lines <$> runReplWithInput dir cmd table
                output `shouldBe` unlines
                    [ "- internal (size 2) [6, 100]"
                    , "  - leaf (size 6)"
                    , "    - 1"
                    , "    - 2"
                    , "    - 3"
                    , "    - 4"
                    , "    - 5"
                    , "    - 6"
                    , "  - leaf (size 11)"
                    , "    - 7"
                    , "    - 8"
                    , "    - 9"
                    , "    - 10"
                    , "    - 11"
                    , "    - 12"
                    , "    - 13"
                    , "    - 14"
                    , "    - 15"
                    , "    - 16"
                    , "    - 100"
                    , "  - leaf (size 7)"
                    , "    - 102"
                    , "    - 103"
                    , "    - 104"
                    , "    - 105"
                    , "    - 106"
                    , "    - 107"
                    , "    - 108"
                    , "Bye!"
                    ]
        it "makes a four leaf node tree" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let insertCommands =
                        [ "insert 100 a a"
                        , "insert 102 a b"
                        , "insert 103 a b"
                        , "insert 104 a b"
                        , "insert 105 a b"
                        , "insert 106 a b"
                        , "insert 107 a b"
                        , "insert 108 a b"
                        , "insert 1 a b"
                        , "insert 2 a b"
                        , "insert 3 a b"
                        , "insert 4 a b"
                        , "insert 5 a b"
                        , "insert 6 a b"
                        , "insert 7 a b"
                        , "insert 8 a b"
                        , "insert 9 a b"
                        , "insert 10 a b"
                        , "insert 11 a b"
                        , "insert 12 a b"
                        , "insert 13 a b"
                        , "insert 14 a b"
                        , "insert 15 a b"
                        , "insert 16 a b"
                        , "insert 17 a b"
                        , "insert 18 a b"
                        ]
                let cmd = unlines $ insertCommands ++ [".tree", ".exit"]
                output <- (unlines <$> cleanOutput) . lines <$> runReplWithInput dir cmd table
                output `shouldBe` unlines
                    [ "- internal (size 3) [6, 12, 100]"
                    , "  - leaf (size 6)"
                    , "    - 1"
                    , "    - 2"
                    , "    - 3"
                    , "    - 4"
                    , "    - 5"
                    , "    - 6"
                    , "  - leaf (size 6)"
                    , "    - 7"
                    , "    - 8"
                    , "    - 9"
                    , "    - 10"
                    , "    - 11"
                    , "    - 12"
                    , "  - leaf (size 7)"
                    , "    - 13"
                    , "    - 14"
                    , "    - 15"
                    , "    - 16"
                    , "    - 17"
                    , "    - 18"
                    , "    - 100"
                    , "  - leaf (size 7)"
                    , "    - 102"
                    , "    - 103"
                    , "    - 104"
                    , "    - 105"
                    , "    - 106"
                    , "    - 107"
                    , "    - 108"
                    , "Bye!"
                    ]
        it "splits rightmost key with internal node" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let insertCommands =
                        [ "insert 100 a a"
                        , "insert 102 a b"
                        , "insert 103 a b"
                        , "insert 104 a b"
                        , "insert 105 a b"
                        , "insert 106 a b"
                        , "insert 107 a b"
                        , "insert 108 a b"
                        , "insert 1 a b"
                        , "insert 2 a b"
                        , "insert 3 a b"
                        , "insert 4 a b"
                        , "insert 5 a b"
                        , "insert 109 a b"
                        , "insert 110 a b"
                        , "insert 112 a b"
                        , "insert 113 a b"
                        , "insert 114 a b"
                        , "insert 115 a b"
                        ]
                let cmd = unlines $ insertCommands ++ [".tree", ".exit"]
                output <- (unlines <$> cleanOutput) . lines <$> runReplWithInput dir cmd table
                output `shouldBe` unlines
                    [ "- internal (size 2) [100, 107]"
                    , "  - leaf (size 6)"
                    , "    - 1"
                    , "    - 2"
                    , "    - 3"
                    , "    - 4"
                    , "    - 5"
                    , "    - 100"
                    , "  - leaf (size 6)"
                    , "    - 102"
                    , "    - 103"
                    , "    - 104"
                    , "    - 105"
                    , "    - 106"
                    , "    - 107"
                    , "  - leaf (size 7)"
                    , "    - 108"
                    , "    - 109"
                    , "    - 110"
                    , "    - 112"
                    , "    - 113"
                    , "    - 114"
                    , "    - 115"
                    , "Bye!"
                    ]

-- Function to clean the output
cleanOutput :: [String] -> [String]
cleanOutput output =
    let
        -- Step 1: Remove all "db> Row inserted!" lines
        filteredOutput = filter (/= "db> Row inserted!") output
        -- Step 2: Remove the "db> " prefix from each line
        removePrefix line = case stripPrefix "db> " line of
            Just stripped -> stripped
            Nothing -> line
    in
        map removePrefix filteredOutput