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
import Text.RawString.QQ (r)

withInput :: FilePath -> String -> IO a -> IO ()
withInput tempDir input action = do
    (tempName, tempHandle) <- openTempFile tempDir "test-input"
    hPutStr tempHandle input
    hSeek tempHandle AbsoluteSeek 0 -- Reset file pointer to beginning
    hDuplicateTo tempHandle stdin -- Redirect stdin to our temp file
    result <- action
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
                output <- runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> (1, a, b)"
                        , "(2, e, f)"
                        , "(3, i, j)"
                        , "(5, g, h)"
                        , "(6, c, d)"
                        , "db> Bye!"
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
                output <- runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> - internal (size 1)"
                        , "  - leaf (size 7)"
                        , "    - 7"
                        , "    - 8"
                        , "    - 9"
                        , "    - 10"
                        , "    - 11"
                        , "    - 12"
                        , "    - 13"
                        , "  - leaf (size 6)"
                        , "    - 1"
                        , "    - 2"
                        , "    - 3"
                        , "    - 4"
                        , "    - 5"
                        , "    - 6"
                        , "db> Bye!"
                        ]
        it "insert with split nodes works" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                let cmd =
                        unlines
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
                            , "insert 28 a b" -- insert right node
                            , "insert 1 a b" -- insert left node
                            , ".tree"
                            , ".exit"
                            ]
                output <- runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> - internal (size 1)"
                        , "  - leaf (size 8)"
                        , "    - 14"
                        , "    - 16"
                        , "    - 18"
                        , "    - 20"
                        , "    - 22"
                        , "    - 24"
                        , "    - 26"
                        , "    - 28"
                        , "  - leaf (size 7)"
                        , "    - 1"
                        , "    - 2"
                        , "    - 4"
                        , "    - 6"
                        , "    - 8"
                        , "    - 10"
                        , "    - 12"
                        , "db> Bye!"
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
                output <- runReplWithInput dir cmd table
                output
                    `shouldBe` unlines
                        [ "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> Row inserted!"
                        , "db> (1, cc, dd)"
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
                        , "db> Bye!"
                        ]
