{-# LANGUAGE OverloadedStrings #-}

module HqLiteSpec where

import Test.Hspec
import System.IO.Silently (capture_)
import System.IO.Temp (withTempDirectory)
import Control.Monad.State
import System.IO (stdin, openTempFile)
import System.Directory (removeFile)
import HqLite
import HqLite.Table.Types (Table)
import GHC.IO.Handle
import HqLite.Table (createTable)


withInput :: FilePath -> String -> IO a -> IO ()
withInput tempDir input action = do
    (tempName, tempHandle) <- openTempFile tempDir "test-input"
    hPutStr tempHandle input
    hSeek tempHandle AbsoluteSeek 0  -- Reset file pointer to beginning
    hDuplicateTo tempHandle stdin    -- Redirect stdin to our temp file
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
                output <- runReplWithInput dir "insert 1 user1 user1@example.com\nselect\n.exit\n" table
                output `shouldBe` unlines
                    [ "db> Row inserted!"
                    , "db> (1, user1, user1@example.com)"
                    , "db> Bye!"
                    ]

        it "handles invalid commands" $ do
            withTempDirectory "./" "tmp" $ \dir -> do
                let dbPath = dir ++ "/test.db"
                table <- createTable dbPath
                output <- runReplWithInput dir "INVALID COMMAND\n.exit\n" table
                output `shouldBe` unlines
                    [ "db> Invalid SQL command: INVALID COMMAND"
                    , "db> Bye!"
                    ]