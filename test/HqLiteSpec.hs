module HqLiteSpec where

import Test.Hspec

import HqLite
import HqLite.Table
import qualified Data.Text as T
import HqLite.Commands

runCommands :: [String] -> Table
runCommands = foldl executeCommand emptyTable
    where
        executeCommand :: Table -> String -> Table
        executeCommand table cmd =
            case parseCommand cmd of
                Right parsedCmd -> case parsedCmd of
                    SqlCommand sql -> executeSQL sql table
                    _ -> table
                _ -> table

finalRows :: [String] -> [Row]
finalRows = selectFunc . runCommands

spec :: Spec
spec = do
    describe "DB" $ do
        emptyDB
        oneInsert
        fiveInserts

createRow :: Int -> String -> String -> Row
createRow n s1 s2 = Row n (T.pack s1) (T.pack s2)

emptyDB :: SpecWith ()
emptyDB = do
    it "EmptyDB returns nothing" $ do
        finalRows ["select"] `shouldBe` []

oneInsert :: SpecWith ()
oneInsert = do
    it "Insert one item" $ do
        finalRows ["insert 100 fortnite ninja@gmail.com"] `shouldBe` [createRow 100 "fortnite" "ninja@gmail.com"]

fiveInserts :: SpecWith ()
fiveInserts = do
    it "Insert five items" $ do
        finalRows
            [ "insert 100 fortnite ninja@gmail.com"
            , "insert 200 fortnite ninja@gmail.com"
            , "insert 300 fortnite ninja@gmail.com"
            , "insert 400 fortnite ninja@gmail.com"
            , "insert 500 fortnite ninja@gmail.com"
            ]
            `shouldBe`
            [ createRow 100 "fortnite" "ninja@gmail.com"
            , createRow 200 "fortnite" "ninja@gmail.com"
            , createRow 300 "fortnite" "ninja@gmail.com"
            , createRow 400 "fortnite" "ninja@gmail.com"
            , createRow 500 "fortnite" "ninja@gmail.com"
            ]

