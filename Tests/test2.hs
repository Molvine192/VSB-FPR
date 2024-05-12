{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
data Attribute = Attribute {name :: String, value :: String} deriving(Show)
data Tag = Tag {jmeno :: String, attributes :: [Attribute]} deriving(Show)
data HtmlDoc = HtmlDoc {tags :: [Tag]} deriving(Show)

htmlDoc :: HtmlDoc
htmlDoc = HtmlDoc [
    Tag "tag1" [
        Attribute "at1" "val1"
        ],
    Tag "tag2" [
            Attribute "at2" "val2",
            Attribute "at3" "val3",
            Attribute "at4" "val4"
        ]
    ]

data FileType = Image | Exucutable | SourceCode | TextFile
data Entry = File {name1 :: String, size :: Int, ftype :: FileType}
            | Directory {name2 :: String, entries :: [Entry]}

root :: Entry
root = Directory "root"
    [
        File "logo.jpg" 5000 Image,
        Directory "classes"
            [
                File "notes-fpr.txt" 200 TextFile,
                File "presentation.jpg" 150 Image,
                File "first_test.hs" 20 SourceCode
            ]
    ]

countFilesInDir :: [Entry] -> Int
countFilesInDir [x] = 1
countFilesInDir (x:xs) = 1 + countFilesInDir xs
countFilesInDir [Directory _ entries] = countFilesInDir entries
countFilesInDir [Directory _ []] = 0

countFiles :: Entry -> Int
countFiles (File _ _ _) = 1
countFiles (Directory _ entries) = countFilesInDir entries