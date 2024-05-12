import Data.List

-- Uloha 1
data Attribute = Attribute {attrname :: String, attrvalue :: String} deriving (Show)
data Tag = Tag {tagname :: String, attributes :: [Attribute], tags :: [Tag]} deriving (Show)
data HTMLDocument = HTMLDocument {htmlTags :: [Tag]} deriving (Show)

htmlDoc :: HTMLDocument
htmlDoc = HTMLDocument [
        Tag "tag1" [
            Attribute "size" "12"
        ] [],
        Tag "tag2" [] [
            Tag "tag3" [] []
        ]
    ]



data Component = TextBox {name :: String, text :: String}
               | Button {name :: String, value :: String}
               | Container {name :: String, children :: [Component]}

gui :: Component
gui = Container "My App" [
        Container "Menu" [
            Button "btn_new" "New",
            Button "btn_open" "Open",
            Button "btn_close" "Close"
        ],
        Container "Body" [TextBox "textbox_1" "Some text goes here"],
        Container "Footer" []
    ]

-- Uloha 2
isTarget :: Component -> String -> Bool
isTarget (Container name _) target_name = name == target_name
isTarget (TextBox name _) target_name = name == target_name
isTarget (Button name _) target_name = name == target_name

printPath :: Component -> String -> String
printPath gui "" = []
printPath (TextBox name _) x = if name == x then name else []
printPath (Button name _) x = if name == x then name else []
printPath (Container name children) target = if any (`isTarget` target) children then name ++ " / " ++ target else concatMap (`printPath` target) children

-- Uloha 3
removeContainerFromContainerAtIndex :: Component -> String -> Int -> Component
removeContainerFromContainerAtIndex (Button n v) _ _ = Button n v
removeContainerFromContainerAtIndex (TextBox n v) _ _ = TextBox n v
-- removeContainerFromContainerAtIndex (Container name children) target_name index = if name == target_name then 

