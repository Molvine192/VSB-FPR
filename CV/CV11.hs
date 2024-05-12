data Point = Point {x :: Int, y :: Int}
data Position = Position {upperLeftCorner :: Point, width :: Int, height :: Int}

data Component = TextBox {name :: String, position :: Position, text :: String} |
                 Button {name :: String, position :: Position, text :: String} |
                 Container {name :: String, children :: [Component]}

gui :: Component
gui = Container "Moje Aplikace" [
    Container "Menu" [
        Button "btn_new" (Position (Point 10 10) 100 20) "New",
        Button "btn_save" (Position (Point 120 10) 100 20) "Save"
    ],
    Container "Body" [
        TextBox "tb_1" (Position (Point 10 50) 100 20) "Muj Text"
    ],
    Container "Footer" []
    ]

instance Show Position where
    show (Position (Point x y) a b) = "[" ++ show x ++ "," ++ show y ++ "]"

instance Show Component where
    show comp = showComponent comp 0 where
        showComponent (Container name children) tabs = "\n" ++ replicate tabs '\t' ++ "Container " ++ name ++ concatMap (`showComponent` (tabs+1)) children
        showComponent (Button name position title) tabs = "\n" ++ replicate tabs '\t' ++ "Button " ++ show position ++ " " ++ title
        showComponent (TextBox name position title) tabs = "\n" ++ replicate tabs '\t' ++ "TextBox "  ++ show position ++ " " ++ title

identity :: Component -> Component
identity (Container n ch) = Container n (map identity ch)
identity (Button n p t) = Button n p t
identity (TextBox n p t) = TextBox n p t

insertInto :: Component -> String -> Component -> Component
insertInto (Container n ch) name comp =
    let
        ch_new = if n == name then ch ++ [comp] else ch
    in Container n [insertInto c name comp | c <- ch_new]
insertINto c _ _ = c