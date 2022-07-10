module SocketParser exposing
    ( Fragment(..)
    , Pos
    , Side(..)
    , Socket
    , SocketSet
    , allSides
    , empty
    , parse
    , parseFragment
    )


type Fragment
    = XFragment


parseFragment : String -> Result String Fragment
parseFragment s =
    case s of
        "X" ->
            Ok XFragment

        _ ->
            Err <| "Unknown fragment: " ++ s


type alias Pos =
    ( Int, Int )


type alias Socket =
    List ( Pos, Fragment )


type Side
    = Top
    | Front
    | Bottom
    | Back
    | Left
    | Right


allSides : List Side
allSides =
    [ Top, Front, Bottom, Back, Left, Right ]


type alias SocketSet =
    { top : Socket
    , front : Socket
    , bottom : Socket
    , back : Socket
    , left : Socket
    , right : Socket
    }


parse : String -> Result String SocketSet
parse _ =
    Ok empty


empty : SocketSet
empty =
    { top = []
    , front = []
    , bottom = []
    , back = []
    , left = []
    , right = []
    }
