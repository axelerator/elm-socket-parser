module SocketParser exposing (..)


type alias Fragment =
    Int


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
