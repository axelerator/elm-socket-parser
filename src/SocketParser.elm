module SocketParser exposing
    ( Fragment(..)
    , Pattern
    , PatternDefinition
    , Pos
    , PositionedFragment
    , Side(..)
    , Socket
    , SocketSet
    , allSides
    , empty
    , parse
    , parseFragment
    , parsePattern
    )

import Dict exposing (Dict)
import Maybe.Extra exposing (values)


type Fragment
    = XFragment


type alias PatternDefinition =
    { positionMapping : Dict Char Pos
    , lines : List String
    }


type alias PositionedFragment =
    { patternPos : Pos
    , socketPos : Pos
    }


type alias Pattern =
    List PositionedFragment


parsePattern : PatternDefinition -> Pattern
parsePattern { positionMapping, lines } =
    let
        readPatterns : Int -> List Char -> List PositionedFragment
        readPatterns row characters =
            values <| List.indexedMap (readChar row) characters

        readChar : Int -> Int -> Char -> Maybe PositionedFragment
        readChar row column c =
            case Dict.get c positionMapping of
                Just pos ->
                    Just
                        { patternPos = ( column, row )
                        , socketPos = pos
                        }

                Nothing ->
                    Nothing
    in
    List.concat <| List.indexedMap readPatterns <| List.map String.toList lines


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
