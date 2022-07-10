module SocketParser exposing
    ( Axis(..)
    , Fragment(..)
    , Pattern
    , PatternDefinition
    , Pos(..)
    , PositionedFragment
    , Side(..)
    , Socket
    , SocketSet
    , allSides
    , mkSocket
    , parse
    , parseFragment
    , parsePattern
    , rotateSet
    )

import Dict exposing (Dict)
import Maybe.Extra exposing (values)
import Result.Extra
import String exposing (fromInt)


type Fragment
    = AFragment
    | BFragment
    | CFragment
    | DFragment
    | EFragment
    | FFragment
    | GFragment
    | HFragment
    | IFragment
    | JFragment
    | KFragment
    | LFragment
    | MFragment
    | NFragment
    | OFragment
    | PFragment
    | QFragment
    | RFragment
    | SFragment
    | TFragment
    | UFragment
    | VFragment
    | WFragment
    | XFragment


type alias PatternDefinition =
    { positionMapping : Dict Char ( Side, Pos )
    , lines : List String
    }


type alias PositionedFragment =
    { patternPos : ( Int, Int )
    , socketPos : ( Side, Pos )
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


parseFragment : Char -> Result String Fragment
parseFragment s =
    case s of
        'A' ->
            Ok AFragment

        'B' ->
            Ok BFragment

        'C' ->
            Ok CFragment

        'D' ->
            Ok DFragment

        'E' ->
            Ok EFragment

        'F' ->
            Ok FFragment

        'G' ->
            Ok GFragment

        'H' ->
            Ok HFragment

        'I' ->
            Ok IFragment

        'J' ->
            Ok JFragment

        'K' ->
            Ok KFragment

        'L' ->
            Ok LFragment

        'M' ->
            Ok MFragment

        'N' ->
            Ok NFragment

        'O' ->
            Ok OFragment

        'P' ->
            Ok PFragment

        'Q' ->
            Ok QFragment

        'R' ->
            Ok RFragment

        'S' ->
            Ok SFragment

        'T' ->
            Ok TFragment

        'U' ->
            Ok UFragment

        'V' ->
            Ok VFragment

        'W' ->
            Ok WFragment

        'X' ->
            Ok XFragment

        _ ->
            Err <| "Unknown fragment: " ++ String.fromList [ s ]


type Pos
    = P0
    | P1
    | P2
    | P3


posToComparable : Pos -> Int
posToComparable p =
    case p of
        P0 ->
            0

        P1 ->
            1

        P2 ->
            2

        P3 ->
            3


type Socket
    = Socket (List ( Pos, Fragment ))


mkSocket : List ( Pos, Fragment ) -> Socket
mkSocket ps =
    Socket <| List.sortBy (\( p, _ ) -> posToComparable p) ps


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


parse : Pattern -> List String -> Result String SocketSet
parse positionedFragments lines =
    let
        forSide : Side -> Socket
        forSide side =
            let
                hasSide : ( ( Side, b ), a ) -> Bool
                hasSide ( ( s, _ ), _ ) =
                    s == side

                dropSide : ( ( a, b ), c ) -> ( b, c )
                dropSide ( ( _, pos ), fragment ) =
                    ( pos, fragment )
            in
            mkSocket <| List.map dropSide <| List.filter hasSide parsedFragments

        lookUp : PositionedFragment -> Result String ( ( Side, Pos ), Fragment )
        lookUp { patternPos, socketPos } =
            let
                ( x, y ) =
                    patternPos
            in
            case List.head <| List.drop y lines of
                Just line ->
                    case List.head <| List.drop x <| String.toList line of
                        Just char ->
                            case parseFragment char of
                                Ok fragment ->
                                    Ok ( socketPos, fragment )

                                Err e ->
                                    Err <| "At (" ++ fromInt x ++ "," ++ fromInt y ++ ")" ++ e

                        Nothing ->
                            Err "Out of bounds"

                Nothing ->
                    Err "Out of bounds"

        parsedFragmentResults : List (Result String ( ( Side, Pos ), Fragment ))
        parsedFragmentResults =
            List.map lookUp positionedFragments

        ( parsedFragments, errors ) =
            Result.Extra.partition parsedFragmentResults
    in
    if List.isEmpty errors then
        Ok
            { top = forSide Top
            , front = forSide Front
            , bottom = forSide Bottom
            , back = forSide Back
            , left = forSide Left
            , right = forSide Right
            }

    else
        Err <| String.join ", " errors


type Axis
    = XAxis
    | YAxis
    | ZAxis


rotateSide : Axis -> Side -> Socket -> Socket
rotateSide axis side (Socket ps) =
    let
        rotFunc : Side -> Pos -> Pos
        rotFunc =
            case axis of
                XAxis ->
                    rotateX

                YAxis ->
                    rotateY

                ZAxis ->
                    rotateZ

        update : ( Pos, Fragment ) -> ( Pos, Fragment )
        update ( pos, fragment ) =
            ( rotFunc side pos, fragment )
    in
    mkSocket <| List.map update ps


rotateSet : Axis -> SocketSet -> SocketSet
rotateSet axis s =
    case axis of
        ZAxis ->
            { top = rotateSide axis Top s.top
            , front = rotateSide axis Left s.left
            , bottom = rotateSide axis Bottom s.bottom
            , back = rotateSide axis Right s.right
            , left = rotateSide axis Back s.back
            , right = rotateSide axis Front s.front
            }

        XAxis ->
            { top = rotateSide axis Front s.front
            , front = rotateSide axis Bottom s.bottom
            , bottom = rotateSide axis Back s.back
            , back = rotateSide axis Top s.top
            , left = rotateSide axis Left s.left
            , right = rotateSide axis Right s.right
            }

        YAxis ->
            { top = rotateSide axis Right s.right
            , front = rotateSide axis Front s.front
            , bottom = rotateSide axis Left s.left
            , back = rotateSide axis Back s.back
            , left = rotateSide axis Top s.top
            , right = rotateSide axis Bottom s.bottom
            }


rotateZ : Side -> Pos -> Pos
rotateZ side pos =
    case ( side, pos ) of
        ( Top, P0 ) ->
            P2

        ( Top, P1 ) ->
            P0

        ( Top, P2 ) ->
            P3

        ( Top, P3 ) ->
            P1

        ( Bottom, P0 ) ->
            P2

        ( Bottom, P1 ) ->
            P0

        ( Bottom, P2 ) ->
            P3

        ( Bottom, P3 ) ->
            P1

        ( Front, P0 ) ->
            P0

        ( Front, P1 ) ->
            P1

        ( Front, P2 ) ->
            P2

        ( Front, P3 ) ->
            P3

        ( Back, P0 ) ->
            P0

        ( Back, P1 ) ->
            P1

        ( Back, P2 ) ->
            P2

        ( Back, P3 ) ->
            P3

        ( Left, P0 ) ->
            P1

        ( Left, P1 ) ->
            P0

        ( Left, P2 ) ->
            P3

        ( Left, P3 ) ->
            P2

        ( Right, P0 ) ->
            P1

        ( Right, P1 ) ->
            P0

        ( Right, P2 ) ->
            P3

        ( Right, P3 ) ->
            P2


rotateX : Side -> Pos -> Pos
rotateX side pos =
    case ( side, pos ) of
        ( Top, P0 ) ->
            P2

        ( Top, P1 ) ->
            P3

        ( Top, P2 ) ->
            P0

        ( Top, P3 ) ->
            P1

        ( Bottom, P0 ) ->
            P2

        ( Bottom, P1 ) ->
            P3

        ( Bottom, P2 ) ->
            P0

        ( Bottom, P3 ) ->
            P1

        ( Front, P0 ) ->
            P0

        ( Front, P1 ) ->
            P1

        ( Front, P2 ) ->
            P2

        ( Front, P3 ) ->
            P3

        ( Back, P0 ) ->
            P0

        ( Back, P1 ) ->
            P1

        ( Back, P2 ) ->
            P2

        ( Back, P3 ) ->
            P3

        ( Left, P0 ) ->
            P1

        ( Left, P1 ) ->
            P3

        ( Left, P2 ) ->
            P0

        ( Left, P3 ) ->
            P2

        ( Right, P0 ) ->
            P1

        ( Right, P1 ) ->
            P3

        ( Right, P2 ) ->
            P0

        ( Right, P3 ) ->
            P2


rotateY : Side -> Pos -> Pos
rotateY side pos =
    case ( side, pos ) of
        ( Top, P0 ) ->
            P3

        ( Top, P1 ) ->
            P1

        ( Top, P2 ) ->
            P2

        ( Top, P3 ) ->
            P0

        ( Bottom, P0 ) ->
            P3

        ( Bottom, P1 ) ->
            P1

        ( Bottom, P2 ) ->
            P2

        ( Bottom, P3 ) ->
            P0

        ( Front, P0 ) ->
            P2

        ( Front, P1 ) ->
            P0

        ( Front, P2 ) ->
            P3

        ( Front, P3 ) ->
            P1

        ( Back, P0 ) ->
            P2

        ( Back, P1 ) ->
            P0

        ( Back, P2 ) ->
            P3

        ( Back, P3 ) ->
            P1

        ( Left, P0 ) ->
            P2

        ( Left, P1 ) ->
            P0

        ( Left, P2 ) ->
            P3

        ( Left, P3 ) ->
            P1

        ( Right, P0 ) ->
            P2

        ( Right, P1 ) ->
            P0

        ( Right, P2 ) ->
            P3

        ( Right, P3 ) ->
            P1
