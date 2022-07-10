module SocketParserTest exposing (suite)

import Dict
import Expect
import SocketParser
    exposing
        ( Axis(..)
        , Fragment(..)
        , Pattern
        , PatternDefinition
        , Pos(..)
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
import Test exposing (Test, describe, test)


example1d : String
example1d =
    "| X |"


examplePattern1d : String
examplePattern1d =
    "| a |"


examplePattern1dDef : PatternDefinition
examplePattern1dDef =
    { positionMapping = Dict.fromList [ ( 'a', ( Top, P3 ) ) ]
    , lines = [ examplePattern1d ]
    }


examplePattern3dDef : PatternDefinition
examplePattern3dDef =
    { positionMapping = example3dMapping
    , lines = example3dPattern
    }


example3dMapping : Dict.Dict Char ( Side, Pos )
example3dMapping =
    Dict.fromList
        [ ( 'a', ( Top, P0 ) )
        , ( 'b', ( Top, P1 ) )
        , ( 'c', ( Top, P2 ) )
        , ( 'd', ( Top, P3 ) )
        , ( 'e', ( Front, P0 ) )
        , ( 'f', ( Front, P1 ) )
        , ( 'g', ( Front, P2 ) )
        , ( 'h', ( Front, P3 ) )
        , ( 'i', ( Bottom, P0 ) )
        , ( 'j', ( Bottom, P1 ) )
        , ( 'k', ( Bottom, P2 ) )
        , ( 'l', ( Bottom, P3 ) )
        , ( 'm', ( Back, P0 ) )
        , ( 'n', ( Back, P1 ) )
        , ( 'o', ( Back, P2 ) )
        , ( 'p', ( Back, P3 ) )
        , ( 'q', ( Left, P0 ) )
        , ( 'r', ( Left, P1 ) )
        , ( 's', ( Left, P2 ) )
        , ( 't', ( Left, P3 ) )
        , ( 'u', ( Right, P0 ) )
        , ( 'v', ( Right, P1 ) )
        , ( 'w', ( Right, P2 ) )
        , ( 'x', ( Right, P3 ) )
        ]


example3dPattern : List String
example3dPattern =
    [ "          ▁▁▁▁▁▁    "
    , "         ╱ a b ╱    "
    , "        ╱ c d ╱     "
    , "        ▔▔▔▔▔▔      "
    , "          ┌───┐     "
    , "    ╱ ▏   │m n│  ╱ ▏"
    , "   ╱  ▏   │o p│ ╱  ▏"
    , "  ╱ r ▏   └───┘╱ v ▏"
    , " ╱  t╱        ╱  x╱ "
    , " ▏q ╱┌───┐    ▏u ╱  "
    , " ▏s╱ │e f│    ▏w╱   "
    , " ▏╱  │g h│    ▏╱    "
    , "     └───┘          "
    , "     ▁▁▁▁▁▁         "
    , "    ╱ i j ╱         "
    , "   ╱ k l ╱          "
    , "   ▔▔▔▔▔▔           "
    , "                    "
    ]


example3d : List String
example3d =
    [ "          ▁▁▁▁▁▁    "
    , "         ╱ A B ╱    "
    , "        ╱ C D ╱     "
    , "        ▔▔▔▔▔▔      "
    , "          ┌───┐     "
    , "    ╱ ▏   │A B│  ╱ ▏"
    , "   ╱  ▏   │C D│ ╱  ▏"
    , "  ╱ B ▏   └───┘╱ B ▏"
    , " ╱  D╱        ╱  D╱ "
    , " ▏A ╱┌───┐    ▏A ╱  "
    , " ▏C╱ │A B│    ▏C╱   "
    , " ▏╱  │C D│    ▏╱    "
    , "     └───┘          "
    , "     ▁▁▁▁▁▁         "
    , "    ╱ A B ╱         "
    , "   ╱ C D ╱          "
    , "   ▔▔▔▔▔▔           "
    , "                    "
    ]


example3dFull : List String
example3dFull =
    [ "          ▁▁▁▁▁▁    "
    , "         ╱ A B ╱    "
    , "        ╱ C D ╱     "
    , "        ▔▔▔▔▔▔      "
    , "          ┌───┐     "
    , "    ╱ ▏   │M N│  ╱ ▏"
    , "   ╱  ▏   │O P│ ╱  ▏"
    , "  ╱ R ▏   └───┘╱ V ▏"
    , " ╱  T╱        ╱  X╱ "
    , " ▏Q ╱┌───┐    ▏U ╱  "
    , " ▏S╱ │E F│    ▏W╱   "
    , " ▏╱  │G H│    ▏╱    "
    , "     └───┘          "
    , "     ▁▁▁▁▁▁         "
    , "    ╱ I J ╱         "
    , "   ╱ K L ╱          "
    , "   ▔▔▔▔▔▔           "
    , "                    "
    ]


example3dFullRotatedY : List String
example3dFullRotatedY =
    [ "          ▁▁▁▁▁▁    "
    , "         ╱ V X ╱    "
    , "        ╱ U W ╱     "
    , "        ▔▔▔▔▔▔      "
    , "          ┌───┐     "
    , "    ╱ ▏   │N P│  ╱ ▏"
    , "   ╱  ▏   │M O│ ╱  ▏"
    , "  ╱ B ▏   └───┘╱ J ▏"
    , " ╱  A╱        ╱  I╱ "
    , " ▏D ╱┌───┐    ▏L ╱  "
    , " ▏C╱ │F H│    ▏K╱   "
    , " ▏╱  │E G│    ▏╱    "
    , "     └───┘          "
    , "     ▁▁▁▁▁▁         "
    , "    ╱ R T ╱         "
    , "   ╱ Q S ╱          "
    , "   ▔▔▔▔▔▔           "
    , "                    "
    ]


example3dFullRotatedX : List String
example3dFullRotatedX =
    [ "          ▁▁▁▁▁▁    "
    , "         ╱ E F ╱    "
    , "        ╱ G H ╱     "
    , "        ▔▔▔▔▔▔      "
    , "          ┌───┐     "
    , "    ╱ ▏   │C D│  ╱ ▏"
    , "   ╱  ▏   │A B│ ╱  ▏"
    , "  ╱ Q ▏   └───┘╱ U ▏"
    , " ╱  R╱        ╱  V╱ "
    , " ▏S ╱┌───┐    ▏W ╱  "
    , " ▏T╱ │K L│    ▏X╱   "
    , " ▏╱  │I J│    ▏╱    "
    , "     └───┘          "
    , "     ▁▁▁▁▁▁         "
    , "    ╱ M N ╱         "
    , "   ╱ O P ╱          "
    , "   ▔▔▔▔▔▔           "
    , "                    "
    ]


example3dFullRotatedZ : List String
example3dFullRotatedZ =
    [ "          ▁▁▁▁▁▁    "
    , "         ╱ B D ╱    "
    , "        ╱ A C ╱     "
    , "        ▔▔▔▔▔▔      "
    , "          ┌───┐     "
    , "    ╱ ▏   │V U│  ╱ ▏"
    , "   ╱  ▏   │X W│ ╱  ▏"
    , "  ╱ N ▏   └───┘╱ F ▏"
    , " ╱  P╱        ╱  H╱ "
    , " ▏M ╱┌───┐    ▏E ╱  "
    , " ▏O╱ │R Q│    ▏G╱   "
    , " ▏╱  │T S│    ▏╱    "
    , "     └───┘          "
    , "     ▁▁▁▁▁▁         "
    , "    ╱ J L ╱         "
    , "   ╱ I K ╱          "
    , "   ▔▔▔▔▔▔           "
    , "                    "
    ]


expectedSocketSet : SocketSet
expectedSocketSet =
    { top =
        mkSocket
            [ ( P0, AFragment )
            , ( P1, BFragment )
            , ( P2, CFragment )
            , ( P3, DFragment )
            ]
    , front =
        mkSocket
            [ ( P0, AFragment )
            , ( P1, BFragment )
            , ( P2, CFragment )
            , ( P3, DFragment )
            ]
    , bottom =
        mkSocket
            [ ( P0, AFragment )
            , ( P1, BFragment )
            , ( P2, CFragment )
            , ( P3, DFragment )
            ]
    , back =
        mkSocket
            [ ( P0, AFragment )
            , ( P1, BFragment )
            , ( P2, CFragment )
            , ( P3, DFragment )
            ]
    , left =
        mkSocket
            [ ( P1, BFragment )
            , ( P3, DFragment )
            , ( P0, AFragment )
            , ( P2, CFragment )
            ]
    , right =
        mkSocket
            [ ( P1, BFragment )
            , ( P3, DFragment )
            , ( P0, AFragment )
            , ( P2, CFragment )
            ]
    }


suite : Test
suite =
    describe "The SocketParser module"
        [ describe "the parsePattern function"
            [ test "works for the simple example" <|
                \_ ->
                    let
                        expectedPattern : Pattern
                        expectedPattern =
                            [ { patternPos = ( 2, 0 )
                              , socketPos = ( Top, P3 )
                              }
                            ]
                    in
                    Expect.equal
                        expectedPattern
                        (parsePattern examplePattern1dDef)
            , test "works for the 3d example" <|
                \_ ->
                    Expect.equal
                        24
                        (List.length (parsePattern examplePattern3dDef))
            ]
        , describe "the parse function"
            [ test "can parse the 1d" <|
                \_ ->
                    let
                        pattern : Pattern
                        pattern =
                            parsePattern examplePattern1dDef
                    in
                    Expect.ok
                        (parse pattern [ example1d ])
            , test "can parse the 3d example" <|
                \_ ->
                    let
                        pattern : Pattern
                        pattern =
                            parsePattern examplePattern3dDef
                    in
                    Expect.ok
                        (parse pattern example3d)
            , describe "the parsed fragments"
                [ test "match on the top" (expectMatch Top)
                , test "match on the front" (expectMatch Front)
                , test "match on the bottom" (expectMatch Bottom)
                , test "match on the back" (expectMatch Back)
                , test "match on the left" (expectMatch Left)
                , test "match on the right" (expectMatch Right)
                ]
            ]
        , describe "the rotateSet function"
            [ test "returns expected result when rotating around Z axis" <|
                \_ ->
                    let
                        pattern : Pattern
                        pattern =
                            parsePattern examplePattern3dDef
                    in
                    case ( parse pattern example3dFull, parse pattern example3dFullRotatedZ ) of
                        ( Ok input, Ok expected ) ->
                            Expect.equal
                                expected
                                (rotateSet ZAxis input)

                        ( Err e, _ ) ->
                            Expect.fail <| "Unable to parse input " ++ e

                        ( _, Err e ) ->
                            Expect.fail <| "Unable to parse expected " ++ e
            , test "returns expected result when rotating around X axis" <|
                \_ ->
                    let
                        pattern : Pattern
                        pattern =
                            parsePattern examplePattern3dDef
                    in
                    case ( parse pattern example3dFull, parse pattern example3dFullRotatedX ) of
                        ( Ok input, Ok expected ) ->
                            Expect.equal
                                expected
                                (rotateSet XAxis input)

                        ( Err e, _ ) ->
                            Expect.fail <| "Unable to parse input " ++ e

                        ( _, Err e ) ->
                            Expect.fail <| "Unable to parse expected " ++ e
            , test "returns expected result when rotating around Y axis" <|
                \_ ->
                    let
                        pattern : Pattern
                        pattern =
                            parsePattern examplePattern3dDef
                    in
                    case ( parse pattern example3dFull, parse pattern example3dFullRotatedY ) of
                        ( Ok input, Ok expected ) ->
                            Expect.equal
                                expected
                                (rotateSet YAxis input)

                        ( Err e, _ ) ->
                            Expect.fail <| "Unable to parse input " ++ e

                        ( _, Err e ) ->
                            Expect.fail <| "Unable to parse expected " ++ e
            ]
        , describe "the allSides function"
            [ test "returns six sides" <|
                \_ ->
                    Expect.equal
                        6
                        (List.length allSides)
            , describe "the parseFragment function"
                [ test "can parse X " <|
                    \_ ->
                        Expect.equal
                            (Ok XFragment)
                            (parseFragment 'X')
                ]
            ]
        ]


parsed3dExample : Result String SocketSet
parsed3dExample =
    parse (parsePattern examplePattern3dDef) example3d


expectMatch : Side -> () -> Expect.Expectation
expectMatch side _ =
    case parsed3dExample of
        Ok sides ->
            let
                accessor : SocketSet -> Socket
                accessor =
                    case side of
                        Top ->
                            .top

                        Front ->
                            .front

                        Bottom ->
                            .bottom

                        Back ->
                            .back

                        Left ->
                            .left

                        Right ->
                            .right
            in
            Expect.equal
                (accessor expectedSocketSet)
                (accessor sides)

        Err e ->
            Expect.fail e
