module SocketParserTest exposing (suite)

import Dict
import Expect
import SocketParser exposing (Fragment(..), Pattern, PatternDefinition, allSides, empty, parse, parseFragment, parsePattern)
import Test exposing (Test, describe, test)


example1d : String
example1d =
    "| X |"


examplePattern1d : String
examplePattern1d =
    "| a |"


examplePattern1dDef : PatternDefinition
examplePattern1dDef =
    { positionMapping = Dict.fromList [ ( 'a', ( 1, 1 ) ) ]
    , lines = [ examplePattern1d ]
    }


example3d : String
example3d =
    String.join "\n" <|
        [ "          ▁▁▁▁▁▁    "
        , "         ╱ X X ╱    "
        , "        ╱ X X ╱     "
        , "        ▔▔▔▔▔▔      "
        , "          ┌───┐     "
        , "    ╱ ▏   │X X│  ╱ ▏"
        , "   ╱  ▏   │X X│ ╱  ▏"
        , "  ╱ X ▏   └───┘╱ X ▏"
        , " ╱  X╱        ╱  X╱ "
        , " ▏X ╱┌───┐    ▏X ╱  "
        , " ▏X╱ │X X│    ▏X╱   "
        , " ▏╱  │X X│    ▏╱    "
        , "     └───┘          "
        , "     ▁▁▁▁▁▁         "
        , "    ╱ X X ╱         "
        , "   ╱ X X ╱          "
        , "   ▔▔▔▔▔▔           "
        , "                    "
        ]


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
                              , socketPos = ( 1, 1 )
                              }
                            ]
                    in
                    Expect.equal
                        expectedPattern
                        (parsePattern examplePattern1dDef)
            ]
        , describe "the parse function"
            [ test "returns an empty SocketSet for the 1d example" <|
                \_ ->
                    Expect.equal
                        (Ok empty)
                        (parse example1d)
            , test "returns an empty SocketSet for the 3d example" <|
                \_ ->
                    Expect.equal
                        (Ok empty)
                        (parse example3d)
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
                            (parseFragment "X")
                ]
            ]
        ]
