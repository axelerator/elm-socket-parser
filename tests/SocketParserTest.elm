module SocketParserTest exposing (suite)

import Expect
import SocketParser exposing (allSides, empty, parse)
import Test exposing (Test, describe, test)


example1d : String
example1d =
    "| X |"


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
        [ describe "the parse function"
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
            ]
        ]
