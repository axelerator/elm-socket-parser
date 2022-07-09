module SocketParserTest exposing (..)

import Expect
import SocketParser exposing (empty, parse)
import Test exposing (..)


example1d =
    "| X |"


example3d =
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
            [ test "returns an empty SocketSet" <|
                \_ ->
                    Expect.equal
                        (Ok empty)
                        (parse "")
            ]
        ]
