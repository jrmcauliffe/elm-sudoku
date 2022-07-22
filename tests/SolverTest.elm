module SolverTest exposing (suite)

import Board as B
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Solver
import Test exposing (..)


suite : Test
suite =
    describe "Solver"
        [ describe "Solver.isValid"
            -- Nest as many descriptions as you like.
            [ test "Any value is valid in empty board" <|
                \_ ->
                    Solver.isValid B.empty ( 1, 1 ) 9 |> Expect.true "Can add any value to empty board"

            -- Expect.equal is designed to be used in pipeline style, like this.
            ]
        ]
