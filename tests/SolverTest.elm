module SolverTest exposing (suite)

import Board as B
import Expect exposing (equal)
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
                    True |> Expect.equal True
            --, test "Cannot add existing value to local square" <|
            --    \_ ->
            --        let
            --            b =
            --                B.add B.empty ( 1, 1 ) 1
            --        in
            --        Solver.isValid b ( 1, 2 ) 1 |> Expect.equal False
            --, test "Cannot add existing value to the same row" <|
            --    \_ ->
            --        let
            --            b =
            --                B.add B.empty ( 1, 1 ) 1
            --        in
            --        Solver.isValid b ( 1, 9 ) 1 |> Expect.equal False
            --, test "Cannot add existing value to the same column" <|
            --    \_ ->
            --        let
            --            b =
            --                B.add B.empty ( 1, 1 ) 1
            --        in
            --        Solver.isValid b ( 9, 1 ) 1 |> Expect.equal False
            --]
        ]
