module Solver exposing (Assessment(..), assess)

import Board exposing (Board, Entry, Position, Puzzle, Value, getCol, getRow, getSq)
import Dict exposing (Dict)


type Assessment
    = Incorrect
    | PossiblyCorrect
    | Correct


assess : Puzzle -> List Entry -> Assessment
assess puzzle entries =
    let
        combined : Dict Position Value
        combined =
            entries |> Dict.fromList |> Dict.union puzzle.initial

        assessSquare : Position -> Value -> Bool
        assessSquare pos val =
            let
                checkOnlyValInGroup : (Position -> List Position) -> Bool
                checkOnlyValInGroup getGroup =
                    (getGroup pos |> List.filterMap (\pp -> combined |> Dict.get pp) |> List.filter (\pp -> pp == val) |> List.length) == 1
            in
            checkOnlyValInGroup (getCol puzzle.rank) && checkOnlyValInGroup (getRow puzzle.rank) && checkOnlyValInGroup (getSq puzzle.rank)
    in
    case combined |> Dict.toList |> List.all (\( p, v ) -> assessSquare p v) of
        True ->
            case (combined |> Dict.size) == (puzzle.rank * puzzle.rank * puzzle.rank * puzzle.rank) of
                True ->
                    Correct

                False ->
                    PossiblyCorrect

        False ->
            Incorrect


solve : Puzzle -> ( Puzzle, List Entry )
solve p =
    ( p, [] )


type alias SolveState =
    Dict Position (List Value)


initSolveState : Puzzle -> SolveState
initSolveState p =
    let
        -- List of all possible values for a square
        extent : List Value
        extent =
            p.rank * p.rank |> List.range 1

        -- Total solution space
        keys : SolveState
        keys =
            extent |> List.concatMap (\x -> extent |> List.map (\y -> ( ( x, y ), List.range 1 p.rank ))) |> Dict.fromList
    in
    keys |> Dict.union (p.initial |> Dict.map (\_ v -> [ v ]))



--import Board exposing (..)
--import Dict
--import Set
--
--
--remaining : Board -> Set.Set Position
--remaining b =
--    let
--        rank =
--            b |> Dict.size |> toFloat |> sqrt |> truncate
--
--        d =
--            List.range 1 (rank * rank)
--
--        allKeys : Set.Set Position
--        allKeys =
--            d |> List.concatMap (\x -> d |> List.map (\y -> ( x, y ))) |> Set.fromList
--
--        usedKeys : Set.Set Position
--        usedKeys =
--            b |> Dict.keys |> Set.fromList
--    in
--    usedKeys |> Set.diff allKeys
--
--
--unsolved : Position -> Board -> Set.Set (Maybe Value)
--unsolved ( r, c ) b =
--    let
--        rank =
--            b |> Dict.size |> toFloat |> sqrt |> truncate
--
--        existingVals =
--            getSq b ( r, c ) |> List.append (getRow b r) |> List.append (getCol b c) |> Set.fromList
--
--        allVals =
--            List.range 1 (rank * rank) |> Set.fromList
--    in
--    existingVals |> Set.diff allVals
--
--
--solve : Board -> Board
--solve b =
--    let
--        r =
--            b |> remaining
--
--        lowPos =
--            r |> Set.toList |> List.map (\p -> ( p, unsolved p b |> Set.toList )) |> List.sortBy (\( _, y ) -> y |> List.length) |> List.head
--    in
--    case lowPos of
--        Just ( pnt, val ) ->
--            Board.add b pnt (val |> List.head |> Maybe.withDefault 0)
--
--        Nothing ->
--            b
--
--
--isValid : Board -> Position -> Value -> Bool
--isValid b p v =
