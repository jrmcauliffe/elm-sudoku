module Solver exposing (solve)

import Board exposing (..)
import Dict
import Set


getRow : Board -> Int -> List Int
getRow b r =
    b |> Dict.filter (\k -> \_ -> (k |> Tuple.first) == r) |> Dict.values


getCol : Board -> Int -> List Int
getCol b r =
    b |> Dict.filter (\k -> \_ -> (k |> Tuple.second) == r) |> Dict.values


getSq : Board -> Position -> List Int
getSq b ( r, c ) =
    let
        base =
            \i -> ((i - 1) // dim) * dim

        rowNums =
            List.range 1 dim |> List.map ((+) (base r))

        colNums =
            List.range 1 dim |> List.map ((+) (base c))

        points =
            colNums |> List.concatMap (\cc -> rowNums |> List.map (\rr -> ( rr, cc )))
    in
    points |> List.filterMap (\v -> Dict.get v b)


remaining : Board -> Set.Set Position
remaining b =
    let
        d =
            List.range 1 (dim * dim)

        allKeys : Set.Set Position
        allKeys =
            d |> List.concatMap (\x -> d |> List.map (\y -> ( x, y ))) |> Set.fromList

        usedKeys : Set.Set Position
        usedKeys =
            b |> Dict.keys |> Set.fromList
    in
    usedKeys |> Set.diff allKeys


unSolved : Position -> Board -> Set.Set Int
unSolved ( r, c ) b =
    let
        existingVals =
            getSq b ( r, c ) |> List.append (getRow b r) |> List.append (getCol b c) |> Set.fromList

        allVals =
            List.range 1 (dim * dim) |> Set.fromList
    in
    existingVals |> Set.diff allVals


updateBoard : Board -> Position -> Int -> Board
updateBoard b p i =
    b |> Dict.insert p i


solve : Board -> Board
solve b =
    let
        r =
            b |> remaining

        lowPos =
            r |> Set.toList |> List.map (\p -> ( p, unSolved p b |> Set.toList )) |> List.sortBy (\( _, y ) -> y |> List.length) |> List.head
    in
    case lowPos of
        Just ( pnt, val ) ->
            updateBoard b pnt (val |> List.head |> Maybe.withDefault 0)

        Nothing ->
            b
