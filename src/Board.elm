module Board exposing (..)

import Dict
import Html exposing (Html)
import Svg
import Svg.Attributes as Att


empty : Board
empty =
    { board = Dict.empty, rank = 3 }


type alias Row =
    Int


type alias Col =
    Int


type alias Position =
    ( Row, Col )


type alias Value =
    Int


type alias Board =
    { board : Dict.Dict Position (List Value)
    , rank : Int
    }


newBoard : Int -> Board
newBoard rank =
    let
        b =
            List.range 1 (rank * rank)
                |> List.concatMap
                    (\r ->
                        List.range 1 (rank * rank)
                            |> List.map (\c -> ( ( c, r ), List.range 1 (rank * rank) ))
                    )
                |> Dict.fromList
    in
    { board = b, rank = rank }


add : Board -> Position -> Value -> Board
add b p v =
    { b | board = Dict.insert p [ v ] b.board }


importBoard : String -> Result String Board
importBoard s =
    let
        l =
            s
                |> String.filter Char.isDigit
                |> String.toList
                |> List.map String.fromChar
                |> List.map String.toInt
                |> List.map
                    (\p ->
                        case p of
                            Just 0 ->
                                Nothing

                            _ ->
                                p
                    )

        rank =
            l |> List.length |> toFloat |> sqrt |> sqrt |> truncate

        indexToRC : Int -> ( Int, Int )
        indexToRC index =
            ( ((index - 1) // (rank * rank)) + 1, modBy (rank * rank) (index - 1) + 1 )

        idx =
            List.length l |> List.range 1 |> List.map indexToRC

        pairs =
            List.map2 (\a -> \b -> ( a, b )) idx l

        isSquare =
            (l |> List.length |> toFloat |> sqrt |> sqrt) == (rank |> toFloat) && (rank /= 0)

        bd =
            pairs
                |> List.map
                    (\( ( x, y ), v ) ->
                        case v of
                            Just value ->
                                ( ( x, y ), [ value ] )

                            Nothing ->
                                ( ( x, y ), List.range 1 rank )
                    )
                |> Dict.fromList
    in
    if isSquare then
        { board = bd, rank = rank } |> Ok

    else
        Err "Invalid board"


getRow : Int -> Board -> List Position
getRow r b =
    b.board |> Dict.filter (\k -> \_ -> (k |> Tuple.first) == r) |> Dict.keys


getCol : Int -> Board -> List Position
getCol c b =
    b.board |> Dict.filter (\k -> \_ -> (k |> Tuple.second) == c) |> Dict.keys


getSq : Board -> Position -> List Position
getSq b ( r, c ) =
    let
        base =
            \i -> ((i - 1) // b.rank) * b.rank

        rowNums =
            List.range 1 b.rank |> List.map ((+) (base r))

        colNums =
            List.range 1 b.rank |> List.map ((+) (base c))
    in
    colNums |> List.concatMap (\cc -> rowNums |> List.map (\rr -> ( rr, cc )))


getPeers : Position -> Board -> List Position
getPeers ( r, c ) b =
    getCol c b ++ getRow r b ++ getSq b ( r, c )


renderDigit : Position -> List Value -> Svg.Svg msg
renderDigit p v =
    let
        colOffset =
            15

        rowOffset =
            38

        row =
            ((p |> Tuple.first) - 1) * 50 + rowOffset |> String.fromInt

        col =
            ((p |> Tuple.second) - 1) * 50 + colOffset |> String.fromInt

        val =
            case v of
                [ vv ] ->
                    String.fromInt vv

                _ ->
                    " "
    in
    Svg.text_
        [ Att.x col
        , Att.y row
        , Att.fill "black"
        , Att.style "font-family: Arial; font-size: 34; stroke: #000000; fill: #000000;"
        ]
        [ Svg.text val ]


renderLines : Int -> Int -> Int -> Int -> Int -> List (Svg.Svg msg)
renderLines rank size light heavy l =
    let
        lineWidth =
            case modBy rank l of
                0 ->
                    heavy |> String.fromInt

                _ ->
                    light |> String.fromInt

        lineSize =
            size * l |> String.fromInt

        boardSize =
            rank * rank * size |> String.fromInt
    in
    [ Svg.line
        [ Att.x1 lineSize
        , Att.y1 "0"
        , Att.x2 lineSize
        , Att.y2 boardSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth lineWidth
        ]
        []
    , Svg.line
        [ Att.x1 "0"
        , Att.y1 lineSize
        , Att.x2 boardSize
        , Att.y2 lineSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth lineWidth
        ]
        []
    ]


renderBoard : Board -> Html msg
renderBoard b =
    let
        -- edge of number box in pixels
        boxSize =
            50

        lineWeight =
            2

        heavyLineWeight =
            lineWeight * 2

        -- edge of board in pixels
        boardSize =
            b.rank * b.rank * boxSize

        board =
            List.range 1 (b.rank * b.rank)
                |> List.concatMap
                    (renderLines b.rank boxSize lineWeight heavyLineWeight)
                |> List.append ((b.board |> Dict.map renderDigit) |> Dict.values)
                |> List.append
                    [ Svg.rect
                        [ Att.x "0"
                        , Att.y "0"
                        , boardSize |> String.fromInt |> Att.width
                        , boardSize |> String.fromInt |> Att.height
                        , Att.fill "white"
                        , Att.stroke "black"
                        , heavyLineWeight |> String.fromInt |> Att.strokeWidth
                        ]
                        []
                    ]
                |> Svg.g [ Att.transform "translate(2,2)" ]

        viewbox =
            "0 0 " ++ (boardSize + heavyLineWeight |> String.fromInt) ++ " " ++ (boardSize + heavyLineWeight |> String.fromInt)
    in
    [ board ]
        |> Svg.svg
            [ boardSize |> String.fromInt |> Att.width
            , boardSize |> String.fromInt |> Att.height
            , viewbox |> Att.viewBox
            ]
