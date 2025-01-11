module Board exposing (..)

import Dict
import Html exposing (Html)
import Svg
import Svg.Attributes as Att


empty : Board
empty =
    { board = Dict.empty, rank = 9 }


type Msg
    = SolveMsg
    | TextMsg String


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
            List.range 1 rank
                |> List.concatMap
                    (\r ->
                        List.range 1 rank
                            |> List.map (\c -> ( ( c, r ), List.range 1 rank ))
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
            l |> List.length |> toFloat |> sqrt |> truncate

        indexToRC : Int -> ( Int, Int )
        indexToRC index =
            ( ((index - 1) // rank) + 1, modBy rank (index - 1) + 1 )

        idx =
            List.length l |> List.range 1 |> List.map indexToRC

        pairs =
            List.map2 (\a -> \b -> ( a, b )) idx l

        isSquare =
            (l |> List.length |> toFloat |> sqrt) == (rank |> toFloat) && (rank /= 0)

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


renderNum : Position -> List Value -> Svg.Svg Msg
renderNum p v =
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


myLines : Int -> Int -> Int -> List (Svg.Svg Msg)
myLines rank size l =
    let
        sWidth =
            case modBy rank l of
                0 ->
                    "4"

                _ ->
                    "2"

        lSize =
            size * l |> String.fromInt

        bSize =
            rank * rank * size |> String.fromInt
    in
    [ Svg.line
        [ Att.x1 lSize
        , Att.y1 "0"
        , Att.x2 lSize
        , Att.y2 bSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth sWidth
        ]
        []
    , Svg.line
        [ Att.x1 "0"
        , Att.y1 lSize
        , Att.x2 bSize
        , Att.y2 lSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth sWidth
        ]
        []
    ]


renderBoard : Board -> Html Msg
renderBoard b =
    let
        dim =
            b.rank |> toFloat |> sqrt |> truncate

        -- edge of box in pixels
        boxSize =
            50

        -- edge of board in pixels
        boardSize =
            dim * dim * boxSize

        grp =
            List.range 1 (dim * dim)
                |> List.concatMap
                    (myLines dim boxSize)
                |> List.append ((b.board |> Dict.map renderNum) |> Dict.values)
                |> List.append
                    [ Svg.rect
                        [ Att.x "0"
                        , Att.y "0"
                        , boardSize |> String.fromInt |> Att.width
                        , boardSize |> String.fromInt |> Att.height
                        , Att.fill "white"
                        , Att.stroke "black"
                        , Att.strokeWidth "4"
                        ]
                        []
                    ]
                |> Svg.g [ Att.transform "translate(50,50)" ]
    in
    [ grp ]
        |> Svg.svg
            [ Att.width "500"
            , Att.height "500"
            , Att.viewBox "0 0 500 500"
            ]


getRow : Int -> Board -> List Position
getRow r b =
    b.board |> Dict.filter (\k -> \_ -> (k |> Tuple.first) == r) |> Dict.keys


getCol : Int -> Board -> List Position
getCol c b =
    b.board |> Dict.filter (\k -> \_ -> (k |> Tuple.second) == c) |> Dict.keys


getSq : Board -> Position -> List Position
getSq b ( r, c ) =
    let
        dim =
            b.rank |> toFloat |> sqrt |> truncate

        base =
            \i -> ((i - 1) // dim) * dim

        rowNums =
            List.range 1 dim |> List.map ((+) (base r))

        colNums =
            List.range 1 dim |> List.map ((+) (base c))
    in
    colNums |> List.concatMap (\cc -> rowNums |> List.map (\rr -> ( rr, cc )))


getPeers : Position -> Board -> List Position
getPeers ( r, c ) b =
    getCol c b ++ getRow r b ++ getSq b ( r, c )
