module Main exposing (..)

import Array exposing (..)
import Browser
import Dict exposing (..)
import Element exposing (Element)
import Element.Input exposing (..)
import Html exposing (Html)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Position =
    ( Int, Int )


type alias Value =
    Int


dimToRC : Int -> Int -> ( Int, Int )
dimToRC dim index =
    ( ((index - 1) // (dim * dim)) + 1, modBy (dim * dim) (index - 1) + 1 )


allNums : Int -> List Int
allNums dim =
    List.range 1 (dim * dim)



-- List of missing values from a point


missing : Board -> Position -> List Int
missing b p =
    let
        row =
            p |> Tuple.first

        col =
            p |> Tuple.second

        rowVals =
            getRow b row |> Set.fromList

        colVals =
            getCol b col |> Set.fromList

        sqVals =
            getSq b (row, col) |> Set.fromList
    in
    rowVals |> Set.union colVals |> Set.union sqVals |> Set.diff (allNums b.dim |> Set.fromList) |> Set.toList


allPoints : Int -> List Position
allPoints dim =
    let
        nums =
            List.range 1 (dim * dim)
    in
    nums |> List.concatMap (\rr -> nums |> List.map (\cc -> ( rr, cc )))



-- initialModel.b |> unfilled |> List.map (\p -> (p, (missing initialModel.b p))) |> List.sortBy (\(x,y) -> y |> List.length)
-- TODO Error checking
-- Simple way to enter existing puzzles just enter row by row, zero for unknown


unfilled : Board -> List Position
unfilled b =
    b.contents |> Dict.keys |> Set.fromList |> Set.diff (allPoints b.dim |> Set.fromList) |> Set.toList


contentsFromOrderedList : Int -> List Int -> Dict Position Value
contentsFromOrderedList dim l =
    let
        --
        idx =
            List.length l |> List.range 1 |> List.map (dimToRC dim)

        pairs =
            List.map2 (\a -> \b -> ( a, b )) idx l
                |> List.filter (\p -> Tuple.second p > 0)
    in
    pairs
        |> List.map
            (\p ->
                case Tuple.first p of
                    ( x, y ) ->
                        ( ( x, y ), Tuple.second p )
            )
        |> Dict.fromList



-- Puzzle from Brain Training 1


puzzle1 =
    [ 1
    , 6
    , 0
    , 4
    , 0
    , 9
    , 8
    , 0
    , 0
    , 4
    , 2
    , 0
    , 0
    , 0
    , 1
    , 5
    , 0
    , 0
    , 0
    , 0
    , 5
    , 8
    , 0
    , 0
    , 0
    , 4
    , 3
    , 5
    , 0
    , 2
    , 7
    , 0
    , 8
    , 0
    , 9
    , 1
    , 0
    , 0
    , 0
    , 0
    , 9
    , 0
    , 0
    , 0
    , 0
    , 8
    , 7
    , 0
    , 1
    , 0
    , 3
    , 2
    , 0
    , 6
    , 9
    , 3
    , 0
    , 0
    , 0
    , 5
    , 6
    , 0
    , 0
    , 0
    , 0
    , 1
    , 9
    , 0
    , 0
    , 0
    , 3
    , 2
    , 0
    , 0
    , 4
    , 6
    , 0
    , 7
    , 0
    , 1
    , 5
    ]


type alias Board =
    { dim : Int
    , contents : Dict Position Value
    }


type alias Model =
    { b : Board
    }


initialModel : Model
initialModel =
    { b = Board 3 (puzzle1 |> contentsFromOrderedList 3) }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldB =
            model.b
    in
    case msg of
        Increment ->
            ( { model | b = solve oldB }, Cmd.none )

        _ ->
            ( model, Cmd.none )


renderNum : Position -> Value -> Svg Msg
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
    in
    text_
        [ x col
        , y row
        , fill "black"
        , Svg.Attributes.style "font-family: Arial; font-size: 34; stroke: #000000; fill: #000000;"
        ]
        [ Svg.text (String.fromInt v) ]


myLines : Int -> Int -> Int -> List (Svg Msg)
myLines size dim l =
    let
        sWidth =
            case modBy dim l of
                0 ->
                    "4"

                _ ->
                    "2"

        lSize =
            size * l |> String.fromInt

        bSize =
            dim * dim * size |> String.fromInt
    in
    [ line
        [ x1 lSize
        , y1 "0"
        , x2 lSize
        , y2 bSize
        , fill "white"
        , stroke "black"
        , strokeWidth sWidth
        ]
        []
    , line
        [ x1 "0"
        , y1 lSize
        , x2 bSize
        , y2 lSize
        , fill "white"
        , stroke "black"
        , strokeWidth sWidth
        ]
        []
    ]


dispBoard : Board -> Element Msg
dispBoard b =
    let
        -- edge of box in pixels
        boxSize =
            50

        -- edge of board in pixels
        boardSize =
            b.dim * b.dim * boxSize

        grp =
            List.range 1 (b.dim * b.dim)
                |> List.concatMap
                    (myLines boxSize b.dim)
                |> List.append ((b.contents |> Dict.map renderNum) |> Dict.values)
                |> List.append
                    [ rect
                        [ x "0"
                        , y "0"
                        , boardSize |> String.fromInt |> width
                        , boardSize |> String.fromInt |> height
                        , fill "white"
                        , stroke "black"
                        , strokeWidth "4"
                        ]
                        []
                    ]
                |> g [ transform "translate(50,50)" ]
    in
    [ grp ]
        |> svg
            [ width "800"
            , height "800"
            , viewBox "0 0 800 800"
            ]
        |> Element.html


view : Model -> Html Msg
view model =
    Element.column [ Element.explain Debug.todo, Element.width Element.fill, Element.height Element.fill, Element.spacing 5 ]
        [ Element.row [] [ model.b |> dispBoard ], Element.row [ Element.centerX ] [ Element.el [] myButton ] ]
        |> Element.layout []


myButton =
    Element.Input.button [] { onPress = Just Increment, label = Element.text "Click Me" }


getRow : Board -> Int -> List Int
getRow b r =
    b.contents |> Dict.filter (\k -> \v -> (k |> Tuple.first) == r) |> Dict.values


getCol : Board -> Int -> List Int
getCol b r =
    b.contents |> Dict.filter (\k -> \v -> (k |> Tuple.second) == r) |> Dict.values


getSq : Board -> Position -> List Int
getSq b ( r, c ) =
    let
        base =
            \i -> ((i - 1) // b.dim) * b.dim

        rowNums =
            List.range 1 b.dim |> List.map ((+) (base r))

        colNums =
            List.range 1 b.dim |> List.map ((+) (base c))

        points =
            colNums |> List.concatMap (\cc -> rowNums |> List.map (\rr -> ( rr, cc )))
    in
    points |> List.filterMap (\v -> Dict.get v b.contents)


remaining : Board -> Set Position
remaining b =
    let
        d =
            List.range 1 (b.dim * b.dim)

        allKeys : Set Position
        allKeys =
            d |> List.concatMap (\x -> d |> List.map (\y -> ( x, y ))) |> Set.fromList

        usedKeys : Set Position
        usedKeys =
            b.contents |> Dict.keys |> Set.fromList
    in
    usedKeys |> Set.diff allKeys


unSolved : Position -> Board -> Set Int
unSolved ( r, c ) b =
    let
        existingVals = getSq b ( r, c ) |> List.append (getRow b r) |> List.append (getCol b c) |> Set.fromList
        allVals = List.range 1 (b.dim * b.dim) |> Set.fromList
    in
    existingVals |> Set.diff allVals


updateBoard : Board -> Position -> Int -> Board
updateBoard b p i =
    let
        vals = b.contents |> Dict.insert p i
    in
    { b | contents = vals }

solve : Board -> Board
solve b =
    let
        r =
            b |> remaining
        lowPos =
            r |> Set.toList |> List.map (\p -> ( p, unSolved p b |> Set.toList )) |> List.sortBy (\(x,y) -> y |> List.length) |> List.head
    in
    case lowPos of
        Just (pnt, val) -> updateBoard b pnt (val |> List.head |> Maybe.withDefault 0)
        Nothing -> b
