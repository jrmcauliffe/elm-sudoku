module Main exposing (..)

import Array exposing (..)
import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { dim : Int
    , contents : Array Int
    }


initialModel : Model
initialModel =
    { dim = 2
    , contents = Array.fromList [ 1, 2, 3, 4, 3, 2, 1, 4, 1, 2, 3, 4, 3, 2, 1, 4 ]
    }



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
    case msg of
        Increment ->
            ( { model | dim = model.dim + 1 }, Cmd.none )

        Decrement ->
            ( { model | dim = model.dim - 1 }, Cmd.none )


myNum : Int -> Int -> Int -> Svg Msg
myNum dim index value =
    let
        xOffset =
            15

        yOffset =
            38

        (xx,yy) = dimToXY dim index

        row =
           xx * 50 + xOffset |> String.fromInt

        col =
           yy * 50 + yOffset |> String.fromInt
    in
    text_
        [ x row
        , y col
        , fill "black"
        , Svg.Attributes.style "font-family: Arial; font-size: 34; stroke: #000000; fill: #000000;"
        ]
        [ text (String.fromInt value) ]



dimToXY : Int -> Int -> (Int, Int)
dimToXY dim index = (modBy (dim * dim) index, (index // (dim * dim)))


myNums : Array Int -> List (Svg Msg)
myNums nums =
    let
        allNums =
            List.range 0 (Array.length nums - 1)
                |> List.map (\x -> (nums |> Array.get x) |> Maybe.map (\y -> ( x, y )))
                |> List.filterMap identity
    in
    allNums
        |> List.map
            (\( ind, val ) ->
                myNum 2 ind val
             --       , Svg.Attributes.style "font-family: Arial; font-size: 34; stroke: #000000; fill: #000000;"
            )


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


board : Model -> Svg Msg
board m =
    let
        -- edge of box in pixels
        boxSize =
            50

        -- edge of board in pixels
        boardSize =
            m.dim * m.dim * boxSize
    in
    g [ transform "translate(100,100)" ]
        (List.range 1 (m.dim * m.dim)
            |> List.concatMap
                (myLines boxSize m.dim)
            |> List.append (myNums m.contents)
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
        )


view : Model -> Html Msg
view model =
    svg
        [ width "1500"
        , height "1500"
        , viewBox "0 0 1500 1500"
        ]
        [ model |> board ]
