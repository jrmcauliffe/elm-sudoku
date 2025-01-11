module Main exposing (..)

import Board as B
import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)



--import Solver


puzzle1 =
    "160409800420001500005800043502708091000090000870103206930005600001900032004607015"


type alias Model =
    { board : B.Board
    , inputString : String
    , inputStatus : String
    }


initialModel : Model
initialModel =
    case puzzle1 |> B.importBoard of
        Ok b ->
            { board = b
            , inputString = ""
            , inputStatus = ""
            }

        Err e ->
            { board = B.newBoard 9
            , inputString = ""
            , inputStatus = e
            }


main : Program () Model B.Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : B.Msg -> Model -> ( Model, Cmd B.Msg )
update msg model =
    case msg of
        B.SolveMsg ->
            case model.inputString |> B.importBoard of
                Ok b ->
                    ( { model | board = b, inputStatus = "Success!" }, Cmd.none )

                Err e ->
                    ( { model | inputStatus = e }, Cmd.none )

        B.TextMsg s ->
            ( { model | inputString = s }, Cmd.none )


view : Model -> Html B.Msg
view model =
    Element.column [ Element.width Element.fill, Element.spacing 5 ]
        [ Element.row [] [ model.board |> B.renderBoard |> Element.html ]
        , Element.row [ padding 20 ] [ Element.el [] gameInput, model.inputStatus |> Element.text ]
        , Element.row [] [ Element.el [] myButton ]
        ]
        |> Element.layout []


gameInput =
    Input.multiline
        [ width <| px 450
        , height <| px 200
        , Border.rounded 6
        , Border.width 2
        , Border.color <| rgb255 0x72 0x9F 0xCF
        ]
        { onChange = B.TextMsg
        , text = ""
        , placeholder = Just <| Input.placeholder [] <| text "Enter you game here"
        , label = Input.labelAbove [] (Element.text "Game Input")
        , spellcheck = False
        }


myButton =
    Input.button [] { onPress = Just B.SolveMsg, label = Element.text "Load" }
