module Main exposing (..)

import Board as B
import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import InputPad as I


puzzle1 =
    "160409800420001500005800043502708091000090000870103206930005600001900032004607015"


type alias Model =
    { board : B.Board
    , input : I.Input
    , inputString : String
    , status : String
    }


initialModel : Model
initialModel =
    case puzzle1 |> B.importBoard of
        Ok b ->
            { board = b
            , input = I.newInput
            , inputString = ""
            , status = ""
            }

        Err e ->
            { board = B.newBoard 9
            , input = I.newInput
            , inputString = ""
            , status = e
            }


type Msg
    = SolveMsg
    | TextMsg String
    | ButtonMsg I.Value


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SolveMsg ->
            case model.inputString |> B.importBoard of
                Ok b ->
                    ( { model | board = b, status = "Success!" }, Cmd.none )

                Err e ->
                    ( { model | status = e }, Cmd.none )

        TextMsg s ->
            ( { model | inputString = s }, Cmd.none )

        ButtonMsg val ->
            ( { model | status = "Button Pressed -> " ++ Debug.toString val }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.column [ Element.width Element.fill, Element.spacing 20, Element.padding 25 ]
        [ Element.row [] [ model.board |> B.renderBoard |> Element.html, I.renderInput ButtonMsg model.input ]
        , Element.row [] [ Element.el [] gameInput, model.status |> Element.text ]
        , Element.row [] [ Element.el [] loadButton ]
        ]
        |> Element.layout []


gameInput =
    Input.multiline
        [ width <| px 450
        , height <| px 200
        , Border.rounded 6
        , Border.width 2
        , Border.color <| rgb255 0x00 0x00 0x00
        ]
        { onChange = TextMsg
        , text = ""
        , placeholder = Just <| Input.placeholder [] <| text "Enter you game here"
        , label = Input.labelAbove [] (Element.text "Game InputPad")
        , spellcheck = False
        }


loadButton =
    Input.button
        []
        { onPress = Just SolveMsg, label = Element.text "Load" }
