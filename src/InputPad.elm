module InputPad exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input


type Value
    = Num Int
    | Undo
    | Nothing


type alias Input =
    { value : Value
    }


newInput : Input
newInput =
    { value = Nothing }


renderInput : (Value -> msg) -> Input -> Element msg
renderInput msgOnClick input =
    let
        -- Buttons in a phone keypad layout ordering
        vals : List Value
        vals =
            Undo :: ([ 3, 2, 1, 6, 5, 4, 9, 8, 7 ] |> List.map Num) |> List.reverse

        buttonStyle =
            [ Element.padding 10
            , Element.width (px 60)
            , Element.height (px 60)
            , Border.rounded 6
            , Border.width 2
            , Border.color <| rgb255 0x72 0x9F 0xCF
            ]

        button v =
            case v of
                Num n ->
                    Input.button buttonStyle { onPress = msgOnClick v |> Just, label = Element.text (String.fromInt n) }

                Undo ->
                    Input.button buttonStyle { onPress = msgOnClick v |> Just, label = Element.text "Undo" }

                Nothing ->
                    Input.button buttonStyle { onPress = msgOnClick v |> Just, label = Element.text "" }

        renderRows : List Value -> List (Element msg)
        renderRows vs =
            let
                renderRow : List Value -> Element msg
                renderRow rowVals =
                    Element.row [] (rowVals |> List.map button)
            in
            case vs of
                a :: b :: c :: rest ->
                    renderRow [ a, b, c ] :: renderRows rest

                x ->
                    [ renderRow x ]
    in
    Element.column [] (vals |> renderRows)
