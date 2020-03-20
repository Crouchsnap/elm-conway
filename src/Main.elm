module Main exposing (..)

import Array exposing (Array, get)
import Browser
import Html exposing (button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


init =
    Array.fromList
        [ Array.fromList [ False, False, False, False ]
        , Array.fromList [ False, True, False, False ]
        , Array.fromList [ False, True, False, False ]
        , Array.fromList [ False, True, False, False ]
        ]


type Msg
    = Advance
    | InvertCell Bool Int Int


update msg model =
    case msg of
        Advance ->
            mutateState model

        InvertCell cell x y ->
            model


view model =
    let
        lists =
            model |> Array.toList |> List.map (\column -> Array.toList column)
    in
    div []
        (List.map
            (\column ->
                div []
                    (List.map
                        (\cell ->
                            if cell then
                                span [] [ text "alive" ]

                            else
                                span [ style "color" "red" ] [ text "dead" ]
                        )
                        column
                    )
            )
            lists
            ++ [ button [ onClick Advance ] [ text "advance model" ] ]
        )


initialize : Int -> Int -> Array (Array Bool)
initialize width height =
    Array.repeat width (Array.repeat height False)


type alias Board =
    Array (Array Bool)


mutateState : Board -> Board
mutateState input =
    Array.indexedMap (\y column -> Array.indexedMap (\x cell -> mutateCell input x y) column) input


numberOfLivingNeighbors input x y =
    getNeighbors input x y |> List.filter (\cell -> cell == True) |> List.length


mutateCell input x y =
    let
        livingNeighbors =
            numberOfLivingNeighbors input x y

        inputCell =
            getCell input x y

        log =
            Debug.log (livingNeighbors |> String.fromInt)
    in
    if livingNeighbors < 2 then
        False

    else if (livingNeighbors == 2 || livingNeighbors == 3) && inputCell then
        True

    else if livingNeighbors > 3 then
        False

    else if livingNeighbors == 3 && inputCell == False then
        True

    else
        inputCell


neighbors =
    [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( -1, 0 ), ( 1, 0 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]


getNeighbors : Board -> Int -> Int -> List Bool
getNeighbors input x y =
    List.map (\( xOffset, yOffset ) -> getCell input (x + xOffset) (y + yOffset)) neighbors


getCell input x y =
    case get y input of
        Just row ->
            case get x row of
                Just cell ->
                    cell

                Nothing ->
                    False

        Nothing ->
            False
