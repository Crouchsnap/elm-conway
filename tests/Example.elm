module Example exposing (..)

import Array exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "conway"
        [ test "initialize returns empty board" <|
            \_ ->
                initialize 4 4
                    |> Expect.equal
                        (Array.fromList
                            [ Array.fromList [ False, False, False, False ]
                            , Array.fromList [ False, False, False, False ]
                            , Array.fromList [ False, False, False, False ]
                            , Array.fromList [ False, False, False, False ]
                            ]
                        )
        , test "mutate board with cell with no neighbors dies" <|
            \_ ->
                mutateState
                    (Array.fromList
                        [ Array.fromList [ False, False, False ]
                        , Array.fromList [ False, True, False ]
                        , Array.fromList [ False, False, False ]
                        ]
                    )
                    |> Expect.equal
                        (Array.fromList
                            [ Array.fromList [ False, False, False ]
                            , Array.fromList [ False, False, False ]
                            , Array.fromList [ False, False, False ]
                            ]
                        )
        , test "mutate board with cell with one neighbor dies" <|
            \_ ->
                mutateState
                    (Array.fromList
                        [ Array.fromList [ False, False, False ]
                        , Array.fromList [ True, True, False ]
                        , Array.fromList [ False, False, False ]
                        ]
                    )
                    |> Expect.equal
                        (Array.fromList
                            [ Array.fromList [ False, False, False ]
                            , Array.fromList [ False, False, False ]
                            , Array.fromList [ False, False, False ]
                            ]
                        )
        , test "mutate board with cell with two neighbors lives" <|
            \_ ->
                mutateState
                    (Array.fromList
                        [ Array.fromList [ True, False, False ]
                        , Array.fromList [ False, True, False ]
                        , Array.fromList [ False, False, True ]
                        ]
                    )
                    |> Expect.equal
                        (Array.fromList
                            [ Array.fromList [ False, False, False ]
                            , Array.fromList [ False, True, False ]
                            , Array.fromList [ False, False, False ]
                            ]
                        )
        ]


initialize : Int -> Int -> Array (Array Bool)
initialize width height =
    Array.repeat width (Array.repeat height False)


type alias Board =
    Array (Array Bool)


mutateState : Board -> Board
mutateState input =
    Array.indexedMap (\y column -> Array.indexedMap (\x cell -> mutateCell input x y) column) input


mutateCell input x y =
    if (getNeighbors input x y |> List.filter (\cell -> cell == True) |> List.length) < 2 then
        False

    else
        True


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
