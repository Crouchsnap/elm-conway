module Example exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
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
        , test "find cell works" <|
            \_ ->
                getCell
                    (Array.fromList
                        [ Array.fromList [ True, False, False ]
                        , Array.fromList [ False, False, False ]
                        , Array.fromList [ False, False, False ]
                        ]
                    )
                    0
                    0
                    |> Expect.equal True
        , test "count neighbors works" <|
            \_ ->
                numberOfLivingNeighbors
                    (Array.fromList
                        [ Array.fromList [ False, False, False ]
                        , Array.fromList [ True, True, False ]
                        , Array.fromList [ False, False, False ]
                        ]
                    )
                    1
                    1
                    |> Expect.equal 1
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
                Array.fromList
                    [ Array.fromList [ False, False, False ]
                    , Array.fromList [ True, True, False ]
                    , Array.fromList [ False, False, False ]
                    ]
                    |> mutateState
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
