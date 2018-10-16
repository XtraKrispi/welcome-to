module Main exposing (Board, Estate, House, HouseValue(..), Model, Msg(..), Pool(..), Street, init, main, update, view)

import Browser
import Deck exposing (Deck)
import Dict
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (src, value)
import Html.Events exposing (onClick, onInput)
import List
import Maybe
import Random exposing (Generator)
import Random.List exposing (shuffle)



---- MODEL ----


cardNumbers : List CardNumber
cardNumbers =
    let
        threeTimes =
            List.repeat 3 [ CardNumber 1, CardNumber 2, CardNumber 14, CardNumber 15 ]

        fourTimes =
            List.repeat 4 [ CardNumber 3, CardNumber 13 ]

        fiveTimes =
            List.repeat 5 [ CardNumber 4, CardNumber 12 ]

        sixTimes =
            List.repeat 6 [ CardNumber 5, CardNumber 11 ]

        sevenTimes =
            List.repeat 7 [ CardNumber 6, CardNumber 10 ]

        eightTimes =
            List.repeat 8 [ CardNumber 7, CardNumber 9 ]

        nineTimes =
            List.repeat 9 [ CardNumber 8 ]
    in
    List.concat threeTimes
        ++ List.concat fourTimes
        ++ List.concat fiveTimes
        ++ List.concat sixTimes
        ++ List.concat sevenTimes
        ++ List.concat eightTimes
        ++ List.concat nineTimes


cardEffects : List CardEffect
cardEffects =
    let
        pools =
            List.repeat 9 (CardEffect PoolManufacturer)

        tempAgencies =
            List.repeat 9 (CardEffect TempAgency)

        bis =
            List.repeat 9 (CardEffect Bis)

        landscapers =
            List.repeat 18 (CardEffect Landscaper)

        realEstates =
            List.repeat 18 (CardEffect RealEstateAgent)

        surveyors =
            List.repeat 18 (CardEffect Surveyor)
    in
    pools
        ++ tempAgencies
        ++ bis
        ++ landscapers
        ++ realEstates
        ++ surveyors


cards : List Card
cards =
    List.map2 (\n e -> { number = n, effect = e }) cardNumbers cardEffects


type HouseValue
    = NumberHouse Int
    | BisHouse Int


type Pool
    = NoPool
    | UnBuiltPool
    | BuiltPool


type Effect
    = Surveyor
    | RealEstateAgent
    | Landscaper
    | PoolManufacturer
    | TempAgency
    | Bis


type CardNumber
    = CardNumber Int


type CardEffect
    = CardEffect Effect


type alias Card =
    { number : CardNumber, effect : CardEffect }


type alias House =
    { currentValue : Maybe HouseValue
    , pool : Pool
    }


type alias Estate =
    { houses : List House
    }


type alias Street =
    { estates : List Estate
    , parks : List Int
    }


type alias Board =
    { streets : List Street
    }


type alias Player =
    { name : String, board : Board }


type alias Model =
    { players : List Player
    , cards : ( Deck Card, Deck Card, Deck Card )
    , gameState : GameState
    }


type alias Option =
    { effectCard : Card, numberCard : Card }


type GameState
    = NotStarted String
    | Playing Option Option Option


mkParks : Int -> List Int
mkParks =
    List.append [ 0 ] << List.map (\n -> 2 ^ (n - 1)) << List.range 2


mkHouses : Int -> List Int -> List House
mkHouses num pools =
    List.range 0 (num - 1)
        |> List.map
            (\n ->
                { currentValue = Nothing
                , pool =
                    if List.any (\p -> p == n) pools then
                        UnBuiltPool

                    else
                        NoPool
                }
            )


mkBoard : Board
mkBoard =
    let
        street1 =
            { estates =
                [ { houses = mkHouses 10 [ 2, 6, 7 ] }
                ]
            , parks = mkParks 3
            }

        street2 =
            { estates =
                [ { houses = mkHouses 11 [ 0, 3, 7 ] }
                ]
            , parks = mkParks 4
            }

        street3 =
            { estates =
                [ { houses = mkHouses 12 [ 1, 6, 10 ] }
                ]
            , parks = mkParks 5
            }
    in
    { streets = [ street1, street2, street3 ] }


init : ( Model, Cmd Msg )
init =
    ( { players = []
      , cards = ( Deck.empty, Deck.empty, Deck.empty )
      , gameState = NotStarted ""
      }
    , Random.generate DeckShuffled (Deck.shuffledDeck cards)
    )



---- UPDATE ----


type Msg
    = DeckShuffled (Deck Card)
    | StartGame
    | NewPlayerChanged String
    | AddNewPlayer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeckShuffled deck ->
            case Deck.divide 3 deck of
                [ deck1, deck2, deck3 ] ->
                    ( { model
                        | cards = ( deck1, deck2, deck3 )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StartGame ->
            let
                getCards deck =
                    let
                        ( mCard, newDeck ) =
                            Deck.deal deck

                        ( mPeeked, newDeck1 ) =
                            Deck.peek newDeck
                    in
                    case ( mCard, mPeeked ) of
                        ( Just card, Just peeked ) ->
                            Just
                                ( { effectCard = card, numberCard = peeked }
                                , newDeck1
                                )

                        _ ->
                            Nothing

                ( deck1, deck2, deck3 ) =
                    model.cards
            in
            case
                ( getCards deck1
                , getCards deck2
                , getCards deck3
                )
            of
                ( Just ( r1, d1 ), Just ( r2, d2 ), Just ( r3, d3 ) ) ->
                    ( { model
                        | cards = ( d1, d2, d3 )
                        , gameState = Playing r1 r2 r3
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NewPlayerChanged np ->
            ( { model | gameState = NotStarted np }, Cmd.none )

        AddNewPlayer ->
            case model.gameState of
                NotStarted newPlayer ->
                    ( { model | players = model.players ++ [ { name = newPlayer, board = mkBoard } ], gameState = NotStarted "" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        mainView =
            case model.gameState of
                NotStarted np ->
                    let
                        playersView =
                            div []
                                ([ p [] [ text "Players" ]
                                 ]
                                    ++ List.map (\p -> div [] [ text p.name ]) model.players
                                )
                    in
                    div []
                        [ playersView
                        , div [] [ input [ value np, onInput NewPlayerChanged ] [], button [ onClick AddNewPlayer ] [ text "Add Player" ] ]
                        , button [ onClick StartGame ] [ text "Start the game!" ]
                        ]

                Playing option1 option2 option3 ->
                    div []
                        [ div []
                            [ div []
                                [ text ("Effect Card: " ++ Debug.toString option1.effectCard) ]
                            , div
                                []
                                [ text ("Number Card: " ++ Debug.toString option1.numberCard) ]
                            ]
                        , div []
                            [ div []
                                [ text ("Effect Card: " ++ Debug.toString option2.effectCard) ]
                            , div
                                []
                                [ text ("Number Card: " ++ Debug.toString option2.numberCard) ]
                            ]
                        , div []
                            [ div []
                                [ text ("Effect Card: " ++ Debug.toString option3.effectCard) ]
                            , div
                                []
                                [ text ("Number Card: " ++ Debug.toString option3.numberCard) ]
                            ]
                        ]
    in
    mainView



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
