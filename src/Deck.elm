module Deck exposing (Deck, deal, divide, empty, peek, shuffledDeck)

import Random
import Random.List as Random


type Deck a
    = Deck (List a)


shuffledDeck : List a -> Random.Generator (Deck a)
shuffledDeck =
    Random.map Deck << Random.shuffle


empty : Deck a
empty =
    Deck []


peek : Deck a -> ( Maybe a, Deck a )
peek ((Deck xs) as deck) =
    case xs of
        [] ->
            ( Nothing, deck )

        x :: _ ->
            ( Just x, deck )


deal : Deck a -> ( Maybe a, Deck a )
deal ((Deck xs) as deck) =
    case xs of
        [] ->
            ( Nothing, deck )

        x :: xs_ ->
            ( Just x, Deck xs_ )


divide : Int -> Deck a -> List (Deck a)
divide num (Deck cards) =
    let
        subdivide n cs =
            case cs of
                [] ->
                    []

                _ ->
                    List.take n cs :: subdivide n (List.drop n cs)
    in
    List.map Deck (subdivide (List.length cards // num) cards)
