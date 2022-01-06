module Tests exposing (..)

import Test exposing (..)
import Expect
import Json.Decode exposing (decodeString)
import Main exposing (listingDecoder, Listing)

-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "json gets parsed correctly" <|
            (\_ ->
                "{ \"title\": \"Grand Hyatt Dubai\", \"stars\": 5, \"location\": \"Bur Dubai Area, Dubai\", \"rating\": 9.2, \"smiley\": \"happy\", \"numReviews\": 92, \"includesRoom\": true, \"includesLocalCharge\": true, \"totalPrice\": 526, \"perPerson\": 108 }"
                    |> decodeString listingDecoder
                    |> Expect.equal 
                        (Ok (Listing "hotel1.jpg" "Grand Hyatt Dubai" 5 "Bur Dubai Area, Dubai" 9.2 "happy" 92 True True 526 108)))
        ]
