module Main exposing (..)

import List exposing (map, repeat)
import Http
import Html exposing (Html, text, img, div, h1)
import Html.Attributes exposing (src, href, width, height)
import Html.Events exposing (onClick)
import Json.Decode as D
import Browser
import Json.Decode.Pipeline exposing (required, hardcoded)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Debug exposing (toString)

---- API -----
api : String
api = "http://localhost:3001/"

hotelListingUrl : String
hotelListingUrl = "listings/"

---- MODEL ----

type ListingsState 
    = Failed
    | Loading 
    | Success (List Listing) 

type alias Model =
    { listings : ListingsState }

type Msg
  = GotListings (Result Http.Error (List Listing))

type alias Listing  = 
    { image : String
    , title : String 
    , stars : Int
    , location : String 
    , rating : Float 
    , smiley : String 
    , numReviews : Int 
    , includesRoom : Bool
    , includesLocalCharge : Bool 
    , totalPrice : Float 
    , perPerson : Float
    }

----- DECODER ------

listingDecoder : D.Decoder Listing 
listingDecoder = D.succeed Listing 
    |> hardcoded "hotel1.jpg"
    |> required "title" D.string
    |> required "stars" D.int
    |> required "location" D.string
    |> required "rating" D.float
    |> required "smiley" D.string
    |> required "numReviews" D.int
    |> required "includesRoom" D.bool
    |> required "includesLocalCharge" D.bool
    |> required "totalPrice" D.float
    |> required "perPerson" D.float

---- UPDATE ----
getArticle : Cmd Msg
getArticle = 
    Http.get 
        { url = api ++ hotelListingUrl
        , expect = Http.expectJson GotListings (D.list listingDecoder)
        }

init : ( Model, Cmd Msg )
init =
    ( Model Loading
    , getArticle
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotListings result ->
      case result of
        Ok fetchedListings ->
            ({ model | listings = Success fetchedListings }, Cmd.none)
        Err _ ->
            ({ model | listings = Failed } , Cmd.none)

---- VIEW ----

viewStar : Html Msg 
viewStar = img [src "star-fill.svg", width 30, height 30] [] 

viewExtras : Bool -> String -> String -> Html Msg 
viewExtras extra msg icon = if extra then 
    div [] 
    [ img [src icon, width 45, height 45] []
    , text msg ] else 
    text ""

viewArticle : Listing -> Html Msg
viewArticle a = 
    div [] [
        Grid.row [] 
            [ Grid.col []
                [ Card.config [ Card.outlinePrimary ]
                    |> Card.headerH4 [] [ text a.title ]
                    |> Card.block []
                        [ Block.custom <| img [src a.image] [] ]
                    |> Card.block []    
                        (repeat a.stars (Block.custom viewStar) )
                    |> Card.block []
                        [ Block.text [] [ text "Price includes:" ]
                        , Block.custom (viewExtras a.includesRoom "Room Only" "gift.svg") 
                        , Block.custom (viewExtras a.includesLocalCharge "Local charges Payable at hotel" "percent.svg")
                        ]
                    |> Card.block []
                        [ Block.text [] [ text "Total price from" ]
                        , Block.text [] [ text ("£" ++ toString a.totalPrice) ]
                        , Block.text [] [ text ("(Per person £" ++ toString a.perPerson ++ ")")]
                        , Block.custom <|
                            Button.linkButton
                                [ Button.primary, Button.attrs [ href "#view-more" ] ]
                                [ text "View More" ]
                        ]
                    |> Card.view
                ]            
            ]
    ]

listingsNotFound : List (Html Msg)
listingsNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Listings could not be loaded"
    ]

loadingListings : List (Html Msg)
loadingListings =
    [ h1 [] [ text "Loading" ]
    , text "Loading the listings"
    ]

mainContent : Model -> Html Msg 
mainContent model = 
    Grid.container [] <|
        case model.listings of 
            Failed -> 
                listingsNotFound
            Loading ->
                loadingListings
            Success articles -> 
                [ div [] (map (\a -> viewArticle a) articles) ]

view : Model -> Html Msg 
view model = Grid.container []
        [ CDN.stylesheet      
        , mainContent model
        ]

---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
