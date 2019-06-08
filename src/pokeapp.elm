module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, section)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Svg exposing (svg, image)
import Svg.Attributes exposing (viewBox, version)
import Json.Decode exposing (Decoder)
import File

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

type alias Model =
    { shapes : List(Polygon) }

initialModel : Model
initialModel =
    { shapes = []}

type Msg
    = PolyClicked String
    | ButtonClicked

type alias Polygon = 
    { dexId : String 
    ,shiny : String 
    ,polypoints : String
    }  

update : Msg -> Model -> Model
update msg model =
    case msg of
        PolyClicked id->
            --TODO
            model
        ButtonClicked ->
            --TODO
            model

view : Model -> Html Msg
view model =
    div []
        [ clickableImage
        ]

readSinglePoly : Decoder Polygon
readSinglePoly =
    Json.Decode.map3 Polygon
        (Json.Decode.field "Pokedex No" Json.Decode.string)
        (Json.Decode.field "Zusatzinfo" Json.Decode.string)
        (Json.Decode.field "Polygon points" Json.Decode.string)

readPolys : String -> List(Polygon)
readPolys name = 
    Json.Decode.list readSinglePoly

clickableImage :  Html Msg
clickableImage =
    section[ class "section" ]
        [ div [ class "container" ]
            [ Html.figure [ class "image" ]
                [ svg [Svg.Attributes.class "viewBoxCenter", Svg.Attributes.width "80%", viewBox "0 0 1920 1800", version "1.1"]
                    [Svg.defs[]
                        [Svg.style[]
                            --css für mittige viewBox
                            [ text """.viewBoxCenter  {
                                                    display: block;
                                                    margin-left: auto;
                                                    margin-right: auto;
                                                    }"""
                            ]        
                        ]
                    ,image [ Html.Attributes.width 1920, Html.Attributes.height 1800, Html.Attributes.title "Aquarium", Svg.Attributes.xlinkHref "/src/Bilder/Aquarium.png" ] []
                    ]
                ]
            ]
        ]