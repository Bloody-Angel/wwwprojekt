module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, section)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Svg exposing (svg, image)
import Svg.Attributes exposing (viewBox, version)
import Json.Decode

type alias Model =
    { count : Int }



initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement

type Shape  
    = Polygon String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ clickableImage
        ]


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





main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
