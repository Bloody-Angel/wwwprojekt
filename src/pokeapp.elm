module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, section)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Svg exposing (svg, image)
import Svg.Attributes exposing (viewBox, version)

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
        div [ class "container" ]
            [ Html.figure [ class "image" ]
                [ svg [ Svg.Attributes.width "100%", viewBox "0 0 1920 1800", version "1.1" ]
                    ([ image [ Html.Attributes.width 1920, Html.Attributes.height 1800, Html.Attributes.title "Aquarium", Svg.Attributes.xlinkHref "/src/Bilder/Aquarium.png" ] []
                     ]

                    )
                ]
            ]
        


     {-    <section class="section">
                <div class="container">
                    <figure class="image">
                        <svg width="100%" viewBox="0 0 800 600" version="1.1">
                        <image width="800" height="600" title="Informatikgebäude" xlink:href="http://www.informatik.uni-halle.de/im/1285058520_1381_00_800.jpg"></image>
                        </svg>
                    </figure>
                </div>
            </section>
 -}

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
