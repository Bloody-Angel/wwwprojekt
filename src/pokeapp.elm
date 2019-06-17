module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, section, a)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Svg exposing (svg, image)
import Svg.Attributes exposing (viewBox, version)
import List
import Json.Decode exposing (Decoder)
import File
import Http
import Set
import Random
import Random.List exposing (choose)

main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        ,subscriptions = subscriptions
        }

type alias Model =
    {shapes : List(Polygon)
    ,zustand : Zustand
    ,searchedPokeId : Maybe String
    }

initialModel : () ->(Model ,Cmd Msg)
initialModel _ =
    (
        {shapes = []
        ,zustand = Success
        ,searchedPokeId = Nothing}
        ,Http.get
            {url = "/src/Maps/Aquarium.json"
            ,expect = Http.expectString GotText
            }
    )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

type Msg
    = PolyClicked String String --erster String: Pokedex ID, zweiter String: Zusatzinfo
    | PokeGenerateClicked
    | PokeGenerated (Maybe String, List String)
    | GotText (Result Http.Error String) 

type alias Polygon = 
    {dexId : String 
    ,shiny : String 
    ,polypoints : String
    }
    
type Zustand 
    = Success
    | Failure

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PolyClicked id info->
            --TODO
            (model, Cmd.none)
        PokeGenerateClicked ->
            (model, Random.generate PokeGenerated (zufallsID model))
        PokeGenerated (id,liste)->
            case id of 
                Just value -> 
                     ({model|searchedPokeId = id}, Cmd.none)
                Nothing -> 
                    ({model|zustand = Failure}, Cmd.none)
        GotText res ->
            case res of
                Ok jsondatei -> 
                    ({model|shapes = readPolys jsondatei, zustand = Success}, Cmd.none)
                Err _ ->
                    ({model|shapes = [], zustand = Failure}, Cmd.none)

view : Model -> Html Msg
view model =
    div [class "inhalt"]
        [buttons model
        ,clickableImage model
        ]

buttons : Model -> Html Msg
buttons model =
    div [class "nes-container is-rounded"
        ]
        [a  [class "nes-btn", onClick PokeGenerateClicked] 
            [text "generate question"
            ]
        ]

-- Decoder fuer json-Polygone zu Polygonen
readSinglePoly : Decoder Polygon
readSinglePoly =
    Json.Decode.map3 Polygon
        (Json.Decode.field "Pokedex No" Json.Decode.string)
        (Json.Decode.field "Zusatzinfo" Json.Decode.string)
        (Json.Decode.field "Polygon points" Json.Decode.string)


-- bekommt String (json Liste von  Polygonen) uebergeben, erstellt Liste von Polygonen
readPolys : String -> List(Polygon)
readPolys str = 
    let result = Json.Decode.decodeString (Json.Decode.list readSinglePoly) str
    in 
        case result of
            Ok wert ->
                wert
            Err error ->
                []


--erstellt aus einem Polygon eine Html Msg mit einem svg polygon
svgPoly : Polygon ->Svg.Svg Msg
svgPoly poly= 
    Svg.polygon[Svg.Attributes.points poly.polypoints, Svg.Attributes.class "polygon" , onClick (PolyClicked poly.dexId poly.shiny) ][]



--erstellt das Bild und die Polygone   
clickableImage : Model -> Html Msg
clickableImage model = 
    section[ class "section" ]
        [div [ class "container"]
            [Html.figure [ class "image" ]
                [svg [Svg.Attributes.class "viewBoxCenter", Svg.Attributes.width "100%", viewBox "0 0 1920 1080", version "1.1"]
                    ([Svg.defs[]
                        [Svg.style[]
                            --css für mittige viewBox
                            [text """   .viewBoxCenter {
                                                    display: block;
                                                    margin-left: auto;
                                                    margin-right: auto;
                                                    }
                                                    
                                        .polygon { 
                                                    fill: black;   
                                                    fill-opacity:0;
                                                }
                                         .polygon:hover {
                                                    fill: white;
                                                    fill-opacity:0.03;
                                        }"""
                            ]        
                        ]
                    ,image [ Html.Attributes.width 1920, Html.Attributes.height 1080, Html.Attributes.title "Aquarium", Svg.Attributes.xlinkHref "/src/Bilder/Aquarium.png" ] []
                    ]
                    ++(List.map svgPoly model.shapes)
                    )
                ]
            ]
        ]

getID : Polygon -> String
getID poly =
    poly.dexId

zufallsID : Model -> Random.Generator (Maybe String, List String)
zufallsID model =
    (choose (Set.toList (Set.fromList (List.map getID model.shapes))))

ashsText : Model ->  String
ashsText model =
    case model.searchedPokeId of
        Nothing -> 
            "Hello, I'm Ash. If you want, I'll quiz you on you pokemon knowledge. Just klick the Button above me."
        Just id ->
            "Search for "++id++" in this picture."   