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

type alias Model 
    =
    {shapes : List(Polygon)
    ,zustand : Zustand
    ,aktiveSuche : Maybe Suche
    }

type alias Polygon
     =
    {dexId : String 
    ,shiny : String 
    ,polypoints : String
    }

type Zustand 
    = Success
    | Failure

type alias Suche 
    =
    {searchedPokeId : String 
    ,foundIt : Maybe Bool
    ,pokeInfo : Maybe String
    }
    
type Msg 
    = PolyClicked String String --erster String: Pokedex ID, zweiter String: Zusatzinfo
    | PokeGenerateClicked
    | PokeGenerated (Maybe String, List String)
    | GotPokemonInfo (Result Http.Error String)
    | GotImageMap (Result Http.Error String) 

main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

initialModel : () ->(Model ,Cmd Msg)
initialModel _ =
    (
        {shapes = []
        ,zustand = Success
        ,aktiveSuche = Nothing}
        ,Http.get
            {url = "/src/Maps/Aquarium.json"
            ,expect = Http.expectString GotImageMap
            }
    )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PolyClicked id info->
            case model.aktiveSuche of
                Nothing ->
                    (model, Cmd.none)
                Just aktiveSuche ->
                    updatePolyClicked model id aktiveSuche

        PokeGenerateClicked ->
            (model, Random.generate PokeGenerated (zufallsID model))
        PokeGenerated (id,liste)->
            updatePokeGenerated model id
        GotPokemonInfo infojson ->
            updateGotPokeInfo model infojson
        GotImageMap res ->
            case res of
                Ok jsondatei -> 
                    ({model|shapes = readPolys jsondatei, zustand = Success}, Cmd.none)
                Err _ ->
                    ({model|shapes = [], zustand = Failure}, Cmd.none)

updatePokeGenerated: Model -> Maybe String-> (Model,Cmd Msg)
updatePokeGenerated model id=
    case id of 
        Just value -> 
            ({model|aktiveSuche = Just {searchedPokeId=value, foundIt=Nothing, pokeInfo=Nothing}}
            , Http.get
                {url = "https://pokeapi.co/api/v2/pokemon/"++value
                ,expect = Http.expectString GotPokemonInfo
                }
            )
        Nothing -> 
            ({model|zustand = Failure}, Cmd.none)

updateGotPokeInfo: Model -> Result Http.Error String ->(Model, Cmd Msg)
updateGotPokeInfo model infojson=
    case infojson of
        Ok jsondatei -> 
            case model.aktiveSuche of
                    Nothing ->
                        ({model|zustand = Failure}, Cmd.none)
                    Just value -> 
                        let 
                            oldaktiveSuche = value
                            newaktiveSuche = {oldaktiveSuche | pokeInfo=Just jsondatei}
                        in
                            ({model|aktiveSuche = Just newaktiveSuche} ,Cmd.none)
        Err _ ->
            ({model|shapes = [], zustand = Failure}, Cmd.none)

updatePolyClicked: Model -> String -> Suche -> (Model, Cmd Msg)
updatePolyClicked model id suche =
    if id ==suche.searchedPokeId then
        let 
            newaktiveSuche = {suche | foundIt=Just True}
        in           
            ({model|aktiveSuche = Just newaktiveSuche} ,Cmd.none)
    else
        let 
            newaktiveSuche = {suche | foundIt=Just False}
        in           
            ({model|aktiveSuche = Just newaktiveSuche} ,Cmd.none)


view : Model -> Html Msg
view model =
    div [class "inhalt"]
        [buttons model
        ,ashCatchThem (ashsText model)
        ,clickableImage model
        ]

buttons : Model -> Html Msg
buttons model =
    div [class "nes-container is-rounded inhaltsElemente"
        ]
        [a  [class "nes-btn", onClick PokeGenerateClicked] 
            [text "Ask me a question!"
            ]
        ]

ashCatchThem : String -> Html Msg
ashCatchThem str =
    section [class "message-list inhaltsElemente"]  
        [section [class "message -left"]
            [Html.i [class "nes-ash ash"] []
            ,div    [class "nes-balloon from-left balloon"] 
                    [Html.p [] [text str]
                    ]
            ]
        ]

ashsText : Model ->  String
ashsText model =
    case model.zustand of
        Failure ->
            "Oh, seems like an error happened somewhere."
        Success -> 
            case model.aktiveSuche of
                Nothing -> 
                    "Hello, I'm Ash. If you want, I'll quiz you on your pokemon knowledge. Just klick the Button above me."
                Just suche ->
                    case suche.foundIt of
                        Nothing -> 
                            case suche.pokeInfo of
                                Just str ->
                                    "Search for "++(getPokeName (str))++" in this picture."  
                                Nothing ->
                                    "Hmmm..."
                        Just True ->
                            "Great, that's right!"
                        Just False ->
                            case suche.pokeInfo of
                                Just str ->
                                    "Oh, that's not "++(getPokeName (str))++ ", but you can try again."  
                                Nothing ->
                                    "Hmmm..."
                            

--erstellt das Bild und die Polygone   
clickableImage : Model -> Html Msg
clickableImage model = 
    section [class "section inhaltsElemente"]
        [div [class "container"]
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


getID : Polygon -> String
getID poly =
    poly.dexId

zufallsID : Model -> Random.Generator (Maybe String, List String)
zufallsID model =
    (choose (Set.toList (Set.fromList (List.map getID model.shapes))))
 

getPokeName : String -> String
getPokeName str = 
    let result = Json.Decode.decodeString (Json.Decode.field "name" Json.Decode.string) str
    in 
        case result of
            Ok wert ->
                wert
            Err error ->
                ""
    
