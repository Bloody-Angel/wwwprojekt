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

jsondatei : String
jsondatei =
    """
    [
	{
		"Pokedex No":"258",
		"Zusatzinfo":"",
		"Polygon points":"368 335,375 342,380 349,384 344,377 335,389 331,386 314,395 311,406 315,403 330,410 335,412 348,405 352,406 362,378 364,370 362,361 352,364 339"
	}
	,{
		"Pokedex No":"728",
		"Zusatzinfo":"",
		"Polygon points":"507 351,514 351,523 349,538 350,540 336,530 336,528 332,542 324,540 316,533 319,525 316,525 309,532 304,534 296,543 288,554 286,568 289,577 296,585 293,592 300,591 306,586 308,589 316,590 322,585 324,581 323,578 331,587 341,580 339,591 349,610 353,609 360,588 358,576 351,572 351,570 357,561 358,557 363,539 361,510 358"
	}
	,{
		"Pokedex No":"489",
		"Zusatzinfo":"",
		"Polygon points":"414 507,427 501,451 491,476 485,489 482,505 481,511 486,515 489,508 492,501 500,495 507,494 513,494 525,503 532,513 536,507 540,499 541,493 541,490 542,490 549,494 554,506 556,513 551,518 558,527 563,539 552,546 554,557 549,562 545,563 539,558 536,553 537,541 540,538 538,546 536,556 527,556 517,551 505,544 494,532 489,536 489,543 485,546 479,539 472,531 470,512 469,507 476,495 475,451 485,429 493,413 504,376 532"
	}
	,{
		"Pokedex No":"393",
		"Zusatzinfo":"",
		"Polygon points":"670 275,671 281,683 283,687 280,695 278,692 273,697 263,693 251,700 253,703 244,702 231,694 221,685 216,673 216,664 224,660 236,662 240,664 252,660 256,657 265,660 269,668 270"
	}
	,{
		"Pokedex No":"349",
		"Zusatzinfo":"",
		"Polygon points":"1252 943,1293 938,1330 963,1328 978,1338 979,1344 994,1378 994,1369 971,1412 967,1426 962,1425 950,1421 931,1408 887,1365 858,1373 827,1312 821,1325 870,1297 883,1282 855,1249 867,1241 911"
	}
	,{
		"Pokedex No":"129",
		"Zusatzinfo":"",
		"Polygon points":"734 413,746 427,761 411,766 413,765 427,800 439,808 417,821 417,829 437,842 433,846 421,837 418,833 402,833 383,821 365,798 355,798 335,786 338,769 327,773 349,760 349,764 369,754 373,749 357,737 354,738 389"
	}
	,{
		"Pokedex No":"129",
		"Zusatzinfo":"s",
		"Polygon points":"1480 514,1489 501,1522 513,1525 502,1541 501,1546 516,1554 516,1553 488,1558 481,1545 460,1529 456,1513 437,1494 468,1470 454,1473 481,1470 509"
	}
	,{
		"Pokedex No":"79",
		"Zusatzinfo":"",
		"Polygon points":"930 393,934 401,940 397,953 400,962 401,969 397,974 401,985 401,989 393,1002 389,1002 373,994 361,978 357,966 360,958 356,945 360,946 349,960 341,958 336,936 333,925 364,928 377"
	}
]

    """

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