port module PhotoGroove exposing (main)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, name, src, title, type_, checked)
import Html.Events exposing (on, onClick)
import Browser
import Random
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed, at)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode

type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }

type alias Photo = 
    { url : String
    , size : Int
    , title : String 
    }

type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }

type ThumbnailSize 
    = Small
    | Medium
    | Large
type Msg 
    = ClickedPhoto Photo
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | ClickedReload
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int

type Status
    = Loading
    | Loaded (List Photo) Photo
    | Errored String

port setFilters : FilterOptions -> Cmd msg

view : Model -> Html Msg
view model =
    div 
        [ class "content" ] <|
        case model.status of
            Loaded photos photo ->
                viewLoaded photos photo model.chosenSize model
            Loading ->
                []
            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]
 
viewLoaded : List Photo -> Photo -> ThumbnailSize -> Model -> List (Html Msg)
viewLoaded photos photo chosenSize model =
        [ h1 
            [] 
            [ text "Photo Groove" ]
        , h3 
            []
            [ text "Thumbnail Size:" ]
        , div
            [ id "choose-size" ]
            (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , div 
            [ class "filters" ]
            [ viewFilter SlidHue "Hue" model.hue
            , viewFilter SlidRipple "Ripple" model.ripple
            , viewFilter SlidNoise "Noise" model.noise
            ]
        , div 
            [ id "thumbnails", class (sizeToClass chosenSize)] 
            (List.map (viewThumbnail photo) photos)
        , canvas 
            [ id "main-canvas", class "large" ]
            []
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        ClickedReload ->
            ( { model | status = Loading, chosenSize = Medium }, initialCmd)
        ClickedPhoto photo -> 
            applyFilters { model | status = selectedPhoto photo model.status }
        ClickedSize size ->
            ({ model | chosenSize = size }, Cmd.none)
        ClickedSurpriseMe ->
            case model.status of
                Loaded (head :: tail) _ ->
                    Random.uniform head tail
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model
                Loaded [] _ ->
                    (model, Cmd.none)
                Loading ->
                    (model, Cmd.none)
                Errored _ ->
                    (model, Cmd.none)
        GotRandomPhoto photo ->
            applyFilters { model | status = selectedPhoto photo model.status }
        GotPhotos (Ok photos) ->
            case photos of 
                photo :: _ ->
                    applyFilters
                        { model | status = Loaded photos photo }
                [] ->
                    ({ model | status = Errored "0 photos found" }, Cmd.none)
        GotPhotos (Err _) ->
            ({ model | status = Errored "Server error!" }, Cmd.none)
        SlidHue hue ->
            applyFilters { model | hue = hue }
        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }
        SlidNoise noise ->
            applyFilters { model | noise = noise }
                
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

selectedPhoto : Photo -> Status -> Status
selectedPhoto photo status =
    case status of 
        Loaded photos _ ->
            Loaded photos photo
        Loading ->
            status
        Errored errorMessage ->
            status

viewThumbnail : Photo -> Photo -> Html Msg
viewThumbnail photo thumbnail =
    img 
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ String.fromInt thumbnail.size ++ " KB]")
        , classList [ ("selected", photo.url == thumbnail.url) ] 
        , onClick (ClickedPhoto thumbnail)
        ]
        []

viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosensize size = 
    label 
        []
        [ input 
            [ type_ "radio"
            , name "size"
            , checked (chosensize == size)
            , onClick (ClickedSize size) 
            ]
            []
        , text (sizeToString size)
        ]

applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos photo ->
            let
                filters = 
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url = urlPrefix ++ "large/" ++ photo.url
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none ) 

onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg = 
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"

viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude = 
    div [ class "filter-slider" ]
        [
          label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Json.Encode.int magnitude) 
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]

sizeToString : ThumbnailSize -> String
sizeToString size = 
    case size of 
        Small ->
            "small"
        Medium ->
            "medium"
        Large ->
            "large"

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children

photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"

sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of 
        Small ->
            "small"
        Medium ->
            "med"
        Large ->
            "large"

initialModel : Model
initialModel = 
    { status = Loading 
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }

initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, initialCmd)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
