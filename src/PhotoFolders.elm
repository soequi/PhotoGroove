module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import PhotoGroove exposing (urlPrefix)


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root =
        Folder
            { name = "Loading..."
            , photoUrls = []
            , subfolders = []
            , expanded = True
            }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "https://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
            { photos = photos
            , root = root
            , selectedPhotoUrl = Nothing
            }
        )
        modelPhotosDecoder
        folderDecoder



{-


   > Decode.map2
   <function>
       : (a -> b -> value)
         -> Decode.Decoder a
         -> Decode.Decoder b
         -> Decode.Decoder value
   > Decode.map
   <function> : (a -> value) -> Decode.Decoder a -> Decode.Decoder value

-}


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)
    | ClickedFolder FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )

        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


viewPhoto : String -> Html Msg
viewPhoto url =
    div [ class "photo", onClick (ClickedPhoto url) ]
        [ text url ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []



-- viewFolder : Folder -> Html Msg
-- viewFolder (Folder folder) =
--     let
--         subfolders =
--             List.map viewFolder folder.subfolders
--     in
--     div [ class "folder" ]
--         [ label [] [ text folder.name ]
--         , div [ class "subfolders" ] subfolders
--         ]
-- Because the Folder custom type has exactly one variant, Elm lets us avoid
-- writing a full case-expression by using the shorthand
-- viewFoder (Folder folder) = ...
-- to destructure it inline instead.That shorter version is equivalent to:
-- viewFolder wrappedFolder =
--      case wrappedFolder of
--          Folder folder ->
--              ...


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                    (List.indexedMap viewSubfolder folder.subfolders)
                    (List.map viewPhoto folder.photoUrls)
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"



-- selectedPhoto : Html Msg
-- selectedPhoto =
--     case model.selectedPhotoUrl of
--         Just url ->
--             case Dict.get url model.photos of
--                 Just photo ->
--                     viewSelectedPhoto photo
--                 Nothing ->
--                     text ""
--         Nothing ->
--             text ""
--
-- Whenever we have two nested case-expressions, with both of them handling
-- handling Nothing the same way, Maybe.andThen does exactly the same thing
-- as the code we had before.


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


type FolderPath
    = End
    | Subfolder Int FolderPath


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }



-- toggleExpanded takes a FolderPath and a Folder, and does one of the
-- following:
-- * If FolderPath is End, thre are no subfolders to traverse into, so toggle
--   the expanded value on the given folder.
-- * If FolderPath is Subfulder targetIndex, look through the given root's
--   subfolders until we find the one at position targetIndex. Then call
--   toggleExpanded again, this time passing that subfolder as the new root
--   folder, and passing the remaining FolderPath after discarding the
--   Subfolder value we just handled.
-- TODO:
-- Maybe.map, Maybe.andThen, Result.map, Result.andThen
-- A type alias to represent the photo info we get from JSON
-- this is an intermediate representation -- a value we'll use only to help
-- us translate fro one value to another.


type alias JsonPhoto =
    { title : String
    , size : Int
    , relatedUrls : List String
    }


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        -- Decodes these fields from JSON object into a JsonPhoto record
        |> required "title" string
        |> required "size" int
        |> required "related_photos" (list string)


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url = url
      , size = json.size
      , title = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList



-- Decode.map
-- <function> : (a -> value) -> Decode.Decoder a -> Decode.Decoder value


photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.keyValuePairs jsonPhotoDecoder
        |> Decode.map fromPairs


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , expanded = True
        , subfolders = subfolders
        , photoUrls = Dict.keys photos
        }


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" string
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list folderDecoder))



-- We call Decode.succeed folderFromJson, meaning this folderFromJson
-- function will be called if all the decoding steps succeed.
-- If you see cyclic-definition error on a decoder, it's likely that
-- Decode.lazy can resolve it.


modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list modelPhotosDecoder))


modelPhotosFromJson :
    Dict String Photo
    -> List (Dict String Photo)
    -> Dict String Photo
modelPhotosFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos



{- List.foldl vs List.foldr:


   > List.foldl (\letter str -> str ++ "-" ++ String.fromChar letter) "start" ['a', 'b', 'c', 'd']
   "start-a-b-c-d" : String


   > List.foldr (\letter str -> str ++ "-" ++ String.fromChar letter) "start" ['a', 'b', 'c', 'd']
   "start-d-c-b-a" : String

   List.fold{l,r} returns type will be the same as its initial type/state ("start")

   foldl is faster than foldr.. you know why? List

-}
