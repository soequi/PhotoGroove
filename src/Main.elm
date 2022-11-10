module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import PhotoFolders as Folders
import PhotoGallery as Gallery
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)



-- elm/url package gives us three modules for working with the various
-- pieces of URL:
-- 1. Url.Builder helps us assemble URLs from its parts.
-- 2. Url.Parser and Url.Parser.Query help us translte the path and query
--    portions of a URL into more helpful values.


type Page
    = GalleryPage Gallery.Model
    | FoldersPage Folders.Model
    | NotFound


type Route
    = Gallery
    | Folders
    | SelectedPhoto String


type alias Model =
    { page : Page
    , key : Nav.Key
    , version : Float
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotFoldersMsg Folders.Msg
    | GotGalleryMsg Gallery.Msg


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, GalleryPage _ ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, FoldersPage _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            li [ classList [ ( "active", isActive { link = route, page = page } ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]



-- TODO: Debug.log


viewFooter : Html msg
viewFooter =
    footer []
        [ text "One is never alone with a rubber duck. -Douglas Adams" ]


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                FoldersPage folders ->
                    Folders.view folders
                        |> Html.map GotFoldersMsg

                GalleryPage gallery ->
                    Gallery.view gallery
                        |> Html.map GotGalleryMsg

                NotFound ->
                    text "Not Found"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }



-- viewHeader use lazy because it's rarely change. lazy tell Elm runtime
-- to cache the previous return value of viewHeader, so viewHeader will get
-- rerun only when model.page changes.
-- If you want to use lazy with a function that takes multiple arguments,
-- check out lazy2 and lazy3.
-- This Document Msg value we're returning is a record with two fields:
-- "title" is a string that sets the page's title in the browser. Because
-- we control the whole page now, we can do that.
-- "body" is a List (Html Msg) that specifies the children of the page's
-- <body> element. It's a List rather than a single Html Msg node because
-- we're controlling <body>'s entire lst of children -- whereas with
-- Browser.element, we controlled a single element on the page.


toFolders : Model -> ( Folders.Model, Cmd Folders.Msg ) -> ( Model, Cmd Msg )
toFolders model ( folders, cmd ) =
    ( { model | page = FoldersPage folders }
    , Cmd.map GotFoldersMsg cmd
    )


toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( gallery, cmd ) =
    ( { model | page = GalleryPage gallery }
    , Cmd.map GotGalleryMsg cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GotFoldersMsg foldersMsg ->
            case model.page of
                FoldersPage folders ->
                    toFolders model (Folders.update foldersMsg folders)

                _ ->
                    ( model, Cmd.none )

        GotGalleryMsg galleryMsg ->
            case model.page of
                GalleryPage gallery ->
                    toGallery model (Gallery.update galleryMsg gallery)

                _ ->
                    ( model, Cmd.none )



-- ClickedLink message will contain an External request if the user clicked
-- a link to a different domain. ClickedLink will contain an Internal request
-- if the user clicked a link to the same domain
-- type UrlRequest
--   = External String
--   | Internal Url
--
-- Whereas Nav.load does a full page load of an entirely new page all pushUrl
-- does is to push the given URL onto the browser's history stack. This has
-- a few implications:
-- * The URL shown in the browser's address bar will become this one.
-- * Browser.application will send a ChagedUrl event to update, with this
--   URL stored inside it. That's because we specified ChagedUrl for our
--   onUrlChaged handler when we set up our Browser.application.
-- * When the user clicks the Back button in the browser, this URL will now
--   be one of the ones it goes back to. Also, when the user does that,
--   Browser.application will send a ChagedUrl event to udpate.


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        GalleryPage gallery ->
            Gallery.subscriptions gallery
                |> Sub.map GotGalleryMsg

        _ ->
            Sub.none


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top -- match "/"
        , Parser.map Gallery (s "gallery") -- match "/gallery"
        , Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]



-- urlToPage : Float -> Url -> Page
-- urlToPage version url =
--     case Parser.parse parser url of
--         Just Gallery ->
--             GalleryPage (Tuple.first (Gallery.init version))
--         Just Folders ->
--             FoldersPage (Tuple.first (Folders.init Nothing))
--         Just (SelectedPhoto filename) ->
--             FoldersPage (Tuple.first (Folders.init (Just filename)))
--         Nothing ->
--             NotFound


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Gallery ->
            Gallery.init model.version
                |> toGallery model

        Just Folders ->
            Folders.init Nothing
                |> toFolders model

        Just (SelectedPhoto filename) ->
            Folders.init (Just filename)
                |> toFolders model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


init : Float -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    updateUrl url
        { page = NotFound
        , key = key
        , version = version
        }



-- The (s "photos" </> Parser.string):
-- 1. This parser will succeed only if it is run on URL whose path begins
--    with the string "/photos" followed by a slash and then another string
--    with a length of at least 1. ("/photos/" -> not match, but "/photos/a" match)
-- 2. If it succeeds, the Parser's final output will be the STring following
--    the "photos/" part of the URL path. (So it would succeed with "pop"
--    when parsing a path of "/photos/pop")
-- Parser.oneOf takes a List of parsers and tries them one at a time until
-- it either finds a match or runs out.


main : Program Float Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Browser.element view's function must return Html Msg.
-- Browser.document view's function returns Document Msg, this gives Elm
-- application control over the entire page, whereas with Browser.element
-- we were confined to a single DOM element on the page.
