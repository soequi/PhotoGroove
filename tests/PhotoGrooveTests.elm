module PhotoGrooveTests exposing
    (clickThumbnail, decoderTest, photoFromUrl, sliders, testSlider,
    thumbnailRendered, thumbnailsWork, urlFuzzer, urlsFromCount)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Model, Msg(..), Photo, Status(..), initialModel, update, urlPrefix, view)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


suite : Test
suite =
    test "one plus one equals two" (\_ -> Expect.equal 2 (1 + 1))



-- Encode.int and Encode.string translate ELm values into their JSON equivalent.
-- Encode.object takes a list of key-value pairs, each key must be a String, and
-- each value must be a Value, we use this function to create a Value representing
-- JSON structure.
-- decodeValue, decodes a Value directly, without having to convert to and from
-- an intermediate string representation.
-- The call to fuzz2 says that we want a fuzz test that randomly generates two values.
-- string and int are fuzzer specifying that we want to first generated value to be
-- a string, and the second to be an integer.
-- elm-test will run this function 100 times, each time randomly generating a fresh
-- String value and passing it in as url, and a fresh Int value and passing it in
-- as size.


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (Untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(Untitled)")


slidHueSetTest : Test
slidHueSetTest =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount


sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount



-- Initially, we don't render any thumbnails test.


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)



-- Query functions make use of two types as they descend into Html:
-- 1. Query.Single, which represents a single DOM node.
-- 2. Query.Multiple, which represents multiple DOM nodes.
-- Query.count function takes Multiple, Query.children takes single.
-- The Query.fromHtml function begins the process of descending into an Html
-- value, by returning a Single representing the Html's root node.
-- What is the rote node? we can tell by looking at the view function that returned
-- that Html, ie: PhotoGroove.view
--   view : Model -> Html Msg
--   view model =
--      div [ class "content" ]
-- because view returns a div with a class of "content", when our Query.fromHtml
-- function returns a Query.Single value, that Single value will refer
-- to this div.
--
-- Query.fromHtml : Html msg -> Query.Single msg
-- Query.findAll : List Selector -> Query.Single msg -> Query.Multiple msg
-- Query.count : (Int -> Expectation) -> Multiple msg -> Expectation
--
-- These two expressions are equivalent:
-- Query.count (Expect.equal 0)
-- Query.count (\count -> Expect.equal 0 count)
-- Test: Once the photos load, we render a thumbnail for each of them.


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)



-- This function starts with a Query.Single -- which will represent the root
-- of our page's DOM, and finds all the img elements within it that have the
-- expected src attribute. Then it runs a Query.count, which expects that our
-- query found at least one img element like that.


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsWork : Test
thumbnailsWork =
    fuzz (Fuzz.intRange 1 5) "Urls render as thumbnails" <|
        \urlCount ->
            let
                urls : List String
                urls =
                    List.range 1 urlCount
                        |> List.map (\num -> String.fromInt num ++ ".png")

                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")


clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)
