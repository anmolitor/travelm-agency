module Generators.DynamicTest exposing (..)

import Dynamic.DateFormatServer
import Dynamic.DateFormatTranslations
import Dynamic.HashServer
import Dynamic.HashTranslations
import Dynamic.HtmlInterpolationServer
import Dynamic.HtmlInterpolationTranslations
import Dynamic.InterpolationMatchServer
import Dynamic.InterpolationMatchTranslations
import Dynamic.MultiInterpolationServer
import Dynamic.MultiInterpolationTranslations
import Dynamic.MultiLanguageTextServer
import Dynamic.MultiLanguageTextTranslations
import Dynamic.NestedHtmlServer
import Dynamic.NestedHtmlTranslations
import Dynamic.NestedInterpolationServer
import Dynamic.NestedInterpolationTranslations
import Dynamic.NumberFormatServer
import Dynamic.NumberFormatTranslations
import Dynamic.PluralServer
import Dynamic.PluralTranslations
import Dynamic.SimpleHtmlServer
import Dynamic.SimpleHtmlTranslations
import Dynamic.SimpleI18nLastServer
import Dynamic.SimpleI18nLastTranslations
import Dynamic.SingleInterpolationServer
import Dynamic.SingleInterpolationTranslations
import Dynamic.SingleTextServer
import Dynamic.SingleTextTranslations
import Expect
import Html
import Html.Attributes
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Util
import Util.Shared exposing (sendRequest)


singleText : Test
singleText =
    describe "single text"
        [ test "returns the expected translated text" <|
            \_ ->
                sendRequest Dynamic.SingleTextServer.server "messages.en.json" Dynamic.SingleTextTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SingleTextTranslations.init)
                    |> Result.map Dynamic.SingleTextTranslations.singleText
                    |> Expect.equal (Ok "the text")
        ]


multiLanguageText : Test
multiLanguageText =
    describe "multi language text"
        [ test "returns the expected translated text in english" <|
            \_ ->
                sendRequest Dynamic.MultiLanguageTextServer.server "messages.en.json" Dynamic.MultiLanguageTextTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.MultiLanguageTextTranslations.init)
                    |> Result.map Dynamic.MultiLanguageTextTranslations.text
                    |> Expect.equal (Ok "english text")
        , test "returns the expected translated text in german" <|
            \_ ->
                sendRequest Dynamic.MultiLanguageTextServer.server "messages.de.json" Dynamic.MultiLanguageTextTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.MultiLanguageTextTranslations.init)
                    |> Result.map Dynamic.MultiLanguageTextTranslations.text
                    |> Expect.equal (Ok "german text")
        ]


singleInterpolation : Test
singleInterpolation =
    describe "single interpolation"
        [ test "interpolates the given value at the correct position" <|
            \_ ->
                sendRequest Dynamic.SingleInterpolationServer.server "messages.en.json" Dynamic.SingleInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SingleInterpolationTranslations.init)
                    |> Result.map (\i18n -> Dynamic.SingleInterpolationTranslations.text i18n "world")
                    |> Expect.equal (Ok "hello world!")
        ]


multiInterpolation : Test
multiInterpolation =
    describe "multi interpolation"
        [ test "interpolates the given values at the correct positions" <|
            \_ ->
                sendRequest Dynamic.MultiInterpolationServer.server "messages.en.json" Dynamic.MultiInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.MultiInterpolationTranslations.init)
                    |> Result.map (\i18n -> Dynamic.MultiInterpolationTranslations.greeting i18n { timeOfDay = "morning", name = "my dear" })
                    |> Expect.equal (Ok "Good morning, my dear")
        , test "works for languages that do not use all interpolated values" <|
            \_ ->
                sendRequest Dynamic.MultiInterpolationServer.server "messages.de.json" Dynamic.MultiInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.MultiInterpolationTranslations.init)
                    |> Result.map (\i18n -> Dynamic.MultiInterpolationTranslations.greeting i18n { timeOfDay = "Morgen", name = "Doesn't matter" })
                    |> Expect.equal (Ok "Guten Morgen")
        , test "works if languages interpolate values in different orders" <|
            \_ ->
                sendRequest Dynamic.MultiInterpolationServer.server "messages.yoda.json" Dynamic.MultiInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.MultiInterpolationTranslations.init)
                    |> Result.map (\i18n -> Dynamic.MultiInterpolationTranslations.greeting i18n { timeOfDay = "morning", name = "Luke" })
                    |> Expect.equal (Ok "Luke, good morning")
        ]


i18nLastSimple : Test
i18nLastSimple =
    describe "generated code with i18nArgLast flag"
        [ test "single text" <|
            \_ ->
                sendRequest Dynamic.SimpleI18nLastServer.server "messages.en.json" Dynamic.SimpleI18nLastTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SimpleI18nLastTranslations.init)
                    |> Result.map Dynamic.SimpleI18nLastTranslations.singleText
                    |> Expect.equal (Ok "the text")
        , test "single interpolation" <|
            \_ ->
                sendRequest Dynamic.SimpleI18nLastServer.server "messages.en.json" Dynamic.SimpleI18nLastTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SimpleI18nLastTranslations.init)
                    |> Result.map (Dynamic.SimpleI18nLastTranslations.interpolation "world")
                    |> Expect.equal (Ok "Hello world!")
        , test "multi interpolation" <|
            \_ ->
                sendRequest Dynamic.SimpleI18nLastServer.server "messages.en.json" Dynamic.SimpleI18nLastTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SimpleI18nLastTranslations.init)
                    |> Result.map (Dynamic.SimpleI18nLastTranslations.greeting { timeOfDay = "evening", name = "Sir" })
                    |> Expect.equal (Ok "Good evening, Sir")
        ]


interpolationMatchCase : Test
interpolationMatchCase =
    describe "interpolation match case"
        [ test "interpolates the correct value for female gender" <|
            \_ ->
                sendRequest Dynamic.InterpolationMatchServer.server "messages.en.json" Dynamic.InterpolationMatchTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.InterpolationMatchTranslations.init)
                    |> Result.map (\i18n -> Dynamic.InterpolationMatchTranslations.text i18n "female")
                    |> Expect.equal (Ok "She bought a cat.")
        , test "interpolates the correct value for male gender" <|
            \_ ->
                sendRequest Dynamic.InterpolationMatchServer.server "messages.en.json" Dynamic.InterpolationMatchTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.InterpolationMatchTranslations.init)
                    |> Result.map (\i18n -> Dynamic.InterpolationMatchTranslations.text i18n "male")
                    |> Expect.equal (Ok "He bought a cat.")
        , test "interpolates the default value for other values" <|
            \_ ->
                sendRequest Dynamic.InterpolationMatchServer.server "messages.en.json" Dynamic.InterpolationMatchTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.InterpolationMatchTranslations.init)
                    |> Result.map (\i18n -> Dynamic.InterpolationMatchTranslations.text i18n "anything else")
                    |> Expect.equal (Ok "It bought a cat.")
        ]


nestedInterpolation : Test
nestedInterpolation =
    describe "nested interpolation" <|
        let
            i18n =
                sendRequest Dynamic.NestedInterpolationServer.server
                    "messages.de.json"
                    Dynamic.NestedInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.NestedInterpolationTranslations.init)
        in
        [ test "interpolates the correct values for 'Ich'" <|
            \_ ->
                i18n
                    |> Result.map (\i -> Dynamic.NestedInterpolationTranslations.text i { pronoun = "Ich", objectsToBuy = "Gemüse" })
                    |> Expect.equal (Ok "Ich kaufe Gemüse.")
        , test "interpolates the correct values for 'Du'" <|
            \_ ->
                i18n
                    |> Result.map (\i -> Dynamic.NestedInterpolationTranslations.text i { pronoun = "Du", objectsToBuy = "Obst" })
                    |> Expect.equal (Ok "Du kaufst Obst.")
        , test "interpolates the default value for other values" <|
            \_ ->
                i18n
                    |> Result.map (\i -> Dynamic.NestedInterpolationTranslations.text i { pronoun = "Er", objectsToBuy = "Fleisch" })
                    |> Expect.equal (Ok "Er kauft Fleisch.")
        ]


numberFormatCase : Test
numberFormatCase =
    describe "number format"
        [ test "type checks" <|
            \_ ->
                sendRequest Dynamic.NumberFormatServer.server
                    "messages.en.json"
                    Dynamic.NumberFormatTranslations.decodeMessages
                    |> Result.map ((|>) (Dynamic.NumberFormatTranslations.init Util.emptyIntl Dynamic.NumberFormatTranslations.En))
                    |> Result.map (\i18n -> Dynamic.NumberFormatTranslations.text i18n 12.34)
                    -- This is expected since we cannot get the actual browser intl API in the test
                    -- We do not want to test the intl-proxy package here, so the fact that the generated
                    -- code typechecks is enough here.
                    |> Expect.equal (Ok "Price: ")
        ]


dateFormatCase : Test
dateFormatCase =
    describe "date format"
        [ test "type checks" <|
            \_ ->
                sendRequest Dynamic.DateFormatServer.server
                    "messages.en.json"
                    Dynamic.DateFormatTranslations.decodeMessages
                    |> Result.map ((|>) (Dynamic.DateFormatTranslations.init Util.emptyIntl Dynamic.DateFormatTranslations.En))
                    |> Result.map (\i18n -> Dynamic.DateFormatTranslations.text i18n <| Time.millisToPosix 9000)
                    -- This is expected since we cannot get the actual browser intl API in the test
                    -- We do not want to test the intl-proxy package here, so the fact that the generated
                    -- code typechecks is enough here.
                    |> Expect.equal (Ok "Today: ")
        ]


pluralCase : Test
pluralCase =
    describe "plural"
        [ test "type checks" <|
            \_ ->
                sendRequest Dynamic.PluralServer.server "messages.en.json" Dynamic.PluralTranslations.decodeMessages
                    |> Result.map ((|>) (Dynamic.PluralTranslations.init Util.emptyIntl Dynamic.PluralTranslations.En))
                    |> Result.map (\i18n -> Dynamic.PluralTranslations.text i18n 4)
                    -- Due to the absent intl api, we can only test the default case here
                    |> Expect.equal (Ok "I met many people.")
        ]


hashRegressionTest : Test
hashRegressionTest =
    describe "content hashing of json files"
        [ test "produces a consistent hash for english content" <|
            \_ ->
                sendRequest Dynamic.HashServer.server "messages.en.2061212766.json" Dynamic.HashTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.HashTranslations.init)
                    |> Result.map Dynamic.HashTranslations.text
                    |> Expect.equal (Ok "english text")
        , test "produces a consistent hash for german content" <|
            \_ ->
                sendRequest Dynamic.HashServer.server "messages.de.1722545000.json" Dynamic.HashTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.HashTranslations.init)
                    |> Result.map Dynamic.HashTranslations.text
                    |> Expect.equal (Ok "german text")
        ]


simpleHtml : Test
simpleHtml =
    describe "simple html"
        [ test "produces the correct html element and text content" <|
            \_ ->
                sendRequest Dynamic.SimpleHtmlServer.server "messages.en.json" Dynamic.SimpleHtmlTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SimpleHtmlTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.SimpleHtmlTranslations.html i18n []
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "a" ]
                                |> Query.has [ Selector.text "Click me" ]
                        )
        , test "generates html attribute from translation file" <|
            \_ ->
                sendRequest Dynamic.SimpleHtmlServer.server "messages.en.json" Dynamic.SimpleHtmlTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SimpleHtmlTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.SimpleHtmlTranslations.html i18n []
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "a" ]
                                |> Query.has [ Selector.attribute <| Html.Attributes.href "/" ]
                        )
        , test "passes extra attributes given at runtime" <|
            \_ ->
                sendRequest Dynamic.SimpleHtmlServer.server "messages.en.json" Dynamic.SimpleHtmlTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.SimpleHtmlTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.SimpleHtmlTranslations.html i18n [ Html.Attributes.class "link" ]
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "a" ]
                                |> Query.has [ Selector.class "link" ]
                        )
        ]


nestedHtml : Test
nestedHtml =
    describe "nested html"
        [ test "produces the correct outer html element" <|
            \_ ->
                sendRequest Dynamic.NestedHtmlServer.server "messages.en.json" Dynamic.NestedHtmlTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.NestedHtmlTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.NestedHtmlTranslations.html i18n { image = [], link = [ Html.Attributes.class "nestedLink" ], text = [] }
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "a" ]
                                |> Query.has
                                    [ Selector.attribute <| Html.Attributes.href "/"
                                    , Selector.text "!"
                                    , Selector.class "nestedLink"
                                    ]
                        )
        , test "produces the correct inner span element" <|
            \_ ->
                sendRequest Dynamic.NestedHtmlServer.server "messages.en.json" Dynamic.NestedHtmlTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.NestedHtmlTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.NestedHtmlTranslations.html i18n { image = [], link = [], text = [ Html.Attributes.class "theText" ] }
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "a" ]
                                |> Query.find [ Selector.tag "span" ]
                                |> Query.has
                                    [ Selector.attribute <| Html.Attributes.width 100
                                    , Selector.attribute <| Html.Attributes.height 50
                                    , Selector.text "Click me"
                                    , Selector.class "theText"
                                    ]
                        )
        , test "produces the correct inner img element" <|
            \_ ->
                sendRequest Dynamic.NestedHtmlServer.server "messages.en.json" Dynamic.NestedHtmlTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.NestedHtmlTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.NestedHtmlTranslations.html i18n { image = [ Html.Attributes.class "nestedImage" ], link = [], text = [] }
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "a" ]
                                |> Query.find [ Selector.tag "img" ]
                                |> Query.has
                                    [ Selector.attribute <| Html.Attributes.src "/imgUrl.png"
                                    , Selector.class "nestedImage"
                                    ]
                        )
        ]


mixedHtmlAndInterpolation : Test
mixedHtmlAndInterpolation =
    describe "mixed html and interpolation"
        [ test "shows expected content for admin role" <|
            \_ ->
                sendRequest Dynamic.HtmlInterpolationServer.server "messages.en.json" Dynamic.HtmlInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.HtmlInterpolationTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.HtmlInterpolationTranslations.text i18n { adminLink = "/admin", role = "admin", username = "A. Dmin" } []
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.has
                                    [ Selector.text "Thanks for logging in. "
                                    , Selector.containing
                                        [ Selector.tag "a"
                                        , Selector.attribute <| Html.Attributes.href "/admin"
                                        , Selector.text "A. Dmin"
                                        ]
                                    , Selector.containing
                                        [ Selector.tag "a"
                                        , Selector.attribute <| Html.Attributes.href "/admin"
                                        , Selector.text "may click on this link"
                                        ]
                                    ]
                        )
        , test "shows expected content for normal role" <|
            \_ ->
                sendRequest Dynamic.HtmlInterpolationServer.server "messages.en.json" Dynamic.HtmlInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.HtmlInterpolationTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.HtmlInterpolationTranslations.text i18n { adminLink = "/admin", role = "normal", username = "Justin Normal" } []
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.has
                                    [ Selector.text "You ("
                                    , Selector.text "Justin Normal"
                                    , Selector.text ") are not an admin."
                                    ]
                        )
        , test "shows expected content for default role" <|
            \_ ->
                sendRequest Dynamic.HtmlInterpolationServer.server "messages.en.json" Dynamic.HtmlInterpolationTranslations.decodeMessages
                    |> Result.map ((|>) Dynamic.HtmlInterpolationTranslations.init)
                    |> expectOkWith
                        (\i18n ->
                            Dynamic.HtmlInterpolationTranslations.text i18n { adminLink = "/admin", role = "undefined", username = "Does not matter" } []
                                |> Html.div []
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "You are not logged in." ]
                        )
        ]


expectOkWith : (a -> Expect.Expectation) -> Result x a -> Expect.Expectation
expectOkWith expect result =
    case result of
        Err _ ->
            Expect.fail "Failed to load translation file"

        Ok ok ->
            expect ok
