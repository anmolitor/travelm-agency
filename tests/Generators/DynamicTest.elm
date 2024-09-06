module Generators.DynamicTest exposing (..)

import Dynamic.DateFormatServer as DateFormatServer
import Dynamic.DateFormatTranslations as DateFormatTranslations
import Dynamic.EscapeServer as EscapeServer
import Dynamic.EscapeTranslations as EscapeTranslations
import Dynamic.FallbackServer as FallbackServer
import Dynamic.FallbackTranslations as FallbackTranslations
import Dynamic.HashServer as HashServer
import Dynamic.HashTranslations as HashTranslations
import Dynamic.HtmlInterpolationServer as HtmlInterpolationServer
import Dynamic.HtmlInterpolationTranslations as HtmlInterpolationTranslations
import Dynamic.HtmlIntlServer as HtmlIntlServer
import Dynamic.HtmlIntlTranslations as HtmlIntlTranslations
import Dynamic.InterpolationMatchServer as InterpolationMatchServer
import Dynamic.InterpolationMatchTranslations as InterpolationMatchTranslations
import Dynamic.MultiBundleLanguageServer as MultiBundleLanguageServer
import Dynamic.MultiBundleLanguageTranslations as MultiBundleLanguageTranslations
import Dynamic.MultiBundleServer as MultiBundleServer
import Dynamic.MultiBundleTranslations as MultiBundleTranslations
import Dynamic.MultiInterpolationServer as MultiInterpolationServer
import Dynamic.MultiInterpolationTranslations as MultiInterpolationTranslations
import Dynamic.MultiLanguageTextServer as MultiLanguageTextServer
import Dynamic.MultiLanguageTextTranslations as MultiLanguageTextTranslations
import Dynamic.NamespacingServer as NamespacingServer
import Dynamic.NamespacingTranslations as NamespacingTranslations
import Dynamic.NestedHtmlServer as NestedHtmlServer
import Dynamic.NestedHtmlTranslations as NestedHtmlTranslations
import Dynamic.NestedInterpolationServer as NestedInterpolationServer
import Dynamic.NestedInterpolationTranslations as NestedInterpolationTranslations
import Dynamic.NumberFormatServer as NumberFormatServer
import Dynamic.NumberFormatTranslations as NumberFormatTranslations
import Dynamic.PluralServer as PluralServer
import Dynamic.PluralTranslations as PluralTranslations
import Dynamic.SimpleHtmlServer as SimpleHtmlServer
import Dynamic.SimpleHtmlTranslations as SimpleHtmlTranslations
import Dynamic.SimpleI18nFirstServer as SimpleI18nFirstServer
import Dynamic.SimpleI18nFirstTranslations as SimpleI18nFirstTranslations
import Dynamic.SingleInterpolationServer as SingleInterpolationServer
import Dynamic.SingleInterpolationTranslations as SingleInterpolationTranslations
import Dynamic.SingleTextServer as SingleTextServer
import Dynamic.SingleTextTranslations as SingleTextTranslations
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
    describe "single text | dynamic"
        [ test "returns the expected translated text" <|
            \_ ->
                sendRequest SingleTextServer.server
                    "messages.en.json"
                    (SingleTextTranslations.decodeMessages SingleTextTranslations.En)
                    |> Result.map ((|>) (SingleTextTranslations.init { lang = SingleTextTranslations.En, path = "" }))
                    |> Result.map SingleTextTranslations.singleText
                    |> Expect.equal (Ok "the text")
        ]


multiLanguageText : Test
multiLanguageText =
    describe "multi language text | dynamic"
        [ test "returns the expected translated text in english" <|
            \_ ->
                sendRequest MultiLanguageTextServer.server
                    "messages.en.json"
                    (MultiLanguageTextTranslations.decodeMessages MultiLanguageTextTranslations.En)
                    |> Result.map ((|>) (MultiLanguageTextTranslations.init { lang = MultiLanguageTextTranslations.En, path = "" }))
                    |> Result.map MultiLanguageTextTranslations.text
                    |> Expect.equal (Ok "english text")
        , test "returns the expected translated text in german" <|
            \_ ->
                sendRequest MultiLanguageTextServer.server
                    "messages.de.json"
                    (MultiLanguageTextTranslations.decodeMessages MultiLanguageTextTranslations.De)
                    |> Result.map ((|>) (MultiLanguageTextTranslations.init { lang = MultiLanguageTextTranslations.De, path = "" }))
                    |> Result.map MultiLanguageTextTranslations.text
                    |> Expect.equal (Ok "german text")
        ]


singleInterpolation : Test
singleInterpolation =
    describe "single interpolation | dynamic"
        [ test "interpolates the given value at the correct position" <|
            \_ ->
                sendRequest SingleInterpolationServer.server
                    "messages.en.json"
                    (SingleInterpolationTranslations.decodeMessages SingleInterpolationTranslations.En)
                    |> Result.map ((|>) (SingleInterpolationTranslations.init { lang = SingleInterpolationTranslations.En, path = "" }))
                    |> Result.map (SingleInterpolationTranslations.text "world")
                    |> Expect.equal (Ok "hello world!")
        ]


multiInterpolation : Test
multiInterpolation =
    describe "multi interpolation | dynamic"
        [ test "interpolates the given values at the correct positions" <|
            \_ ->
                sendRequest MultiInterpolationServer.server
                    "messages.en.json"
                    (MultiInterpolationTranslations.decodeMessages MultiInterpolationTranslations.En)
                    |> Result.map ((|>) (MultiInterpolationTranslations.init { lang = MultiInterpolationTranslations.En, path = "" }))
                    |> Result.map (MultiInterpolationTranslations.greeting { timeOfDay = "morning", name = "my dear" })
                    |> Expect.equal (Ok "Good morning, my dear")
        , test "works for languages that do not use all interpolated values" <|
            \_ ->
                sendRequest MultiInterpolationServer.server
                    "messages.de.json"
                    (MultiInterpolationTranslations.decodeMessages MultiInterpolationTranslations.De)
                    |> Result.map ((|>) (MultiInterpolationTranslations.init { lang = MultiInterpolationTranslations.De, path = "" }))
                    |> Result.map (MultiInterpolationTranslations.greeting { timeOfDay = "Morgen", name = "Doesn't matter" })
                    |> Expect.equal (Ok "Guten Morgen")
        , test "works if languages interpolate values in different orders" <|
            \_ ->
                sendRequest MultiInterpolationServer.server
                    "messages.yoda.json"
                    (MultiInterpolationTranslations.decodeMessages MultiInterpolationTranslations.Yoda)
                    |> Result.map ((|>) (MultiInterpolationTranslations.init { lang = MultiInterpolationTranslations.Yoda, path = "" }))
                    |> Result.map (MultiInterpolationTranslations.greeting { timeOfDay = "morning", name = "Luke" })
                    |> Expect.equal (Ok "Luke, good morning")
        ]


i18nLastSimple : Test
i18nLastSimple =
    describe "generated code with i18nArgLast flag"
        [ test "single text" <|
            \_ ->
                sendRequest SimpleI18nFirstServer.server
                    "messages.en.json"
                    (SimpleI18nFirstTranslations.decodeMessages SimpleI18nFirstTranslations.En)
                    |> Result.map ((|>) (SimpleI18nFirstTranslations.init { lang = SimpleI18nFirstTranslations.En, path = "" }))
                    |> Result.map SimpleI18nFirstTranslations.singleText
                    |> Expect.equal (Ok "the text")
        , test "single interpolation" <|
            \_ ->
                sendRequest SimpleI18nFirstServer.server
                    "messages.en.json"
                    (SimpleI18nFirstTranslations.decodeMessages SimpleI18nFirstTranslations.En)
                    |> Result.map ((|>) (SimpleI18nFirstTranslations.init { lang = SimpleI18nFirstTranslations.En, path = "" }))
                    |> Result.map (\i18n -> SimpleI18nFirstTranslations.interpolation i18n "world")
                    |> Expect.equal (Ok "Hello world!")
        , test "multi interpolation" <|
            \_ ->
                sendRequest SimpleI18nFirstServer.server
                    "messages.en.json"
                    (SimpleI18nFirstTranslations.decodeMessages SimpleI18nFirstTranslations.En)
                    |> Result.map ((|>) (SimpleI18nFirstTranslations.init { lang = SimpleI18nFirstTranslations.En, path = "" }))
                    |> Result.map (\i18n -> SimpleI18nFirstTranslations.greeting i18n { timeOfDay = "evening", name = "Sir" })
                    |> Expect.equal (Ok "Good evening, Sir")
        ]


interpolationMatchCase : Test
interpolationMatchCase =
    describe "interpolation match case | dynamic"
        [ test "interpolates the correct value for female gender" <|
            \_ ->
                sendRequest InterpolationMatchServer.server
                    "messages.en.json"
                    (InterpolationMatchTranslations.decodeMessages InterpolationMatchTranslations.En)
                    |> Result.map ((|>) (InterpolationMatchTranslations.init { lang = InterpolationMatchTranslations.En, path = "" }))
                    |> Result.map (InterpolationMatchTranslations.text "female")
                    |> Expect.equal (Ok "She bought a cat.")
        , test "interpolates the correct value for male gender" <|
            \_ ->
                sendRequest InterpolationMatchServer.server
                    "messages.en.json"
                    (InterpolationMatchTranslations.decodeMessages InterpolationMatchTranslations.En)
                    |> Result.map ((|>) (InterpolationMatchTranslations.init { lang = InterpolationMatchTranslations.En, path = "" }))
                    |> Result.map (InterpolationMatchTranslations.text "male")
                    |> Expect.equal (Ok "He bought a cat.")
        , test "interpolates the default value for other values" <|
            \_ ->
                sendRequest InterpolationMatchServer.server
                    "messages.en.json"
                    (InterpolationMatchTranslations.decodeMessages InterpolationMatchTranslations.En)
                    |> Result.map ((|>) (InterpolationMatchTranslations.init { lang = InterpolationMatchTranslations.En, path = "" }))
                    |> Result.map (InterpolationMatchTranslations.text "anything else")
                    |> Expect.equal (Ok "It bought a cat.")
        ]


nestedInterpolation : Test
nestedInterpolation =
    describe "nested interpolation | dynamic" <|
        let
            i18n =
                sendRequest NestedInterpolationServer.server
                    "messages.de.json"
                    (NestedInterpolationTranslations.decodeMessages NestedInterpolationTranslations.De)
                    |> Result.map ((|>) (NestedInterpolationTranslations.init { lang = NestedInterpolationTranslations.De, path = "" }))
        in
        [ test "interpolates the correct values for 'Ich'" <|
            \_ ->
                i18n
                    |> Result.map (NestedInterpolationTranslations.text { pronoun = "Ich", objectsToBuy = "Gemüse" })
                    |> Expect.equal (Ok "Ich kaufe Gemüse.")
        , test "interpolates the correct values for 'Du'" <|
            \_ ->
                i18n
                    |> Result.map (NestedInterpolationTranslations.text { pronoun = "Du", objectsToBuy = "Obst" })
                    |> Expect.equal (Ok "Du kaufst Obst.")
        , test "interpolates the default value for other values" <|
            \_ ->
                i18n
                    |> Result.map (NestedInterpolationTranslations.text { pronoun = "Er", objectsToBuy = "Fleisch" })
                    |> Expect.equal (Ok "Er kauft Fleisch.")
        ]


numberFormatCase : Test
numberFormatCase =
    describe "number format | dynamic"
        [ test "type checks" <|
            \_ ->
                sendRequest NumberFormatServer.server
                    "messages.en.json"
                    (NumberFormatTranslations.decodeMessages NumberFormatTranslations.En)
                    |> Result.map ((|>) (NumberFormatTranslations.init { intl = Util.emptyIntl, lang = NumberFormatTranslations.En, path = "" }))
                    |> Result.map (NumberFormatTranslations.text 12.34)
                    -- This is expected since we cannot get the actual browser intl API in the test
                    -- We do not want to test the intl-proxy package here, so the fact that the generated
                    -- code typechecks is enough here.
                    |> Expect.equal (Ok "Price: ")
        ]


dateFormatCase : Test
dateFormatCase =
    describe "date format | dynamic"
        [ test "type checks" <|
            \_ ->
                sendRequest DateFormatServer.server
                    "messages.en.json"
                    (DateFormatTranslations.decodeMessages DateFormatTranslations.En)
                    |> Result.map ((|>) (DateFormatTranslations.init { intl = Util.emptyIntl, lang = DateFormatTranslations.En, path = "" }))
                    |> Result.map (DateFormatTranslations.text <| Time.millisToPosix 9000)
                    -- This is expected since we cannot get the actual browser intl API in the test
                    -- We do not want to test the intl-proxy package here, so the fact that the generated
                    -- code typechecks is enough here.
                    |> Expect.equal (Ok "Today: ")
        ]


pluralCase : Test
pluralCase =
    describe "plural | dynamic"
        [ test "type checks" <|
            \_ ->
                sendRequest PluralServer.server "messages.en.json" (PluralTranslations.decodeMessages PluralTranslations.En)
                    |> Result.map ((|>) (PluralTranslations.init { intl = Util.emptyIntl, lang = PluralTranslations.En, path = "" }))
                    |> Result.map (PluralTranslations.text 4)
                    -- Due to the absent intl api, we can only test the default case here
                    |> Expect.equal (Ok "I met many people.")
        ]


hashRegressionTest : Test
hashRegressionTest =
    describe "content hashing of json files"
        [ test "produces a consistent hash for english content" <|
            \_ ->
                sendRequest HashServer.server "messages.en.2061212766.json" (HashTranslations.decodeMessages HashTranslations.En)
                    |> Result.map ((|>) (HashTranslations.init { lang = HashTranslations.En, path = "" }))
                    |> Result.map HashTranslations.text
                    |> Expect.equal (Ok "english text")
        , test "produces a consistent hash for german content" <|
            \_ ->
                sendRequest HashServer.server "messages.de.1722545000.json" (HashTranslations.decodeMessages HashTranslations.De)
                    |> Result.map ((|>) (HashTranslations.init { lang = HashTranslations.De, path = "" }))
                    |> Result.map HashTranslations.text
                    |> Expect.equal (Ok "german text")
        ]


simpleHtml : Test
simpleHtml =
    describe "simple html | dynamic"
        [ test "produces the correct html element and text content" <|
            \_ ->
                sendRequest SimpleHtmlServer.server "messages.en.json" (SimpleHtmlTranslations.decodeMessages SimpleHtmlTranslations.En)
                    |> Result.map ((|>) (SimpleHtmlTranslations.init { lang = SimpleHtmlTranslations.En, path = "" }))
                    |> expectOkWith
                        (SimpleHtmlTranslations.html []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "a" ]
                            >> Query.has [ Selector.text "Click me" ]
                        )
        , test "generates html attribute from translation file" <|
            \_ ->
                sendRequest SimpleHtmlServer.server "messages.en.json" (SimpleHtmlTranslations.decodeMessages SimpleHtmlTranslations.En)
                    |> Result.map ((|>) (SimpleHtmlTranslations.init { lang = SimpleHtmlTranslations.En, path = "" }))
                    |> expectOkWith
                        (SimpleHtmlTranslations.html []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "a" ]
                            >> Query.has [ Selector.attribute <| Html.Attributes.href "/" ]
                        )
        , test "passes extra attributes given at runtime" <|
            \_ ->
                sendRequest SimpleHtmlServer.server "messages.en.json" (SimpleHtmlTranslations.decodeMessages SimpleHtmlTranslations.En)
                    |> Result.map ((|>) (SimpleHtmlTranslations.init { lang = SimpleHtmlTranslations.En, path = "" }))
                    |> expectOkWith
                        (SimpleHtmlTranslations.html [ Html.Attributes.class "link" ]
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "a" ]
                            >> Query.has [ Selector.class "link" ]
                        )
        ]


nestedHtml : Test
nestedHtml =
    describe "nested html | dynamic"
        [ test "produces the correct outer html element" <|
            \_ ->
                sendRequest NestedHtmlServer.server "messages.en.json" (NestedHtmlTranslations.decodeMessages NestedHtmlTranslations.En)
                    |> Result.map ((|>) (NestedHtmlTranslations.init { lang = NestedHtmlTranslations.En, path = "" }))
                    |> expectOkWith
                        (NestedHtmlTranslations.html { image = [], link = [ Html.Attributes.class "nestedLink" ], text = [] }
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "a" ]
                            >> Query.has
                                [ Selector.attribute <| Html.Attributes.href "/"
                                , Selector.text "!"
                                , Selector.class "nestedLink"
                                ]
                        )
        , test "produces the correct inner span element" <|
            \_ ->
                sendRequest NestedHtmlServer.server "messages.en.json" (NestedHtmlTranslations.decodeMessages NestedHtmlTranslations.En)
                    |> Result.map ((|>) (NestedHtmlTranslations.init { lang = NestedHtmlTranslations.En, path = "" }))
                    |> expectOkWith
                        (NestedHtmlTranslations.html { image = [], link = [], text = [ Html.Attributes.class "theText" ] }
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "a" ]
                            >> Query.find [ Selector.tag "span" ]
                            >> Query.has
                                [ Selector.attribute <| Html.Attributes.width 100
                                , Selector.attribute <| Html.Attributes.height 50
                                , Selector.text "Click me"
                                , Selector.class "theText"
                                ]
                        )
        , test "produces the correct inner img element" <|
            \_ ->
                sendRequest NestedHtmlServer.server "messages.en.json" (NestedHtmlTranslations.decodeMessages NestedHtmlTranslations.En)
                    |> Result.map ((|>) (NestedHtmlTranslations.init { lang = NestedHtmlTranslations.En, path = "" }))
                    |> expectOkWith
                        (NestedHtmlTranslations.html { image = [ Html.Attributes.class "nestedImage" ], link = [], text = [] }
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "a" ]
                            >> Query.find [ Selector.tag "img" ]
                            >> Query.has
                                [ Selector.attribute <| Html.Attributes.src "/imgUrl.png"
                                , Selector.class "nestedImage"
                                ]
                        )
        ]


mixedHtmlAndInterpolation : Test
mixedHtmlAndInterpolation =
    describe "mixed html and interpolation | dynamic"
        [ test "shows expected content for admin role" <|
            \_ ->
                sendRequest HtmlInterpolationServer.server "messages.en.json" (HtmlInterpolationTranslations.decodeMessages HtmlInterpolationTranslations.En)
                    |> Result.map ((|>) (HtmlInterpolationTranslations.init { lang = HtmlInterpolationTranslations.En, path = "" }))
                    |> expectOkWith
                        (HtmlInterpolationTranslations.text { adminLink = "/admin", role = "admin", username = "A. Dmin" } []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.has
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
                sendRequest HtmlInterpolationServer.server "messages.en.json" (HtmlInterpolationTranslations.decodeMessages HtmlInterpolationTranslations.En)
                    |> Result.map ((|>) (HtmlInterpolationTranslations.init { lang = HtmlInterpolationTranslations.En, path = "" }))
                    |> expectOkWith
                        (HtmlInterpolationTranslations.text { adminLink = "/admin", role = "normal", username = "Justin Normal" } []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.has
                                [ Selector.text "You ("
                                , Selector.text "Justin Normal"
                                , Selector.text ") are not an admin."
                                ]
                        )
        , test "shows expected content for default role" <|
            \_ ->
                sendRequest HtmlInterpolationServer.server "messages.en.json" (HtmlInterpolationTranslations.decodeMessages HtmlInterpolationTranslations.En)
                    |> Result.map ((|>) (HtmlInterpolationTranslations.init { lang = HtmlInterpolationTranslations.En, path = "" }))
                    |> expectOkWith
                        (HtmlInterpolationTranslations.text { adminLink = "/admin", role = "undefined", username = "Does not matter" } []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.has [ Selector.text "You are not logged in." ]
                        )
        ]


htmlAndIntl : Test
htmlAndIntl =
    describe "html mixed with intl functions | dynamic"
        [ test "numberFormat typechecks" <|
            \_ ->
                sendRequest HtmlIntlServer.server "messages.en.json" (HtmlIntlTranslations.decodeMessages HtmlIntlTranslations.En)
                    |> Result.map ((|>) (HtmlIntlTranslations.init { intl = Util.emptyIntl, lang = HtmlIntlTranslations.En, path = "" }))
                    |> expectOkWith
                        (HtmlIntlTranslations.formatNumber 4.2 []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.has
                                [ Selector.text "Price: "
                                , Selector.containing [ Selector.tag "b" ]
                                ]
                        )
        , test "dateFormat typechecks" <|
            \_ ->
                sendRequest HtmlIntlServer.server "messages.en.json" (HtmlIntlTranslations.decodeMessages HtmlIntlTranslations.En)
                    |> Result.map ((|>) (HtmlIntlTranslations.init { intl = Util.emptyIntl, lang = HtmlIntlTranslations.En, path = "" }))
                    |> expectOkWith
                        (HtmlIntlTranslations.formatDate (Time.millisToPosix 1000) []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.has
                                [ Selector.text "Today is "
                                , Selector.containing [ Selector.tag "em" ]
                                ]
                        )
        , test "pluralRules typechecks" <|
            \_ ->
                sendRequest HtmlIntlServer.server "messages.en.json" (HtmlIntlTranslations.decodeMessages HtmlIntlTranslations.En)
                    |> Result.map ((|>) (HtmlIntlTranslations.init { intl = Util.emptyIntl, lang = HtmlIntlTranslations.En, path = "" }))
                    |> expectOkWith
                        (HtmlIntlTranslations.pluralRules 5 []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "p" ]
                            >> Query.has [ Selector.text "5" ]
                        )
        , test "normal html functions still work" <|
            \_ ->
                sendRequest HtmlIntlServer.server "messages.en.json" (HtmlIntlTranslations.decodeMessages HtmlIntlTranslations.En)
                    |> Result.map ((|>) (HtmlIntlTranslations.init { intl = Util.emptyIntl, lang = HtmlIntlTranslations.En, path = "" }))
                    |> expectOkWith
                        (HtmlIntlTranslations.normalHtml []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "p" ]
                            >> Query.has [ Selector.text "just some html" ]
                        )
        ]


multipleBundles : Test
multipleBundles =
    describe "multiple bundles | dynamic"
        [ test "finds the first bundles text when first bundle is loaded" <|
            \_ ->
                sendRequest MultiBundleServer.server "bundle_1.en.json" (MultiBundleTranslations.decodeBundle1 MultiBundleTranslations.En)
                    |> Result.map ((|>) (MultiBundleTranslations.init { lang = MultiBundleTranslations.En, path = "" }))
                    |> Result.map MultiBundleTranslations.text1
                    |> Expect.equal (Ok "text from bundle 1")
        , test "finds the second bundles text when second bundle is loaded" <|
            \_ ->
                sendRequest MultiBundleServer.server "bundle_2.en.json" (MultiBundleTranslations.decodeBundle2 MultiBundleTranslations.En)
                    |> Result.map ((|>) (MultiBundleTranslations.init { lang = MultiBundleTranslations.En, path = "" }))
                    |> Result.map MultiBundleTranslations.text2
                    |> Expect.equal (Ok "text from bundle 2")
        , test "does not find the first bundles text when second bundle is loaded" <|
            \_ ->
                sendRequest MultiBundleServer.server "bundle_2.en.json" (MultiBundleTranslations.decodeBundle2 MultiBundleTranslations.En)
                    |> Result.map ((|>) (MultiBundleTranslations.init { lang = MultiBundleTranslations.En, path = "" }))
                    |> Result.map MultiBundleTranslations.text1
                    |> Expect.equal (Ok "")
        , test "finds both texts if both bundles are loaded" <|
            \_ ->
                sendRequest MultiBundleServer.server "bundle_1.en.json" (MultiBundleTranslations.decodeBundle1 MultiBundleTranslations.En)
                    |> Result.andThen
                        (\addTranslations1 ->
                            sendRequest MultiBundleServer.server "bundle_2.en.json" (MultiBundleTranslations.decodeBundle2 MultiBundleTranslations.En)
                                |> Result.map ((|>) (addTranslations1 <| MultiBundleTranslations.init { lang = MultiBundleTranslations.En, path = "" }))
                        )
                    |> expectOkWith
                        (Expect.all
                            [ MultiBundleTranslations.text1 >> Expect.equal "text from bundle 1"
                            , MultiBundleTranslations.text2 >> Expect.equal "text from bundle 2"
                            ]
                        )
        ]


namespacing : Test
namespacing =
    describe "namespacing | dynamic"
        [ test "escapes elm keywords" <|
            \_ ->
                sendRequest NamespacingServer.server "messages.en.json" (NamespacingTranslations.decodeMessages NamespacingTranslations.En)
                    |> Result.map ((|>) (NamespacingTranslations.init { lang = NamespacingTranslations.En, path = "" }))
                    |> Result.map NamespacingTranslations.let_
                    |> Expect.equal (Ok "elm keyword")
        , test "escapes top level function names" <|
            \_ ->
                sendRequest NamespacingServer.server "messages.en.json" (NamespacingTranslations.decodeMessages NamespacingTranslations.En)
                    |> Result.map ((|>) (NamespacingTranslations.init { lang = NamespacingTranslations.En, path = "" }))
                    |> Result.map NamespacingTranslations.init_
                    |> Expect.equal (Ok "reserved name")
        ]


escapedCurlyBrackets : Test
escapedCurlyBrackets =
    describe "escaped curly brackets"
        [ test "works without parsing problems for text" <|
            \_ ->
                sendRequest EscapeServer.server "messages.en.json" (EscapeTranslations.decodeMessages EscapeTranslations.En)
                    |> Result.map ((|>) (EscapeTranslations.init { lang = EscapeTranslations.En, path = "" }))
                    |> Result.map (EscapeTranslations.text "interp")
                    |> Expect.equal (Ok "escaped interpolation { $var }, actual interp")
        , test "works without parsing problems for html" <|
            \_ ->
                sendRequest EscapeServer.server "messages.en.json" (EscapeTranslations.decodeMessages EscapeTranslations.En)
                    |> Result.map ((|>) (EscapeTranslations.init { lang = EscapeTranslations.En, path = "" }))
                    |> expectOkWith
                        (EscapeTranslations.html "interp" []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.has
                                [ Selector.text "escaped interpolation "
                                , Selector.text "{"
                                , Selector.text "$var"
                                , Selector.text "}"
                                , Selector.containing [ Selector.tag "b", Selector.text "interp" ]
                                ]
                        )
        ]


escapedQuotationMarks : Test
escapedQuotationMarks =
    describe "escaped quotation marks"
        [ test "works without parsing problems for text" <|
            \_ ->
                sendRequest EscapeServer.server "messages.en.json" (EscapeTranslations.decodeMessages EscapeTranslations.En)
                    |> Result.map ((|>) (EscapeTranslations.init { lang = EscapeTranslations.En, path = "" }))
                    |> Result.map (EscapeTranslations.quotationMarkAndBackslash "abc")
                    |> Expect.equal (Ok "just a \\ and \"quotation mark\"abc")
        , test "works without parsing problems for html" <|
            \_ ->
                sendRequest EscapeServer.server "messages.en.json" (EscapeTranslations.decodeMessages EscapeTranslations.En)
                    |> Result.map ((|>) (EscapeTranslations.init { lang = EscapeTranslations.En, path = "" }))
                    |> expectOkWith
                        (EscapeTranslations.quotationMarkAndBackslashHtml "abc" []
                            >> Html.div []
                            >> Query.fromHtml
                            >> Query.has
                                [ Selector.text "just a "
                                , Selector.text "\\"
                                , Selector.text " and \"quotation mark\""
                                , Selector.containing [ Selector.tag "b", Selector.text "abc" ]
                                ]
                        )
        ]


escapedPipeSymbols : Test
escapedPipeSymbols =
    describe "escaped pipe symbol"
        [ test "interpolation match works" <|
            \_ ->
                sendRequest EscapeServer.server "messages.en.json" (EscapeTranslations.decodeMessages EscapeTranslations.En)
                    |> Result.map ((|>) (EscapeTranslations.init { lang = EscapeTranslations.En, path = "" }))
                    |> Result.map (EscapeTranslations.pipeOperatorInterpolationCase "abc")
                    |> Expect.equal (Ok "just a | pipe")
        , test "html works" <|
            \_ ->
                sendRequest EscapeServer.server "messages.en.json" (EscapeTranslations.decodeMessages EscapeTranslations.En)
                    |> Result.map ((|>) (EscapeTranslations.init { lang = EscapeTranslations.En, path = "" }))
                    |> expectOkWith
                        (EscapeTranslations.pipeOperatorHtml []
                            >> Html.p []
                            >> Query.fromHtml
                            >> Query.find [ Selector.tag "div" ]
                            >> Query.has
                                [ Selector.text "just a "
                                , Selector.text "|"
                                , Selector.text "pipe"
                                ]
                        )
        ]


fallback : Test
fallback =
    describe "fallback to another language | dynamic"
        [ test "falls back to the fallback language successfully" <|
            \_ ->
                sendRequest FallbackServer.server "messages.en.json" (FallbackTranslations.decodeMessages FallbackTranslations.En)
                    |> Result.map ((|>) (FallbackTranslations.init { lang = FallbackTranslations.De, path = "" }))
                    |> Result.map FallbackTranslations.justInGerman
                    |> Expect.equal (Ok "more german text")
        , test "still uses other existing values" <|
            \_ ->
                sendRequest FallbackServer.server "messages.en.json" (FallbackTranslations.decodeMessages FallbackTranslations.En)
                    |> Result.map ((|>) (FallbackTranslations.init { lang = FallbackTranslations.De, path = "" }))
                    |> Result.map FallbackTranslations.text
                    |> Expect.equal (Ok "english text")
        ]


getCurrentLanguage : Test
getCurrentLanguage =
    describe "get current language | dynamic"
        [ test "is correct for initial language" <|
            \_ ->
                MultiLanguageTextTranslations.init { lang = MultiLanguageTextTranslations.De, path = "" }
                    |> MultiLanguageTextTranslations.currentLanguage
                    |> Expect.equal MultiLanguageTextTranslations.De
        , test "is correct after switching" <|
            \_ ->
                MultiLanguageTextTranslations.init { lang = MultiLanguageTextTranslations.De, path = "" }
                    |> MultiLanguageTextTranslations.switchLanguage MultiLanguageTextTranslations.En (always ())
                    |> Tuple.first
                    |> MultiLanguageTextTranslations.currentLanguage
                    |> Expect.equal MultiLanguageTextTranslations.En
        , test "works for intl case" <|
            \_ ->
                HtmlIntlTranslations.init { intl = Util.emptyIntl, lang = HtmlIntlTranslations.En, path = "" }
                    |> HtmlIntlTranslations.currentLanguage
                    |> Expect.equal HtmlIntlTranslations.En
        ]


getArrivedLanguage : Test
getArrivedLanguage =
    describe "get arrived language"
        [ test "is correct for initial language" <|
            \_ ->
                MultiBundleLanguageTranslations.init { lang = MultiBundleLanguageTranslations.De, path = "" }
                    |> MultiBundleLanguageTranslations.arrivedLanguage
                    |> Expect.equal MultiBundleLanguageTranslations.De
        , test "switchLanguage immediately switches arrived language since no translations have been loaded yet" <|
            \_ ->
                MultiBundleLanguageTranslations.init { lang = MultiBundleLanguageTranslations.De, path = "" }
                    |> MultiBundleLanguageTranslations.switchLanguage MultiBundleLanguageTranslations.En (always ())
                    |> Tuple.first
                    |> MultiBundleLanguageTranslations.arrivedLanguage
                    |> Expect.equal MultiBundleLanguageTranslations.En
        , test "switchLanguage does not switch arrived language if translations were loaded" <|
            \_ ->
                let
                    init =
                        MultiBundleLanguageTranslations.init { lang = MultiBundleLanguageTranslations.De, path = "" }
                in
                sendRequest MultiBundleLanguageServer.server "bundle_1.de.json" (MultiBundleLanguageTranslations.decodeBundle1 MultiBundleLanguageTranslations.De)
                    |> Result.map ((|>) init)
                    |> Result.map (MultiBundleLanguageTranslations.switchLanguage MultiBundleLanguageTranslations.En (always ()))
                    |> Result.map Tuple.first
                    |> Result.map MultiBundleLanguageTranslations.arrivedLanguage
                    |> Expect.equal (Ok MultiBundleLanguageTranslations.De)
        , test "switchLanguage switches arrived language if translations in the new language are loaded" <|
            \_ ->
                let
                    init =
                        MultiBundleLanguageTranslations.init { lang = MultiBundleLanguageTranslations.De, path = "" }

                    req_1_de =
                        sendRequest MultiBundleLanguageServer.server "bundle_1.de.json" (MultiBundleLanguageTranslations.decodeBundle1 MultiBundleLanguageTranslations.De)

                    req_1_en =
                        sendRequest MultiBundleLanguageServer.server "bundle_1.en.json" (MultiBundleLanguageTranslations.decodeBundle1 MultiBundleLanguageTranslations.En)
                in
                req_1_de
                    |> Result.map ((|>) init)
                    |> Result.map (MultiBundleLanguageTranslations.switchLanguage MultiBundleLanguageTranslations.En (always ()))
                    |> Result.map Tuple.first
                    |> Result.andThen (\i18n -> Result.map ((|>) i18n) req_1_en)
                    |> Result.map MultiBundleLanguageTranslations.arrivedLanguage
                    |> Expect.equal (Ok MultiBundleLanguageTranslations.En)
        ]


expectOkWith : (a -> Expect.Expectation) -> Result x a -> Expect.Expectation
expectOkWith expect result =
    case result of
        Err _ ->
            Expect.fail "Failed to load translation file"

        Ok ok ->
            expect ok
