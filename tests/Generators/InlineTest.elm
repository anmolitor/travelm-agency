module Generators.InlineTest exposing (..)

import Expect
import Html
import Html.Attributes
import Inline.ComplexI18nFirstTranslations
import Inline.DateFormatTranslations
import Inline.HtmlInterpolationTranslations
import Inline.HtmlIntlTranslations
import Inline.InterpolationMatchTranslations
import Inline.MultiBundleTranslations
import Inline.MultiInterpolationTranslations
import Inline.MultiLanguageTextTranslations
import Inline.NamespacingTranslations
import Inline.NestedHtmlTranslations
import Inline.NestedInterpolationTranslations
import Inline.NumberFormatTranslations
import Inline.PluralTranslations
import Inline.SimpleHtmlTranslations
import Inline.SimpleI18nFirstTranslations
import Inline.SingleInterpolationTranslations
import Inline.SingleTextTranslations
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Util


singleText : Test
singleText =
    describe "single text"
        [ test "returns the expected translated text" <|
            \_ ->
                Inline.SingleTextTranslations.init Inline.SingleTextTranslations.En
                    |> Inline.SingleTextTranslations.singleText
                    |> Expect.equal "the text"
        ]


multiLanguageText : Test
multiLanguageText =
    describe "multi language text"
        [ test "returns the expected translated text in english" <|
            \_ ->
                Inline.MultiLanguageTextTranslations.init Inline.MultiLanguageTextTranslations.En
                    |> Inline.MultiLanguageTextTranslations.text
                    |> Expect.equal "english text"
        , test "returns the expected translated text in german" <|
            \_ ->
                Inline.MultiLanguageTextTranslations.init Inline.MultiLanguageTextTranslations.De
                    |> Inline.MultiLanguageTextTranslations.text
                    |> Expect.equal "german text"
        ]


singleInterpolation : Test
singleInterpolation =
    describe "single interpolation"
        [ test "interpolates the given value at the correct position" <|
            \_ ->
                Inline.SingleInterpolationTranslations.init Inline.SingleInterpolationTranslations.En
                    |> Inline.SingleInterpolationTranslations.text "world"
                    |> Expect.equal "hello world!"
        ]


multiInterpolation : Test
multiInterpolation =
    describe "multi interpolation"
        [ test "interpolates the given values at the correct positions" <|
            \_ ->
                Inline.MultiInterpolationTranslations.init Inline.MultiInterpolationTranslations.En
                    |> Inline.MultiInterpolationTranslations.greeting { timeOfDay = "morning", name = "my dear" }
                    |> Expect.equal "Good morning, my dear"
        , test "works for languages that do not use all interpolated values" <|
            \_ ->
                Inline.MultiInterpolationTranslations.init Inline.MultiInterpolationTranslations.De
                    |> Inline.MultiInterpolationTranslations.greeting { timeOfDay = "Morgen", name = "Does not matter" }
                    |> Expect.equal "Guten Morgen"
        , test "works if languages interpolate values in different orders" <|
            \_ ->
                Inline.MultiInterpolationTranslations.init Inline.MultiInterpolationTranslations.Yoda
                    |> Inline.MultiInterpolationTranslations.greeting { timeOfDay = "morning", name = "Luke" }
                    |> Expect.equal "Luke, good morning"
        ]


i18nLastSimple : Test
i18nLastSimple =
    describe "generated code with i18nArgFirst flag"
        [ test "single text" <|
            \_ ->
                Inline.SimpleI18nFirstTranslations.init Inline.SimpleI18nFirstTranslations.En
                    |> Inline.SimpleI18nFirstTranslations.singleText
                    |> Expect.equal "the text"
        , test "single interpolation" <|
            \_ ->
                Inline.SimpleI18nFirstTranslations.interpolation
                    (Inline.SimpleI18nFirstTranslations.init Inline.SimpleI18nFirstTranslations.En)
                    "world"
                    |> Expect.equal "Hello world!"
        , test "multi interpolation" <|
            \_ ->
                Inline.SimpleI18nFirstTranslations.greeting
                    (Inline.SimpleI18nFirstTranslations.init Inline.SimpleI18nFirstTranslations.En)
                    { timeOfDay = "evening", name = "Sir" }
                    |> Expect.equal "Good evening, Sir"
        ]


interpolationMatchCase : Test
interpolationMatchCase =
    describe "interpolation match case"
        [ test "interpolates the correct value for female gender" <|
            \_ ->
                Inline.InterpolationMatchTranslations.init Inline.InterpolationMatchTranslations.En
                    |> Inline.InterpolationMatchTranslations.text "female"
                    |> Expect.equal "She bought a cat."
        , test "interpolates the correct value for male gender" <|
            \_ ->
                Inline.InterpolationMatchTranslations.init Inline.InterpolationMatchTranslations.En
                    |> Inline.InterpolationMatchTranslations.text "male"
                    |> Expect.equal "He bought a cat."
        , test "interpolates the default value for other values" <|
            \_ ->
                Inline.InterpolationMatchTranslations.init Inline.InterpolationMatchTranslations.En
                    |> Inline.InterpolationMatchTranslations.text "anything else"
                    |> Expect.equal "It bought a cat."
        ]


nestedInterpolation : Test
nestedInterpolation =
    describe "nested interpolation"
        [ test "interpolates the correct values for 'Ich'" <|
            \_ ->
                Inline.NestedInterpolationTranslations.init Inline.NestedInterpolationTranslations.De
                    |> Inline.NestedInterpolationTranslations.text { pronoun = "Ich", objectsToBuy = "Gemüse" }
                    |> Expect.equal "Ich kaufe Gemüse."
        , test "interpolates the correct values for 'Du'" <|
            \_ ->
                Inline.NestedInterpolationTranslations.init Inline.NestedInterpolationTranslations.De
                    |> Inline.NestedInterpolationTranslations.text { pronoun = "Du", objectsToBuy = "Obst" }
                    |> Expect.equal "Du kaufst Obst."
        , test "interpolates the default value for other values" <|
            \_ ->
                Inline.NestedInterpolationTranslations.init Inline.NestedInterpolationTranslations.De
                    |> Inline.NestedInterpolationTranslations.text { pronoun = "Er", objectsToBuy = "Fleisch" }
                    |> Expect.equal "Er kauft Fleisch."
        ]


numberFormatCase : Test
numberFormatCase =
    describe "number format"
        [ test "type checks" <|
            \_ ->
                Inline.NumberFormatTranslations.init Util.emptyIntl Inline.NumberFormatTranslations.En
                    |> Inline.NumberFormatTranslations.text 52.34
                    -- This is expected since we cannot get the actual browser intl API in the test
                    -- We do not want to test the intl-proxy package here, so the fact that the generated
                    -- code typechecks is enough here.
                    |> Expect.equal "Price: "
        ]


dateFormatCase : Test
dateFormatCase =
    describe "date format"
        [ test "type checks" <|
            \_ ->
                Inline.DateFormatTranslations.init Util.emptyIntl Inline.DateFormatTranslations.En
                    |> Inline.DateFormatTranslations.text (Time.millisToPosix 1000)
                    -- This is expected since we cannot get the actual browser intl API in the test
                    -- We do not want to test the intl-proxy package here, so the fact that the generated
                    -- code typechecks is enough here.
                    |> Expect.equal "Today: "
        ]


pluralCase : Test
pluralCase =
    describe "plural"
        [ test "type checks" <|
            \_ ->
                Inline.PluralTranslations.init Util.emptyIntl Inline.PluralTranslations.En
                    |> Inline.PluralTranslations.text 5
                    -- Due to the absent intl api, we can only test the default case here
                    |> Expect.equal "I met many people."
        ]


simpleHtml : Test
simpleHtml =
    describe "simple html"
        [ test "produces the correct html element and text content" <|
            \_ ->
                Inline.SimpleHtmlTranslations.init Inline.SimpleHtmlTranslations.En
                    |> Inline.SimpleHtmlTranslations.html []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has [ Selector.text "Click me" ]
        , test "generates html attribute from translation file" <|
            \_ ->
                Inline.SimpleHtmlTranslations.init Inline.SimpleHtmlTranslations.En
                    |> Inline.SimpleHtmlTranslations.html []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has [ Selector.attribute <| Html.Attributes.href "/" ]
        , test "passes extra attributes given at runtime" <|
            \_ ->
                Inline.SimpleHtmlTranslations.init Inline.SimpleHtmlTranslations.En
                    |> Inline.SimpleHtmlTranslations.html [ Html.Attributes.class "link" ]
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has [ Selector.class "link" ]
        ]


nestedHtml : Test
nestedHtml =
    describe "nested html"
        [ test "produces the correct outer html element" <|
            \_ ->
                Inline.NestedHtmlTranslations.init Inline.NestedHtmlTranslations.En
                    |> Inline.NestedHtmlTranslations.html { image = [], link = [ Html.Attributes.class "nestedLink" ], text = [] }
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has
                        [ Selector.attribute <| Html.Attributes.href "/"
                        , Selector.text "!"
                        , Selector.class "nestedLink"
                        ]
        , test "produces the correct inner span element" <|
            \_ ->
                Inline.NestedHtmlTranslations.init Inline.NestedHtmlTranslations.En
                    |> Inline.NestedHtmlTranslations.html { image = [], link = [], text = [ Html.Attributes.class "theText" ] }
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
        , test "produces the correct inner img element" <|
            \_ ->
                Inline.NestedHtmlTranslations.init Inline.NestedHtmlTranslations.En
                    |> Inline.NestedHtmlTranslations.html { image = [ Html.Attributes.class "nestedImage" ], link = [], text = [] }
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.find [ Selector.tag "img" ]
                    |> Query.has
                        [ Selector.attribute <| Html.Attributes.src "/imgUrl.png"
                        , Selector.class "nestedImage"
                        ]
        ]


mixedHtmlAndInterpolation : Test
mixedHtmlAndInterpolation =
    describe "mixed html and interpolation"
        [ test "shows expected content for admin role" <|
            \_ ->
                Inline.HtmlInterpolationTranslations.init Inline.HtmlInterpolationTranslations.En
                    |> Inline.HtmlInterpolationTranslations.text { adminLink = "/admin", role = "admin", username = "A. Dmin" } []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.text "Thanks for logging in. "
                        , Selector.containing
                            [ Selector.tag "a"
                            , Selector.attribute <| Html.Attributes.href "/admin"
                            , Selector.text "A. Dmin may click on this link."
                            ]
                        ]
        , test "shows expected content for normal role" <|
            \_ ->
                Inline.HtmlInterpolationTranslations.init Inline.HtmlInterpolationTranslations.En
                    |> Inline.HtmlInterpolationTranslations.text { adminLink = "/admin", role = "normal", username = "Justin Normal" } []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "You (Justin Normal) are not an admin." ]
        , test "shows expected content for default role" <|
            \_ ->
                Inline.HtmlInterpolationTranslations.init Inline.HtmlInterpolationTranslations.En
                    |> Inline.HtmlInterpolationTranslations.text { adminLink = "/admin", role = "undefined", username = "Does not matter" } []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "You are not logged in." ]
        ]


htmlAndIntl : Test
htmlAndIntl =
    describe "html mixed with intl functions"
        [ test "numberFormat typechecks" <|
            \_ ->
                Inline.HtmlIntlTranslations.init Util.emptyIntl Inline.HtmlIntlTranslations.En
                    |> Inline.HtmlIntlTranslations.formatNumber 4.2 []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.text "Price: "
                        , Selector.containing [ Selector.tag "b" ]
                        ]
        , test "dateFormat typechecks" <|
            \_ ->
                Inline.HtmlIntlTranslations.init Util.emptyIntl Inline.HtmlIntlTranslations.En
                    |> Inline.HtmlIntlTranslations.formatDate (Time.millisToPosix 1000) []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.text "Today is "
                        , Selector.containing [ Selector.tag "em" ]
                        ]
        , test "pluralRules typechecks" <|
            \_ ->
                Inline.HtmlIntlTranslations.init Util.emptyIntl Inline.HtmlIntlTranslations.En
                    |> Inline.HtmlIntlTranslations.pluralRules 5 []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "p" ]
                    |> Query.has [ Selector.text "5" ]
        ]


multipleBundles : Test
multipleBundles =
    describe "multiple bundles"
        [ test "keys from first bundle works" <|
            \_ ->
                Inline.MultiBundleTranslations.init Inline.MultiBundleTranslations.En
                    |> Inline.MultiBundleTranslations.text1
                    |> Expect.equal "text from bundle 1"
        , test "keys from second bundle works" <|
            \_ ->
                Inline.MultiBundleTranslations.init Inline.MultiBundleTranslations.En
                    |> Inline.MultiBundleTranslations.text2
                    |> Expect.equal "text from bundle 2"
        ]


namespacing : Test
namespacing =
    describe "namespacing"
        [ test "escapes elm keywords" <|
            \_ ->
                Inline.NamespacingTranslations.init Inline.NamespacingTranslations.En
                    |> Inline.NamespacingTranslations.let_
                    |> Expect.equal "elm keyword"
        , test "escapes top level function names" <|
            \_ ->
                Inline.NamespacingTranslations.init Inline.NamespacingTranslations.En
                    |> Inline.NamespacingTranslations.init_
                    |> Expect.equal "reserved name"
        ]


complexI18nFirst : Test
complexI18nFirst =
    describe "complex i18n first"
        [ test "case interpolation" <|
            \_ ->
                Inline.ComplexI18nFirstTranslations.interpolationCase
                    (Inline.ComplexI18nFirstTranslations.init Util.emptyIntl Inline.ComplexI18nFirstTranslations.En)
                    "one"
                    |> Expect.equal "One"
        , test "html" <|
            \_ ->
                Inline.ComplexI18nFirstTranslations.html
                    (Inline.ComplexI18nFirstTranslations.init Util.emptyIntl Inline.ComplexI18nFirstTranslations.En)
                    "abc"
                    [ Html.Attributes.class "swag" ]
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has
                        [ Selector.class "swag"
                        , Selector.attribute <| Html.Attributes.href "/"
                        , Selector.text "abc"
                        ]
        , test "numberFormat" <|
            \_ ->
                Inline.ComplexI18nFirstTranslations.numberFormat
                    (Inline.ComplexI18nFirstTranslations.init Util.emptyIntl Inline.ComplexI18nFirstTranslations.En)
                    5.0
                    -- as usual, we can only test for no compilation errors here, since Intl is mocked
                    |> Expect.equal ""
        ]
