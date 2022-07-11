module Generators.InlineTest exposing (..)

import Expect
import Html
import Html.Attributes
import Inline.DateFormatTranslations
import Inline.InterpolationMatchTranslations
import Inline.MultiInterpolationTranslations
import Inline.MultiLanguageTextTranslations
import Inline.NestedInterpolationTranslations
import Inline.NumberFormatTranslations
import Inline.PluralTranslations
import Inline.SimpleHtmlTranslations
import Inline.SimpleI18nLastTranslations
import Inline.SingleInterpolationTranslations
import Inline.SingleTextTranslations
import Json.Encode
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
                Inline.SingleInterpolationTranslations.text (Inline.SingleInterpolationTranslations.init Inline.SingleInterpolationTranslations.En)
                    "world"
                    |> Expect.equal "hello world!"
        ]


multiInterpolation : Test
multiInterpolation =
    describe "multi interpolation"
        [ test "interpolates the given values at the correct positions" <|
            \_ ->
                Inline.MultiInterpolationTranslations.greeting (Inline.MultiInterpolationTranslations.init Inline.MultiInterpolationTranslations.En)
                    { timeOfDay = "morning", name = "my dear" }
                    |> Expect.equal "Good morning, my dear"
        , test "works for languages that do not use all interpolated values" <|
            \_ ->
                Inline.MultiInterpolationTranslations.greeting (Inline.MultiInterpolationTranslations.init Inline.MultiInterpolationTranslations.De)
                    { timeOfDay = "Morgen", name = "Does not matter" }
                    |> Expect.equal "Guten Morgen"
        , test "works if languages interpolate values in different orders" <|
            \_ ->
                Inline.MultiInterpolationTranslations.greeting (Inline.MultiInterpolationTranslations.init Inline.MultiInterpolationTranslations.Yoda)
                    { timeOfDay = "morning", name = "Luke" }
                    |> Expect.equal "Luke, good morning"
        ]


i18nLastSimple : Test
i18nLastSimple =
    describe "generated code with i18nArgLast flag"
        [ test "single text" <|
            \_ ->
                Inline.SimpleI18nLastTranslations.init Inline.SimpleI18nLastTranslations.En
                    |> Inline.SimpleI18nLastTranslations.singleText
                    |> Expect.equal "the text"
        , test "single interpolation" <|
            \_ ->
                Inline.SimpleI18nLastTranslations.init Inline.SimpleI18nLastTranslations.En
                    |> Inline.SimpleI18nLastTranslations.interpolation "world"
                    |> Expect.equal "Hello world!"
        , test "multi interpolation" <|
            \_ ->
                Inline.SimpleI18nLastTranslations.init Inline.SimpleI18nLastTranslations.En
                    |> Inline.SimpleI18nLastTranslations.greeting { timeOfDay = "evening", name = "Sir" }
                    |> Expect.equal "Good evening, Sir"
        ]


interpolationMatchCase : Test
interpolationMatchCase =
    describe "interpolation match case"
        [ test "interpolates the correct value for female gender" <|
            \_ ->
                Inline.InterpolationMatchTranslations.text
                    (Inline.InterpolationMatchTranslations.init Inline.InterpolationMatchTranslations.En)
                    "female"
                    |> Expect.equal "She bought a cat."
        , test "interpolates the correct value for male gender" <|
            \_ ->
                Inline.InterpolationMatchTranslations.text
                    (Inline.InterpolationMatchTranslations.init Inline.InterpolationMatchTranslations.En)
                    "male"
                    |> Expect.equal "He bought a cat."
        , test "interpolates the default value for other values" <|
            \_ ->
                Inline.InterpolationMatchTranslations.text
                    (Inline.InterpolationMatchTranslations.init Inline.InterpolationMatchTranslations.En)
                    "anything else"
                    |> Expect.equal "It bought a cat."
        ]


nestedInterpolation : Test
nestedInterpolation =
    describe "nested interpolation"
        [ test "interpolates the correct values for 'Ich'" <|
            \_ ->
                Inline.NestedInterpolationTranslations.text
                    (Inline.NestedInterpolationTranslations.init Inline.NestedInterpolationTranslations.De)
                    { pronoun = "Ich", objectsToBuy = "Gemüse" }
                    |> Expect.equal "Ich kaufe Gemüse."
        , test "interpolates the correct values for 'Du'" <|
            \_ ->
                Inline.NestedInterpolationTranslations.text
                    (Inline.NestedInterpolationTranslations.init Inline.NestedInterpolationTranslations.De)
                    { pronoun = "Du", objectsToBuy = "Obst" }
                    |> Expect.equal "Du kaufst Obst."
        , test "interpolates the default value for other values" <|
            \_ ->
                Inline.NestedInterpolationTranslations.text
                    (Inline.NestedInterpolationTranslations.init Inline.NestedInterpolationTranslations.De)
                    { pronoun = "Er", objectsToBuy = "Fleisch" }
                    |> Expect.equal "Er kauft Fleisch."
        ]


numberFormatCase : Test
numberFormatCase =
    describe "number format"
        [ test "type checks" <|
            \_ ->
                Inline.NumberFormatTranslations.text
                    (Inline.NumberFormatTranslations.init Util.emptyIntl Inline.NumberFormatTranslations.En)
                    52.34
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
                Inline.DateFormatTranslations.text
                    (Inline.DateFormatTranslations.init Util.emptyIntl Inline.DateFormatTranslations.En)
                    (Time.millisToPosix 1000)
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
                Inline.PluralTranslations.text
                    (Inline.PluralTranslations.init Util.emptyIntl Inline.PluralTranslations.En)
                    5
                    -- Due to the absent intl api, we can only test the default case here
                    |> Expect.equal "I met many people."
        ]


simpleHtml : Test
simpleHtml =
    describe "simple html"
        [ test "produces the correct html element and text content" <|
            \_ ->
                Inline.SimpleHtmlTranslations.html
                    (Inline.SimpleHtmlTranslations.init Inline.SimpleHtmlTranslations.En)
                    []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has [ Selector.text "Click me" ]
        , test "generates html attribute from translation file" <|
            \_ ->
                Inline.SimpleHtmlTranslations.html
                    (Inline.SimpleHtmlTranslations.init Inline.SimpleHtmlTranslations.En)
                    []
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has [ Selector.attribute <| Html.Attributes.href "/" ]
        , test "passes extra attributes given at runtime" <|
            \_ ->
                Inline.SimpleHtmlTranslations.html
                    (Inline.SimpleHtmlTranslations.init Inline.SimpleHtmlTranslations.En)
                    [ Html.Attributes.class "link" ]
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has [ Selector.class "link" ]
        ]
