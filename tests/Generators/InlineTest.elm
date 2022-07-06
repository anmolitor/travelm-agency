module Generators.InlineTest exposing (..)

import Expect
import Inline.InterpolationMatchTranslations
import Inline.MultiInterpolationTranslations
import Inline.MultiLanguageTextTranslations
import Inline.SimpleI18nLastTranslations
import Inline.SingleInterpolationTranslations
import Inline.SingleTextTranslations
import Test exposing (Test, describe, test)


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
