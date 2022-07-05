module Generators.InlineTest exposing (..)

import Expect
import Inline.MultiInterpolationTranslations
import Inline.MultiLanguageTextTranslations
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
