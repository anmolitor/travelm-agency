module Generators.DynamicTest exposing (..)

import Dict
import Dynamic.InterpolationMatchServer
import Dynamic.InterpolationMatchTranslations
import Dynamic.MultiInterpolationServer
import Dynamic.MultiInterpolationTranslations
import Dynamic.MultiLanguageTextServer
import Dynamic.MultiLanguageTextTranslations
import Dynamic.SimpleI18nLastServer
import Dynamic.SimpleI18nLastTranslations
import Dynamic.SingleInterpolationServer
import Dynamic.SingleInterpolationTranslations
import Dynamic.SingleTextServer
import Dynamic.SingleTextTranslations
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
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
