module Generators.DynamicTest exposing (..)

import Dict
import Dynamic.MultiLanguageTextServer
import Dynamic.MultiLanguageTextTranslations
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
