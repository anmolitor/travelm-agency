module Types.ErrorTest exposing (..)

import Expect
import Parser
import Test exposing (..)
import Types.Error


suite : Test
suite =
    describe "Error"
        [ test "format recursive issue" <|
            \_ ->
                Types.Error.cyclicFallback [ "test", "other", "bla" ]
                    |> Types.Error.formatFail
                    |> Expect.equal (Err """Detected mutually recursive fallbacks. This is not allowed, because it can lead to indefinite recursion at runtime or unresolved keys.
\tTrace: test --> other --> bla""")
        , test "format inconsistent keys" <|
            \_ ->
                Types.Error.inconsistentKeys { hasKeys = "en", missesKeys = "de", keys = [ "someKey", "anotherKey" ] }
                    |> Types.Error.formatFail
                    |> Expect.equal (Err """Found inconsistent keys in translation files.
\tLanguage 'en' includes the keys [someKey, anotherKey but language 'de' does not.
\tEither delete the keys in 'en', add the keys in 'de', or opt into a fallback behaviour by adding '# fallback-language: en' into the 'de' translation file.""")
        , test "format recursive term reference" <|
            \_ ->
                Types.Error.cyclicTermReference [ "termA", "termB" ]
                    |> Types.Error.formatFail
                    |> Expect.equal (Err """Detected mutually recursive term references in translation file.
\tTrace: termA --> termB""")
        , test "format 'cannot format string as number' with context" <|
            \_ ->
                Types.Error.failedToFormatStringAsNumber "NaN"
                    |> Types.Error.addLanguageCtx "en-US"
                    |> Types.Error.addAdditionalCtx "When doing stuff"
                    |> Types.Error.formatFail
                    |> Expect.equal (Err """Cannot format the given string 'NaN' as a number.
\tContext:
\t\t- info: When doing stuff
\t\t- language: en-US""")
        , test "format 'cannot parse string as number' with context" <|
            \_ ->
                Types.Error.failedToParseStringAsNumber "BaNaNa"
                    |> Types.Error.addContentTypeCtx ".ftl"
                    |> Types.Error.addAdditionalCtx "Why would you"
                    |> Types.Error.formatFail
                    |> Expect.equal (Err """Cannot parse the given string 'BaNaNa' as a number.
\tContext:
\t\t- fileExtension: .ftl
\t\t- info: Why would you""")
        , test "format multiple unresolvable term references with different contexts" <|
            \_ ->
                [ Types.Error.unresolvableTermReference "unknown" |> Types.Error.addLanguageCtx "en"
                , Types.Error.unresolvableTermReference "not-found" |> Types.Error.addLanguageCtx "de"
                ]
                    |> Types.Error.combineList
                    |> Types.Error.addContentTypeCtx ".ftl"
                    |> Types.Error.formatFail
                    |> Expect.equal (Err """Failed to resolve reference to unknown term 'unknown' in translation file.
\tContext:
\t\t- fileExtension: .ftl
\t\t- language: en

Failed to resolve reference to unknown term 'not-found' in translation file.
\tContext:
\t\t- fileExtension: .ftl
\t\t- language: de""")
        , test "format runParser error" <|
            \_ ->
                Types.Error.runParser (Parser.problem "Expected x but was y.") "bla"
                    |> Types.Error.formatFail
                    |> Expect.equal (Err """Failed to parse translation file: Problem: Expected x but was y. at row:1 col:1""")
        ]
