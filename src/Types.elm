module Types exposing (I18nPairs, TKey(..), TSegment(..), TValue, Translations)

import List.NonEmpty exposing (NonEmpty)
import Placeholder.Internal exposing (Template)


type alias I18nPairs =
    List ( String, Template )



-- Internal representation all formats get converted to


type alias Translations =
    List ( TKey, TValue )


type alias TValue =
    NonEmpty TSegment


type TKey
    = ExposedKey String
    | HiddenKey String


type TSegment
    = End
      -- A simple text
    | Text String
      -- {$var}
    | Interpolation String
      -- {$var -> case var of [List String TValue]} [TValue]
    | InterpolationCase String (NonEmpty ( TPattern, TValue ))
      -- {-other-key}
    | Reference String


type TPattern
    = StringPattern String
    | RangePattern Int Int
