module String.Conversions exposing
    ( fromBool
    , fromWeekday
    , fromDict
    , fromHttpError
    , fromHttpResponse
    , fromList
    , fromMaybe
    , fromMonth
    , fromRecord
    , fromSet
    , fromString
    , fromTuple2
    , fromTuple3
    , fromValue
    , withUnionConstructor
    )

{-| Helpers to convert common types into a `String`.

@docs fromBool
@docs fromWeekday
@docs fromDict
@docs fromHttpError
@docs fromHttpResponse
@docs fromList
@docs fromMaybe
@docs fromMonth
@docs fromRecord
@docs fromSet
@docs fromString
@docs fromTuple2
@docs fromTuple3
@docs fromValue
@docs withUnionConstructor

-}

import Dict exposing (Dict)
import Http
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)
import Time


{-| Nest some arguments under a tag, including parentheses when needed. Helpful for printing union type values.

    withUnionConstructor "Ok" [ String.fromInt 1 ]
    --> "Ok 1"

-}
withUnionConstructor : String -> List String -> String
withUnionConstructor tag args =
    let
        needsParens a =
            String.contains " " a
                && not (String.startsWith "{" a)
                && not (String.startsWith "(" a)
                && not (String.startsWith "[" a)
                && not (String.startsWith "\"" a)
                && not (String.startsWith "'" a)

        argsString =
            args
                |> List.map
                    (\arg ->
                        if needsParens arg then
                            "(" ++ arg ++ ")"

                        else
                            arg
                    )
                |> String.join " "
    in
    tag ++ " " ++ argsString


{-| Build a record string for debugging and logging with pairs of
keys and string conversion functions, meant to be paired with accessors

    fromRecord [ ("hello", .hello >> String.fromInt ) ] { hello = 1 }
    --> "{ hello = 1 }"

-}
fromRecord : List ( String, a -> String ) -> a -> String
fromRecord fields record =
    let
        inner =
            fields
                |> List.map
                    (\( key, fieldToString ) ->
                        key ++ " = " ++ fieldToString record
                    )
                |> String.join ", "
    in
    "{ " ++ inner ++ " }"


{-| Convert a 2-tuple to a string using nested conversions.

    fromTuple2 String.fromInt String.fromFloat ( 1, 1.5 )
    --> "(1,1.5)"

-}
fromTuple2 : (a -> String) -> (b -> String) -> ( a, b ) -> String
fromTuple2 leftToString rightToString ( a, b ) =
    "(" ++ leftToString a ++ "," ++ rightToString b ++ ")"


{-| Convert a 3-tuple to a string using nested conversions.

    fromTuple3 String.fromInt String.fromFloat String.fromInt ( 1, 1.5, 2 )
    --> "(1,1.5,2)"

-}
fromTuple3 : (a -> String) -> (b -> String) -> (c -> String) -> ( a, b, c ) -> String
fromTuple3 firstToString secondToString thirdToString ( a, b, c ) =
    "(" ++ firstToString a ++ "," ++ secondToString b ++ "," ++ thirdToString c ++ ")"


{-| Convert a List to a string using a nested conversion.

    fromList String.fromInt [1, 2, 3]
    --> "[1,2,3]"

-}
fromList : (a -> String) -> List a -> String
fromList innerToString list =
    let
        contents =
            list
                |> List.map innerToString
                |> String.join ","
    in
    "[" ++ contents ++ "]"


{-| Convert a Dict to a String using nested conversions.

    import Dict

    fromDict String.fromInt String.fromFloat (Dict.fromList [(1, 1.5)])
    --> "Dict.fromList [(1,1.5)]"

-}
fromDict : (comparable -> String) -> (v -> String) -> Dict comparable v -> String
fromDict keyToString valueToString dict =
    let
        contents =
            dict
                |> Dict.toList
                |> fromList (fromTuple2 keyToString valueToString)
    in
    "Dict.fromList " ++ contents


{-| Convert a Set to a string using a nested conversion.

    import Set

    fromSet String.fromInt (Set.fromList [1, 2])
    --> "Set.fromList [1,2]"

-}
fromSet : (comparable -> String) -> Set comparable -> String
fromSet innerToString set =
    let
        contents =
            set
                |> Set.toList
                |> fromList innerToString
    in
    "Set.fromList " ++ contents


{-| Convert a String to a debugging version of that String.

    fromString "hello \"world\""
    --> "\"hello \\\"world\\\"\""

-}
fromString : String -> String
fromString string =
    Encode.encode 0 (Encode.string string)


{-| Convert an Http.Error to a String.
-}
fromHttpError : Http.Error -> String
fromHttpError error =
    case error of
        Http.BadUrl url ->
            withUnionConstructor "BadUrl" [ fromString url ]

        Http.Timeout ->
            withUnionConstructor "Timeout" []

        Http.NetworkError ->
            withUnionConstructor "NetworkError" []

        Http.BadStatus statusCode ->
            withUnionConstructor "BadStatus" [ String.fromInt statusCode ]

        Http.BadBody body ->
            withUnionConstructor "BadBody" [ fromString body ]


{-| Convert an Http.Response String to a String.
-}
fromHttpResponse : Http.Response String -> String
fromHttpResponse response =
    case response of
        Http.BadUrl_ url ->
            withUnionConstructor "BadUrl_" [ fromString url ]

        Http.Timeout_ ->
            withUnionConstructor "Timeout_" []

        Http.NetworkError_ ->
            withUnionConstructor "NetworkError_" []

        Http.BadStatus_ metadata body ->
            withUnionConstructor "BadStatus_" [ fromMetadata metadata, fromString body ]

        Http.GoodStatus_ metadata body ->
            withUnionConstructor "GoodStatus_" [ fromMetadata metadata, fromString body ]


fromMetadata : Http.Metadata -> String
fromMetadata =
    fromRecord
        [ ( "url", .url >> fromString )
        , ( "statusCode", .statusCode >> String.fromInt )
        , ( "statusText", .statusText >> fromString )
        , ( "headers", .headers >> fromDict fromString fromString )
        ]


{-| Convert a Time.Month to a String matching its constructor.
-}
fromMonth : Time.Month -> String
fromMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


{-| Convert a Time.Weekday to a String matching its constructor.
-}
fromWeekday : Time.Weekday -> String
fromWeekday day =
    case day of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


{-| Convert a Maybe to a String using a nested conversion.

    fromMaybe String.fromInt (Just 1)
    --> "Just 1"

-}
fromMaybe : (a -> String) -> Maybe a -> String
fromMaybe innerToString maybe =
    case maybe of
        Just a ->
            withUnionConstructor "Just" [ innerToString a ]

        Nothing ->
            "Nothing"


{-| Convert a Bool to a String matching its constructor.
-}
fromBool : Bool -> String
fromBool bool =
    if bool then
        "True"

    else
        "False"


{-| Convert a Json.Decode.Value to a JSON String.
-}
fromValue : Value -> String
fromValue value =
    Encode.encode 0 value
