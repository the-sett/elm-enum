module Enum exposing (Enum, decoder, encoder, find, make, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Enum a
    = Enum (List a) (a -> String)


make : List a -> (a -> String) -> Enum a
make vals toString =
    Enum vals toString


toString : Enum a -> a -> String
toString (Enum _ toString) val =
    toString val


find : Enum a -> String -> Maybe a
find (Enum vals toString) val =
    vals
        |> List.filter ((==) val << toString)
        |> List.head


decoder : Enum a -> Decoder a
decoder enum =
    Decode.string
        |> Decode.andThen
            (\val ->
                case find enum val of
                    Just value ->
                        Decode.succeed value

                    Nothing ->
                        Decode.fail <| "Could not decode value to enum: " ++ val
            )


encoder : Enum a -> a -> Value
encoder enum val =
    toString enum val
        |> Encode.string
