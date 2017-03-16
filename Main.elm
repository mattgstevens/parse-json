module Main exposing (main)

import Html exposing (text)

import Json.Encode
import Json.Decode exposing (Decoder, Value, at, decodeString, decodeValue, field, nullable, oneOf, int, string, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, resolve, optional)

type alias Person =
  { firstName : String
  , lastName : String
  , age : Maybe Int }

nullPerson : Person
nullPerson =
  { firstName = "Max"
  , lastName = "Mustermann"
  , age = Nothing }

-- first attempt

versionDecoder : Decoder Int
versionDecoder = (field "version" int)

firstNameDecoder : Decoder String
firstNameDecoder = oneOf [(field "firstName" string), (at ["name", "first"] string)]

decodePerson : String -> Person
decodePerson jsonData =
  case (decodeString personDecoder_v1 jsonData) of
     Ok value -> value
     Err msg -> nullPerson

-- versioned by hand

finalDecodePerson : Result String Person -> Person
finalDecodePerson decodeResult =
  case decodeResult of
    Ok value -> value
    Err msg -> nullPerson

decodeVersionedPerson : String -> Person
decodeVersionedPerson jsonData =
  case (decodeString versionDecoder jsonData) of
    Ok value -> case value of
                  1 -> finalDecodePerson (decodeString personDecoder_v1 jsonData)
                  2 -> finalDecodePerson (decodeString personDecoder_v2 jsonData)
                  _ -> nullPerson
    Err msg -> nullPerson

personDecoder_v1 : Decoder Person
personDecoder_v1 =
  decode Person
    |> required "firstName" string
    |> required "lastName" string
    |> optional "age" (nullable int) Nothing

personDecoder_v2 : Decoder Person
personDecoder_v2 =
  decode Person
      |> requiredAt ["name", "first"] string
      |> requiredAt ["name", "last"] string
      |> optional "age" (nullable int) Nothing

-- this could be the most compact solution.. needs a bit more work but my hack time is up today
-- http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/3.0.0/Json-Decode-Pipeline#resolve
personDecoderPipeline : Decoder Person
personDecoderPipeline =
  let
    toDecoder : Int -> String -> String -> Maybe Int -> Decoder Person
    toDecoder version firstName lastName age =
      if version > 2 then
        -- could sent an event here to trace a bad version
        fail "Unknown version"
      else
        succeed (Person firstName lastName age)
  in
    decode toDecoder
      |> required "version" int
      |> requiredAt ["name", "first"] string
      |> requiredAt ["name", "last"] string
      |> optional "age" (nullable int) Nothing
      |> resolve

-- main

data1 = """{ "version": 1, "firstName": "Sam", "lastName": "Sample" }"""
data2 = """{ "version": 1, "firstName": "Bob", "lastName": "Bombadillo", "age": null }"""
data3 = """{ "version": 2, "name": { "first": "Tim", "last": "Testa"} }"""
data4 = """{ "version": 3, "name": { "first": "Tim", "last": "Testa"} }"""

main = Html.text (String.join " --- " [ (toString (decodeVersionedPerson data1))
                                      , (toString (decodeVersionedPerson data2))
                                      , (toString (decodeVersionedPerson data3))
                                      , (toString (decodeString personDecoderPipeline data3))
                                      , (toString (decodeString personDecoderPipeline data4))])
