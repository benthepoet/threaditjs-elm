module Threads exposing (Thread, createComment, createThread, getComments, getList, transform, trim)

import Dict
import Set
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Regex


baseUrl : String
baseUrl = "http://api.threaditjs.com"


trimLength : Int
trimLength = 120


-- TYPES


type alias Thread =
    { id : String
    , text : String
    , commentCount : Int
    , parentId : Maybe String
    }


-- DECODERS


threadDecoder =
    Decode.map4 Thread
        (Decode.field "id" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "comment_count" Decode.int)
        (Decode.maybe (Decode.field "parent_id" Decode.string))


-- ENCODERS


commentEncoder text parent =
    Encode.object
        [ ( "text", Encode.string text )
        , ( "parent", Encode.string parent )
        ]


threadEncoder text =
    Encode.object
        [ ( "text", Encode.string text )
        ]



-- DATA FUNCTIONS


createComment text parent msg =
    let
        url =
            baseUrl ++ "/comments/create"
    in
        Http.send msg <|
            Http.post url (commentEncoder text parent |> Http.jsonBody) (Decode.at [ "data" ] threadDecoder)


createThread text msg =
    let
        url =
            baseUrl ++ "/threads/create"
    in
        Http.send msg <|
            Http.post url (threadEncoder text |> Http.jsonBody) (Decode.at [ "data" ] threadDecoder)


getComments id msg =
    let
        url =
            baseUrl ++ "/comments/" ++ id
    in
        Http.send msg <|
            Http.get url (Decode.at [ "data" ] <| Decode.list threadDecoder)


getList msg =
    let
        url =
            baseUrl ++ "/threads"
    in
        Http.send msg <|
            Http.get url (Decode.at [ "data" ] <| Decode.list threadDecoder)


trim text = 
    String.left trimLength
        <| Regex.replace Regex.All (Regex.regex "<(p|a|td|code|pre|table|ol|li)>") (\_ -> "")
        <| Regex.replace Regex.All (Regex.regex "<\\/(p|a|td|code|pre|table|ol|li)>") (\_ -> "") text


transform threads =
    ( getRoot threads, getLookup threads )


getLookup threads =
    let
        keys =
            getKeys threads
    in
        Dict.fromList <|
            List.map (getChildren threads) (Set.toList keys)


getChildren threads key =
    ( key, List.filter (\l -> Maybe.withDefault "" l.parentId == key) threads )


getKeys threads =
    Set.fromList <|
        List.map (\l -> Maybe.withDefault "" l.parentId) threads


getRoot threads =
    List.filter (\l -> l.parentId == Nothing) threads
        |> List.head
