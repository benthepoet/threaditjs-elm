module Threads exposing (Thread, getComments, getList)

import Http
import Json.Decode as Decode


-- TYPES


type alias Thread =
    { id : String
    , text : String
    , commentCount : Int
    , parentId : Maybe String
    }
    
    
-- DECODERS
    
    
threadListDecoder =
    Decode.at ["data"]
        <| Decode.list threadDecoder


threadDecoder =
    Decode.map4 Thread
        (Decode.field "id" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "comment_count" Decode.int)
        (Decode.maybe (Decode.field "parent_id" Decode.string))
        
        
-- DATA FUNCTIONS


getComments id msg =
    let
        url = "http://api.threaditjs.com/comments/" ++ id
    in
        Http.send msg
            <| Http.get url threadListDecoder


getList msg =
    let
        url = "http://api.threaditjs.com/threads"
    in
        Http.send msg
            <| Http.get url threadListDecoder
    