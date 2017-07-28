module Threads exposing (Thread, Comments(Comments), getComments, getList, transform)

import Dict
import Set
import Http
import Json.Decode as Decode


-- TYPES


type alias Thread =
    { id : String
    , text : String
    , commentCount : Int
    , parentId : Maybe String
    , children : Comments
    }
    
    
type Comments = Comments (List Thread)
    
-- DECODERS
    

threadDecoder =
    Decode.at ["data"]
        <| Decode.list
            <| Decode.map5 Thread
                (Decode.field "id" Decode.string)
                (Decode.field "text" Decode.string)
                (Decode.field "comment_count" Decode.int)
                (Decode.maybe (Decode.field "parent_id" Decode.string))
                (Decode.succeed (Comments []))
        
        
-- DATA FUNCTIONS


getComments id msg =
    let
        url = "http://api.threaditjs.com/comments/" ++ id
    in
        Http.send msg
            <| Http.get url threadDecoder


getList msg =
    let
        url = "http://api.threaditjs.com/threads"
    in
        Http.send msg
            <| Http.get url threadDecoder
    
    
node thread lookup =
    let 
        children = Maybe.withDefault [] (Dict.get thread.id lookup)
    in
        { thread | children = Comments (List.map (\l -> node l lookup) children) }
    
transform threads =
    let
        root = List.take 1 threads
        lookup = getLookupDict threads
    in
        List.map (\l -> node l lookup) root
    

getLookupDict threads =
    let
        keys = getParentKeys threads
    in
        Dict.fromList
            <| List.map (\l -> (l, List.filter (\k -> (Maybe.withDefault "" k.parentId) == l) threads)) (Set.toList keys)
    
    
getParentKeys threads =
    Set.fromList
        <| List.map (\l -> Maybe.withDefault "" l.parentId) threads