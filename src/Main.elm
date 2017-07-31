import Dict exposing (Dict)
import Html exposing (Html, a, div, form, h3, hr, input, p, text, textarea)
import Html.Attributes exposing (class, href, name, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode as Json
import Navigation
import UrlParser exposing (map, oneOf, parsePath, s, string, (</>))

import Threads


-- TYPES

type alias Model =
    { page : Page
    , post : String
    , replies : Dict String String
    , threads : List Threads.Thread
    , comments : List Threads.Thread
    }


type Msg 
    = ProcessThreads (Result Http.Error (List Threads.Thread))
    | ProcessComments (Result Http.Error (List Threads.Thread))
    | ProcessThread (Result Http.Error Threads.Thread)
    | PageChange (Maybe Page)
    | UpdateReply String String
    | UpdatePost String
    | CreatePost

    
type Page 
    = ThreadList
    | CommentList String
    | NotFound


-- HELPER FUNCTIONS


changeCmd page =
    case page of
        ThreadList ->
            Threads.getList ProcessThreads
            
        CommentList id ->
            Threads.getComments id ProcessComments
            
        NotFound ->
            Cmd.none


locationChange location = 
    parsePath route location
        |> PageChange


onFormInput tagger =
    on "input" <|
        Json.at [ "target" ] <|
            Json.map2 tagger
                (Json.field "name" Json.string)
                (Json.field "value" Json.string)


viewComment replies thread =
    div [ class "comment" ] 
        [ p [] [ text thread.text ] 
        , div [ class "reply" ] 
            [ case Dict.get thread.id replies of
                Nothing ->
                    a [ onClick (UpdateReply thread.id "") ] [ text "Reply" ]
                Just reply ->
                    form [] 
                        [ textarea [ name thread.id ] []
                        , input [ type_ "submit", value "Reply" ] []
                        , div [ class "preview" ] [ text reply ]
                        ]
                
            ]
        , div [ class "children" ] (List.map (viewComment replies) (
            case thread.children of
                Threads.Comments (list) ->
                    list
        ))
        ]


renderThread thread =
    p []
        [ a [ href ("/comments/" ++ thread.id) ] [ text thread.text ]
        , p [ class "comment_count" ] [ text ((toString thread.commentCount) ++ " comment(s)") ]
        , hr [] [] 
        ]


renderThreads threads =
    let 
        threadList = List.map renderThread threads
    in
        threadList ++ 
            [ form [ onSubmit CreatePost ] 
                [ textarea [ onInput UpdatePost ] []
                , input [ type_ "submit", value "Post" ] []
                ]
            ]

route =
    oneOf
        [ map ThreadList (s "threads")
        , map CommentList (s "comments" </> string)
        ]


updatePage maybePage model =
    case maybePage of
        Nothing ->
            (model, Navigation.newUrl "/threads")
        Just page ->
            ({ model | page = page }, changeCmd page)


-- MAIN PROGRAM


main = 
    Navigation.program locationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
        
    
init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        model = Model NotFound "" Dict.empty [] []
    in
        updatePage (parsePath route location) model
    
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PageChange maybePage ->
            updatePage maybePage model
        
        ProcessComments (Ok comments) ->
            ({ model | comments = Threads.transform comments}
            , Cmd.none
            )
            
        ProcessComments (Err _) ->
            (model, Cmd.none)
            
        ProcessThreads (Ok threads) ->
            ({ model | threads = threads}
            , Cmd.none
            )
            
        ProcessThreads (Err _) ->
            (model, Cmd.none)
            
        ProcessThread (Ok thread) ->
            ({ model 
                | threads = model.threads ++ [thread] 
                , post = "" }, Cmd.none)
            
        ProcessThread (Err _) ->
            (model, Cmd.none)
            
        UpdateReply id text ->
            ({ model | replies = Dict.insert id text model.replies }, Cmd.none)
            
        UpdatePost post ->
            ({ model | post = post }, Cmd.none)
            
        CreatePost ->
            (model, Threads.createThread model.post ProcessThread)
            
            
view : Model -> Html Msg
view model =
    case model.page of
        ThreadList ->
            div [ class "thread-list" ] (renderThreads model.threads)
        
        CommentList id ->
            div [ class "comments" ] 
                [ form [ onFormInput UpdateReply ] 
                    (List.map (viewComment model.replies) model.comments)
                ]
            
        NotFound ->
            div [] []
                