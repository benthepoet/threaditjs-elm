import Html exposing (Html, a, button, div, h3, hr, p, text)
import Html.Attributes exposing (class, href, type_)
import Http
import Navigation
import UrlParser exposing (map, oneOf, parsePath, s, string, (</>))

import Threads exposing (Thread, getComments, getList)


-- TYPES

type alias Model =
    { page : Page
    , threads : List Thread
    , comments : List Thread
    }


type Msg 
    = ProcessThreads (Result Http.Error (List Thread))
    | ProcessComments (Result Http.Error (List Thread))
    | PageChange (Maybe Page)

    
type Page 
    = ThreadList
    | CommentList String
    | NotFound


-- HELPER FUNCTIONS


changeCmd page =
    case page of
        ThreadList ->
            getList ProcessThreads
            
        CommentList id ->
            getComments id ProcessComments
            
        NotFound ->
            Cmd.none


getChildren id comments =
    List.filter (\l -> 
        case l.parentId of
            Nothing -> 
                False
            Just parentId ->
                parentId == id) comments


locationChange location = 
    parsePath route location
        |> PageChange


renderComment lookup =
    (\l -> 
        div [ class "comment" ] 
            [ p [] [ text l.text ] 
            , div [ class "children" ] (renderComments (getChildren l.id lookup) lookup) 
            ]
    )


renderComments comments lookup =
    List.map (renderComment lookup) comments


renderThread thread =
    p []
        [ a [ href ("/comments/" ++ thread.id) ] [ text thread.text ]
        , p [ class "comment_count" ] [ text ((toString thread.commentCount) ++ " comment(s)") ]
        , hr [] [] 
        ]


renderThreads threads =
    List.map renderThread threads


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
        model = Model NotFound [] []
    in
        updatePage (parsePath route location) model
    
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PageChange maybePage ->
            updatePage maybePage model
        
        ProcessComments (Ok comments) ->
            ({ model | comments = comments}
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
            
            
view : Model -> Html Msg
view model =
    case model.page of
        ThreadList ->
            div [ class "thread-list" ] (renderThreads model.threads)
        
        CommentList id ->
            div [ class "comments" ] (renderComments (List.take 1 model.comments) model.comments)
            
        NotFound ->
            div [] []
                