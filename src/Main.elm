module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, a, div, form, hr, input, p, text, textarea)
import Html.Attributes exposing (class, href, name, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Navigation
import UrlParser exposing (map, oneOf, parsePath, s, string, (</>))
import Threads


-- TYPES


type alias Model =
    { page : Page
    , error : String
    , post : String
    , replies : Dict String String
    , threads : List Threads.Thread
    , comments : ( Maybe Threads.Thread, Dict.Dict String (List Threads.Thread) )
    }


type Msg
    = SetThreads (Result Http.Error (List Threads.Thread))
    | SetThread (Result Http.Error Threads.Thread)
    | SetComments (Result Http.Error (List Threads.Thread))
    | SetComment (Result Http.Error Threads.Thread)
    | PageChange (Maybe Page)
    | SetReply String String
    | SetPost String
    | CreateThread
    | CreateComment String String


type Page
    = ThreadList
    | CommentList String
    | NotFound



-- HELPER FUNCTIONS


changePage page =
    case page of
        ThreadList ->
            Threads.getList SetThreads

        CommentList id ->
            Threads.getComments id SetComments

        NotFound ->
            Cmd.none


locationChange location =
    parsePath route location
        |> PageChange


onFormInput tagger =
    on "input" <|
        Decode.at [ "target" ] <|
            Decode.map2 tagger
                (Decode.field "name" Decode.string)
                (Decode.field "value" Decode.string)


viewComment lookup replies thread =
    div [ class "comment" ]
        [ p [] [ text thread.text ]
        , div [ class "reply" ]
            [ case Dict.get thread.id replies of
                Nothing ->
                    a [ onClick (SetReply thread.id "") ] [ text "Reply" ]

                Just reply ->
                    form [ onSubmit (CreateComment reply thread.id) ]
                        [ textarea [ name thread.id ] []
                        , input [ type_ "submit", value "Reply" ] []
                        , div [ class "preview" ] [ text reply ]
                        ]
            ]
        , div [ class "children" ]
            (case Dict.get thread.id lookup of
                Just children ->
                    List.map (viewComment lookup replies) children

                Nothing ->
                    []
            )
        ]


viewThread thread =
    p []
        [ a [ href ("/comments/" ++ thread.id) ] [ text (Threads.trim thread.text) ]
        , p [ class "comment_count" ] [ text ((toString thread.commentCount) ++ " comment(s)") ]
        , hr [] []
        ]


viewThreads threads =
    let
        threadList =
            List.map viewThread threads
    in
        if List.isEmpty threadList then
            []
        else
            threadList
                ++ [ form [ onSubmit CreateThread ]
                        [ textarea [ onInput SetPost ] []
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
            ( model, Navigation.newUrl "/threads" )

        Just page ->
            ( { model | page = page }, changePage page )



-- MAIN PROGRAM


main =
    Navigation.program locationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            Model NotFound "" "" Dict.empty [] ( Nothing, Dict.empty )
    in
        updatePage (parsePath route location) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageChange maybePage ->
            updatePage maybePage model

        SetThreads (Ok threads) ->
            ( { model 
                | threads = threads
                , error = "" }
            , Cmd.none
            )

        SetThreads (Err _) ->
            ( { model | error = "An error occurred. Please refresh the page." }, Cmd.none )

        SetThread (Ok thread) ->
            ( { model
                | threads = model.threads ++ [ thread ]
                , post = ""
              }
            , Cmd.none
            )

        SetThread (Err _) ->
            ( model, Cmd.none )

        SetComments (Ok comments) ->
            ( { model 
                | comments = Threads.transform comments
                , error = "" }
            , Cmd.none
            )

        SetComments (Err _) ->
            ( { model | error = "An error occurred. Please refresh the page." }, Cmd.none )

        SetComment (Ok thread) ->
            case thread.parentId of
                Just parentId ->
                    let
                        ( root, lookup ) =
                            model.comments

                        siblings =
                            Maybe.withDefault [] <| Dict.get parentId lookup
                    in
                        ( { model
                            | comments = ( root, Dict.insert parentId (siblings ++ [ thread ]) lookup )
                            , replies = Dict.remove parentId model.replies
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        SetComment (Err _) ->
            ( model, Cmd.none )

        SetReply id text ->
            ( { model | replies = Dict.insert id text model.replies }, Cmd.none )

        SetPost post ->
            ( { model | post = post }, Cmd.none )

        CreateComment text parent ->
            ( model, Threads.createComment text parent SetComment )

        CreateThread ->
            ( model, Threads.createThread model.post SetThread )


view : Model -> Html Msg
view model =
    if String.isEmpty model.error then
        case model.page of
            ThreadList ->
                div [ class "thread-list" ] (viewThreads model.threads)

            CommentList id ->
                let
                    ( root, lookup ) =
                        model.comments
                in
                    div [ class "comments" ]
                        (case root of
                            Just thread ->
                                [ form [ onFormInput SetReply ] [ viewComment lookup model.replies thread ]
                                ]

                            Nothing ->
                                []
                        )

            NotFound ->
                div [] []
    else
        div [] [ text model.error ]
