import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Navigation


type alias Model =
    { location : Navigation.Location
    , threads : List Thread
    }

type Msg 
    = LocationChange Navigation.Location
    | ProcessThreads (Result Http.Error (List Thread))
    | GetThreads
    
    
type alias Thread =
    { id : String
    , commentCount : Int
    , text : String
    }
    

threadListDecoder =
    Decode.at ["data"]
        <| Decode.list threadDecoder


threadDecoder =
    Decode.map3 Thread
        (Decode.field "id" Decode.string)
        (Decode.field "comment_count" Decode.int)
        (Decode.field "text" Decode.string)


getThreads =
    let
        url = "https://api.threaditjs.com/threads"
    in
        Http.send ProcessThreads
            <| Http.get url threadListDecoder


renderThreads threads =
    List.map (\l -> li [] [ text l.text ]) threads


main = 
    Navigation.program LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
        
    
init : Navigation.Location -> (Model, Cmd Msg)
init location =
    ( Model location []
    , Cmd.none
    )
    
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LocationChange location ->
            ({ model | location = location }
            , Cmd.none
            )
            
        GetThreads ->
            (model, getThreads)
            
        ProcessThreads (Ok threads) ->
            ({ model | threads = threads}
            , Cmd.none
            )
            
        ProcessThreads (Err _) ->
            (model, Cmd.none)
            
            
view : Model -> Html Msg
view model =
    div [] 
        [ button [ type_ "button", onClick GetThreads ] [ text "Get Threads" ]
        , ul [] (renderThreads model.threads)
        ]