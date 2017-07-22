import Html exposing (Html, div, text)
import Navigation


type alias Model =
    { location: Navigation.Location
    }

type Msg 
    = LocationChange Navigation.Location
    

main = 
    Navigation.program LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
        
    
init : Navigation.Location -> (Model, Cmd Msg)
init location =
    ( Model location
    , Cmd.none
    )
    
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LocationChange location ->
            ({ model | location = location }
            , Cmd.none
            )
            
            
view : Model -> Html Msg
view model =
    div [] [ text model.location.hash ]