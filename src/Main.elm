module Main exposing (main)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Time
import Random
import Url exposing (Url)
import Route exposing (Route)
import Pages.Dashboard
import Flags exposing (Flags)


-- init
-- type alias Model = 
--   { secondsElapsed : Int
--   , isRunning : Bool
--   , roll : Maybe Int
--   }

type alias Model =
    { key : Key
    , url : Url
    , page : PageModel
    }

type PageModel 
    = Dashboard Pages.Dashboard.Model
    | NotFound


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key = 
    case Route.fromUrl url of
        Route.Dashboard ->
            let
                ( dashboardModel, dashboardCmd ) =
                    Pages.Dashboard.init flags
            in
            ( { url = url, key = key, page = Dashboard dashboardModel } 
            , Cmd.map DashboardMsg dashboardCmd
            )

        Route.NotFound ->
            ( { url = url, key = key, page = NotFound } 
            , Cmd.none
            )


-- update
type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | DashboardMsg Pages.Dashboard.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlRequested request, _ ) ->
            -- Debug.todo "URL REQUESTED"
            ( model, Cmd.none )

        ( UrlChanged url, _ ) ->
            -- Debug.todo "URL CHANGED"
            ( model, Cmd.none )

        -- If we get a dashboard message, and we are on the dashboard page: update
        ( DashboardMsg pageMsg, Dashboard pageModel ) ->
            let
                ( dashboardModel, dashboardCmd ) =
                    Pages.Dashboard.update pageMsg pageModel
            in
            ( { model | page = Dashboard dashboardModel }
            , Cmd.map DashboardMsg dashboardCmd
            )

        _ ->
            ( model, Cmd.none )


-- view
view : Model -> Browser.Document Msg
view model =
    case model.page of
        Dashboard dashboardModel ->
            documentMap DashboardMsg 
                (Pages.Dashboard.view dashboardModel)

        NotFound ->
            { title = "404", body = [ h1 [] [ text "No such thing" ] ] }

documentMap : 
        (pageMsg -> Msg)
        -> Browser.Document pageMsg
        -> Browser.Document Msg

documentMap toMsg doc =
    { title = doc.title
    , body = List.map (Html.map toMsg) doc.body
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Dashboard dashboardModel ->
            Sub.map DashboardMsg (Pages.Dashboard.subscriptions dashboardModel)

        NotFound ->
            Sub.none


main =
  Browser.application
      { init = init
      , update = update
      , view = view 
      , subscriptions = subscriptions
      , onUrlChange = UrlChanged
      , onUrlRequest = UrlRequested
      }

