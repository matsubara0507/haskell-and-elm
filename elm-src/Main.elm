module Main exposing (main)

import Browser as Browser
import Generated.API as API exposing (Todo)
import Html as Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, href, placeholder, style, type_)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import RemoteData exposing (RemoteData(..))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { todos : RemoteData String (List API.Todo)
    , title : String
    }


type Msg
    = FetchTodos (Result Http.Error (List API.Todo))
    | Reload
    | ChangeTodo Todo
    | Title String
    | Push Button


type Button
    = Post String
    | Delete Int


initModel : Model
initModel =
    { todos = NotAsked, title = "" }


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, fetchTodos )


view : Model -> Html Msg
view model =
    div [ class "my-3 mx-auto col-10" ]
        [ h1 [] [ text "ToDo List !!", loginButton ]
        , viewToDos model
        , viewPost model
        ]


loginButton : Html msg
loginButton =
    button [ class "btn btn-sm btn-outline float-right mt-2", type_ "button" ]
        [ a
            [ href "/login"
            , style "color" "inherit"
            , style "text-decoration-line" "none"
            ]
            [ text "Login by GitHub" ]
        ]


viewToDos : Model -> Html Msg
viewToDos model =
    case model.todos of
        NotAsked ->
            text "Please Push Button."

        Loading ->
            text "Loading..."

        Failure err ->
            text ("Error: " ++ err)

        Success todos ->
            div
                [ class "border-bottom" ]
                [ ul [] (List.map viewTodo todos) ]


viewTodo : API.Todo -> Html Msg
viewTodo todo =
    li
        [ class "Box-row" ]
        [ label
            [ class "float-left py-2 pl-3" ]
            [ input
                [ type_ "checkbox"
                , checked todo.done
                , onCheck (\b -> ChangeTodo { todo | done = b })
                ]
                []
            ]
        , div
            [ class "float-left col-10 p-2 lh-condensed" ]
            [ div [ class "h4" ] [ text todo.title ] ]
        , button
            [ class "btn-link", onClick <| Push (Delete todo.id) ]
            [ i
                [ class "fas fa-trash"
                , attribute "aria-hidden" "true"
                , style "color" "#cb2431"
                ]
                []
            ]
        ]


viewPost : Model -> Html Msg
viewPost model =
    div []
        [ input
            [ class "form-control m-3"
            , type_ "text"
            , placeholder "Todo Title"
            , onInput Title
            ]
            []
        , span []
            [ button
                [ class "btn"
                , onClick <| Push (Post model.title)
                ]
                [ text "Add Todo" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTodos (Ok todos) ->
            ( { model | todos = Success todos }, Cmd.none )

        FetchTodos (Err _) ->
            ( { model | todos = Failure "Something went wrong.." }, Cmd.none )

        ChangeTodo todo ->
            ( model, changeTodo todo )

        Title title ->
            ( { model | title = title }, Cmd.none )

        Push (Post title) ->
            ( model, addTodo (Todo 0 title False) )

        Push (Delete todoId) ->
            ( model, removeTodo todoId )

        Reload ->
            ( model, fetchTodos )


fetchTodos : Cmd Msg
fetchTodos =
    API.getApiTodos FetchTodos


changeTodo : Todo -> Cmd Msg
changeTodo todo =
    API.putApiTodosById todo.id todo (always Reload)


addTodo : Todo -> Cmd Msg
addTodo todo =
    API.postApiTodos todo (always Reload)


removeTodo : Int -> Cmd Msg
removeTodo todoId =
    API.deleteApiTodosById todoId (always Reload)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
