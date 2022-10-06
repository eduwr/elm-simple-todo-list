module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, img, input, li, p, span, text, ul)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type State
    = Done
    | InProgress
    | Stalled


type alias Todo =
    { id : Int
    , description : String
    , state : State
    }



-- Refactor to use Dictionary


type alias TodoList =
    List Todo


type alias Model =
    { todos : TodoList
    , newTodo : String
    }


init : Model
init =
    { todos = []
    , newTodo = ""
    }


type Msg
    = AddTodo
    | DeleteTodo Int
    | ChangeNewTodo String
    | UpdateState Int State


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            { model | todos = addTodo model.todos model.newTodo, newTodo = "" }

        DeleteTodo id ->
            { model | todos = deleteTodo model.todos id }

        ChangeNewTodo newTodo ->
            { model | newTodo = newTodo }

        UpdateState id state ->
            { model | todos = updateListItemState model.todos id (getNextState state) }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "form-input" ]
            [ input [ type_ "text", placeholder "Type a new todo", value model.newTodo, onInput ChangeNewTodo ] []
            , button [ type_ "button", onClick AddTodo ] [ text "Add Todo" ]
            ]
        , ul [ class "todo-list" ]
            (model.todos |> List.map buildTodoItem)
        ]


buildTodoItem : Todo -> Html Msg
buildTodoItem todo =
    li [ class (getClassByState todo.state "todo-item") ]
        [ span [] [ text todo.description ]
        , button [ type_ "button", onClick (UpdateState todo.id todo.state) ] [ text "Update State" ]
        , button [ type_ "button", onClick (DeleteTodo todo.id) ] [ text "Delete" ]
        ]


createNewTodo : Int -> String -> Todo
createNewTodo id description =
    Todo id description Stalled


nextId : TodoList -> Int
nextId todoList =
    List.length todoList + 1


addTodo : TodoList -> String -> TodoList
addTodo prevTodoList description =
    prevTodoList ++ [ createNewTodo (nextId prevTodoList) description ]


updateListItemState : TodoList -> Int -> State -> TodoList
updateListItemState prevTodoList id newState =
    prevTodoList
        |> List.map
            (\todo ->
                if todo.id == id then
                    { todo | state = newState }

                else
                    todo
            )


deleteTodo : TodoList -> Int -> TodoList
deleteTodo oldList id =
    List.filter (\todo -> todo.id /= id) oldList


getNextState : State -> State
getNextState state =
    if state == Stalled then
        InProgress

    else if state == InProgress then
        Done

    else
        Stalled


getClassByState : State -> String -> String
getClassByState state prepend =
    if state == Stalled then
        prepend ++ " stalled"

    else if state == InProgress then
        prepend ++ " in-progress"

    else
        prepend ++ " done"
