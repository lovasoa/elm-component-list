module ComponentList
    exposing
        ( Model
        , init
        , getModels
        , setModels
        , Msg
        , update
        , ViewParams
        , view
        )

{-| This library implements a generic list component, that, given a component type, creates a list of it
It can be used everywhere in your interface where there are list of elements with
which the user can interact.

The generated html will have this form:

    <div class="ComponentList">
      <ul>
        <li class="Component">
          {{YOUR COMPONENT}}
          <div class="ComponentListActions">
            <button>{{Your deleteModelTxt}}</button>
            <button>{{Your newModelTxt}}</button>
          </div>
        </li>
      </ul>
      <button class="newComponent">{{Your newModelTxt}}</button>
    </div>


# Model

## Model creation
How to create a `ComponentList.Model` from your own component model.

@docs init, setModels, Model

## Get the information back from the `ComponentList`
@docs getModels

# Update
@docs Msg, update

# View
Create a list view from your own component's view
@docs ViewParams, view

-}

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MODEL


type alias ID =
    Int


{-| Represents a model.
-}
type alias Model componentModel =
    { models : List ( ID, componentModel )
    , nextID : ID
    , defaultModel : componentModel
    }


{-| Create a new model representing a list of elements of the given model.
When a new element will be added to the list, its initial model will be the model
given as the first parameter of this function.

    -- EXAMPLE
    -- simple component, representing a string, and printing it to a text field
    cModel = "Hello"
    cUpdate v _ = v
    cView cmodel = input [onInput identity, value cmodel] []

    -- Using ComponentList to create a list of the component defined above
    main =
      Html.beginnerProgram {
        model = init cModel,
        view = view (ViewParams "New hello" "Delete this hello") cView,
        update = update cUpdate
      }
-}
init : a -> Model a
init model =
    { models = []
    , nextID = 0
    , defaultModel = model
    }


{-| Get the the models of the elements of a `ComponentList`
-}
getModels : Model a -> List a
getModels model =
    List.map Tuple.second model.models


{-| Set the the models of the elements of a `ComponentList`
-}
setModels : List a -> Model a -> Model a
setModels cmodels model =
    { model
        | models =
            List.map2 (,)
                (List.range 0 (List.length cmodels))
                cmodels
        , nextID = List.length cmodels
    }



-- UPDATE


{-| Represents a model update message.
`componentMsg` is the type of the component messages.
-}
type Msg componentMsg
    = Insert
    | InsertAfter ID
    | Remove ID
    | Modify ID componentMsg


{-| Given the component update function, returns a `ComponentList` update function

    updateComponentList = ComponentList.update updateComponent
-}
update : (compMsg -> compModel -> compModel) -> Msg compMsg -> Model compModel -> Model compModel
update updateModel msg model =
    case msg of
        Insert ->
            { model
                | nextID = model.nextID + 1
                , models = model.models ++ [ ( model.nextID, model.defaultModel ) ]
            }

        InsertAfter id ->
            let
                mapper ( cid, cmodel ) =
                    if cid == id then
                        [ ( cid, cmodel ), ( model.nextID, model.defaultModel ) ]
                    else
                        [ ( cid, cmodel ) ]
            in
                { model
                    | nextID = model.nextID + 1
                    , models = List.concatMap mapper model.models
                }

        Remove id ->
            { model | models = List.filter (\( mid, _ ) -> mid /= id) model.models }

        Modify id componentMsg ->
            let
                updateComponent componentId componentModel =
                    if componentId == id then
                        updateModel componentMsg componentModel
                    else
                        componentModel
            in
                { model
                    | models =
                        List.map
                            (\( cid, cm ) -> ( cid, updateComponent cid cm ))
                            model.models
                }



-- VIEW


{-| Represents the text to display in the GUI view

  * *newModelTxt* : text to display in the "+" button
  * *deleteModelTxt* : text to display in the "-" button
-}
type alias ViewParams =
    { newModelTxt : String
    , deleteModelTxt : String
    }


{-| Given a component view function, return a ComponentList view function

    params = ViewParams "New component" "Delete this component"
    listView = ComponentList.view params componentView
-}
view : ViewParams -> (compModel -> Html compMsg) -> Model compModel -> Html (Msg compMsg)
view params viewComponent model =
    div [ class "ComponentList" ]
        [ ul [] (List.map (viewComponentActions params viewComponent) model.models)
        , button
            [ onClick Insert
            , class "newComponent"
            ]
            [ text params.newModelTxt ]
        ]


viewComponentWith : (compModel -> Html compMsg) -> ( ID, compModel ) -> Html (Msg compMsg)
viewComponentWith viewComponent ( id, model ) =
    Html.map (Modify id) (viewComponent model)


{-| View a component and its associated "new component" and "delete component"
buttons
-}
viewComponentActions : ViewParams -> (compModel -> Html compMsg) -> ( ID, compModel ) -> Html (Msg compMsg)
viewComponentActions params viewComponent ( id, compModel ) =
    li [ class "Component" ]
        [ viewComponentWith viewComponent ( id, compModel )
        , div [ class "ComponentListActions" ]
            [ button
                [ class "deleteComponent"
                , onClick (Remove id)
                ]
                [ text params.deleteModelTxt ]
            , button
                [ class "insertComponent"
                , onClick (InsertAfter id)
                ]
                [ text params.newModelTxt ]
            ]
        ]



-- EXAMPLE
-- simple component that displays a text field


cModel =
    "Hello"



-- The initial content of the text field


cUpdate newText oldText =
    newText


cView cmodel =
    -- A very simple view, just a text field containing the model
    input [ onInput identity, value cmodel ] []


main =
    Html.beginnerProgram
        { model = init cModel
        , view =
            -- Indicate the text that will be displayed on the buttons
            view (ViewParams "New hello" "Delete this hello") cView
        , update = update cUpdate
        }
