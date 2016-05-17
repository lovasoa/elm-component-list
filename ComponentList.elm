module ComponentList exposing (
    Model, init, getModels, setModels,
    Msg, update,
    ViewParams, view
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
@docs init, getModels, setModels, Model

# View
@docs ViewParams, view

# Update
@docs Msg, update
-}

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- MODEL
type alias ID = Int

{-| Represents a model.-}
type alias Model componentModel =
  {
    models : List (ID, componentModel),
    nextID : ID,
    defaultModel : componentModel
  }


{-| Create a new model representing a list of elements of the given model

    -- EXAMPLE
    -- simple component that prints a hello button
    cModel = "Hello"
    cUpdate v _ = v
    cView cmodel = input [onInput identity, value cmodel] []
    -- Create a list of text inputs
    main =
      App.beginnerProgram {
        model =
          init "New hello" "Delete this hello" cModel,
        view = view cView,
        update = update cUpdate
      }
-}
init : a -> Model a
init model =
  { models = []
  , nextID = 0
  , defaultModel = model
  }

{-|Get the the models of the elements of a `ComponentList` -}
getModels : Model a -> List a
getModels model = List.map snd model.models

{-|Set the the models of the elements of a `ComponentList` -}
setModels : List a -> Model a -> Model a
setModels cmodels model =
  {model |
    models = List.map2 (,) [0..List.length cmodels] cmodels,
    nextID = List.length cmodels
  }

-- UPDATE
{-| Represents a model update message-}
type Msg componentMsg
  = Insert
  | InsertAfter ID
  | Remove ID
  | Modify ID componentMsg

{-| Given the component update function, returns a `ComponentList` update function
    update Counter.update
-}
update : (compMsg -> compModel -> compModel) -> Msg compMsg -> Model compModel -> Model compModel
update updateModel msg model =
  case msg of
    Insert ->
        {model |
            nextID = model.nextID + 1,
            models = model.models ++ [(model.nextID, model.defaultModel)]
        }

    InsertAfter id ->
        let
          mapper (cid, cmodel) =
            if cid == id then [(cid, cmodel), (model.nextID, model.defaultModel)]
            else [(cid, cmodel)]
        in
        {model |
            nextID = model.nextID + 1,
            models = List.concatMap mapper model.models
        }

    Remove id ->
      { model | models = List.filter (\(mid,_) -> mid /= id) model.models }

    Modify id componentMsg ->
      let
        updateComponent componentId componentModel =
          if componentId == id then
            updateModel componentMsg componentModel
          else
            componentModel
      in
        { model |
          models = List.map
                      (\(cid,cm) -> (cid, updateComponent cid cm))
                      model.models
        }


-- VIEW
{-| Represents the text to display in the GUI view
    * *newModelTxt* : text to display in the "+" button
    * *deleteModelTxt* : text to display in the "-" button
-}
type alias ViewParams = {
    newModelTxt : String,
    deleteModelTxt : String
}

{-| Given a component view function, return a ComponentList view function-}
view : ViewParams -> (compModel -> Html compMsg) -> Model compModel -> Html (Msg compMsg)
view params viewComponent model =
  div [class "ComponentList"]
      [
          ul [] (List.map (viewComponentActions params viewComponent) model.models)
        , button
            [  onClick Insert
             , class "newComponent"]
            [text params.newModelTxt]
      ]

viewComponentWith : (compModel -> Html compMsg) -> (ID, compModel) -> Html (Msg compMsg)
viewComponentWith viewComponent (id, model) =
  App.map (Modify id) (viewComponent model)

{-| View a component and its associated "new component" and "delete component"
buttons
-}
viewComponentActions : ViewParams -> (compModel -> Html compMsg) -> (ID, compModel) -> Html (Msg compMsg)
viewComponentActions params viewComponent (id, compModel) =
  li [class "Component"]
      [
        viewComponentWith viewComponent (id, compModel),
        div [class "ComponentListActions"]
          [
            button
              [   class "deleteComponent"
                , onClick (Remove      id)]
              [text params.deleteModelTxt],
            button
              [   class "insertComponent"
                , onClick (InsertAfter id)]
              [text params.newModelTxt   ]
          ]
      ]


-- EXAMPLE
-- simple component that prints a hello button
cModel = "Hello"
cUpdate v _ = v
cView cmodel = input [onInput identity, value cmodel] []
main =
  App.beginnerProgram {
    model = init cModel,
    view = view (ViewParams "New hello" "Delete this hello") cView,
    update = update cUpdate
  }
