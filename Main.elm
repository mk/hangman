module Main (..) where

import Debug
import Signal
import String
import NativeUi as Ui
import NativeUi.NativeApp as NativeApp
import NativeUi.Style as Style exposing (defaultTransform)
import NativeUi.Elements exposing (..)
import NativeUi.Properties exposing (..)
import NativeUi.Handlers exposing (..)


app : Signal Ui.NativeUi
app =
  NativeApp.start { model = model, view = view, update = update }


main : Signal Ui.NativeUi
main =
  app


type alias Model =
  { solution : String
  , display : String
  , attempts : List Char
  }


model : Model
model =
  { solution = "waldsterben"
  , display = "___________"
  , attempts = []
  }


remainingAttempts model =
  let
    remaining =
      10 - List.length (List.filter (\c -> not (List.member c (String.toList model.solution))) model.attempts)
  in
    if remaining > 0 then
      remaining
    else
      0



-- [  ]


view : Signal.Address Action -> Model -> Ui.NativeUi
view address model =
  view
    [ Ui.style [ Style.alignItems "center" ]
    ]
    [ text
        [ Ui.style
            [ Style.fontWeight "bold"
            , Style.fontSize 30
            ]
        ]
        [ Ui.string ((toString (remainingAttempts model)) ++ " tries left.") ]
    , text
        [ Ui.style
            [ Style.backgroundColor "#ccc"
            , Style.letterSpacing 4
            , Style.borderColor "#000"
            , Style.borderWidth 1
            , Style.borderRadius 5
            , Style.fontWeight "bold"
            , Style.fontSize 30
            ]
        ]
        [ Ui.string model.display ]
    , textInput
        [ value ""
        , placeholder "tap here to take a guess"
        , onChangeText address (\s -> TextChanged s)
        , Ui.style
            [ Style.height 50
            , Style.width 200
            , Style.marginBottom 30
            ]
        ]
        []
    ]


type Action
  = TextChanged String
  | Noop


update : Action -> Model -> Model
update action model =
  case action of
    TextChanged newText ->
      let
        d =
          Debug.log "newText" newText

        solution =
          model.solution

        attempts =
          model.attempts ++ (String.toList (String.toLower newText))

        display =
          String.map
            (\char ->
              if (List.member char attempts) then
                char
              else
                '_'
            )
            solution
      in
        { model
          | display = display
          , attempts = attempts
        }

    Noop ->
      model
