module Main (..) where

import Signal
import String
import NativeUi as Ui
import NativeUi.NativeApp as NativeApp
import NativeUi.Style as Style exposing (defaultTransform)


app : Signal Ui.NativeUi
app =
  NativeApp.start { model = model, view = view, update = update }


main : Signal Ui.NativeUi
main =
  app


type alias Model =
  { solution: String
  , display: String
  , attempts: List Char }


model : Model
model =
  { solution = "waldsterben"
  , display = "___________"
  , attempts = ['e', 'r']
  }


view : Signal.Address Action -> Model -> Ui.NativeUi
view address model =
  Ui.view
    [ Ui.style [ Style.alignItems "center" ]
    ]
    [ Ui.textInput
        [ Ui.value model.display
        , Ui.style
            [ Style.height 50
            , Style.width 200
            , Style.marginBottom 30
            ]
        ]
    ]


type Action
  = Increment
  | Decrement


update : Action -> Model -> Model
update action model =
  let
    solution = model.solution
    attempts = model.attempts
    display = String.map (\char -> if (List.member char attempts) then char else '_' ) solution
  in
    { model | display = display }
