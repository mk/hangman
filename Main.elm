module Main (..) where

import Debug
import Signal
import Set
import String
import NativeUi as Ui
import NativeUi.NativeApp as NativeApp
import NativeUi.Style as Style exposing (defaultTransform)
import NativeUi.Elements exposing (..)
import NativeUi.Properties exposing (..)
import NativeUi.Handlers exposing (..)
import Random


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
  , seed : Random.Seed
  }


model : Model
model =
  newWord initialSeed


blanks : String -> String
blanks word =
  (List.map (\c -> '_') (String.toList word))
    |> String.fromList


newWord : Random.Seed -> Model
newWord seed =
  let
    words =
      [ "radzange", "zufall", "wirrungen", "ausblick", "shanghai", "kilometer" ]

    ( randomIndex, newSeed ) =
      Random.generate (Random.int 0 3) seed

    solution =
      (List.head <| List.drop randomIndex words)
        |> Maybe.withDefault "waldsterben"
  in
    { solution = solution
    , display = blanks solution
    , attempts = []
    , seed = newSeed
    }


maxAttempts : Int
maxAttempts =
  11


incorrectAttempts : Model -> Int
incorrectAttempts model =
  let
    correctChars =
      String.toList model.solution
  in
    List.filter (\char -> not (List.member char correctChars)) model.attempts
      |> List.length


remainingAttempts : Model -> Int
remainingAttempts model =
  let
    remaining =
      maxAttempts - (incorrectAttempts model)
  in
    if remaining > 0 then
      remaining
    else
      0


hasWon : Model -> Bool
hasWon model =
  model.solution == model.display && (incorrectAttempts model) < maxAttempts


hasLost : Model -> Bool
hasLost model =
  (incorrectAttempts model) >= maxAttempts


stickmanView : Signal.Address Action -> Int -> Ui.NativeUi
stickmanView address incorrAttempts =
  let
    src =
      if incorrAttempts > 0 then
        (toString incorrAttempts)
      else
        "new-word"
  in
    image
      [ source src
      , Ui.style
          [ Style.width 150
          , Style.height 152
          , Style.marginTop 100
          ]
      ]
      []


youWonView : Signal.Address Action -> Ui.NativeUi
youWonView address =
  image
    [ source "you-won"
    , Ui.style
        [ Style.width 150
        , Style.height 152
        , Style.marginTop 100
        ]
    ]
    []


statusView : Signal.Address Action -> Model -> Ui.NativeUi
statusView address model =
  let
    incorrAttempts =
      incorrectAttempts model

    ( incorrLabelText, incorrLabelColor ) =
      if (hasLost model) then
        ( "You lost :( Tap here for another word.", "#fffed3" )
      else
        ( "Too hard? Tap here for another word.", "#fff" )

    content =
      if hasWon model then
        [ youWonView address
        , text
            [ Ui.style
                [ Style.fontWeight "normal"
                , Style.fontSize 16
                , Style.color "#fff"
                , Style.marginTop 20
                ]
            , onPress address NewWord
            ]
            [ Ui.string "You won! Tap here for another word." ]
        ]
      else
        [ stickmanView address incorrAttempts
        , text
            [ Ui.style
                [ Style.fontWeight "normal"
                , Style.fontSize 14
                , Style.color incorrLabelColor
                , Style.marginTop 20
                ]
            , onPress address NewWord
            ]
            [ Ui.string incorrLabelText ]
        ]
  in
    NativeUi.Elements.view
      [ Ui.style
          [ Style.flex 1
          , Style.alignItems "center"
          , Style.alignSelf "stretch"
          ]
      ]
      content



-- [  ]


view : Signal.Address Action -> Model -> Ui.NativeUi
view address model =
  let
    remainingAtts =
      remainingAttempts model

    display =
      if hasLost model then
        model.solution
      else
        model.display

    remainingText =
      if remainingAtts > 1 then
        (toString remainingAtts) ++ " tries left"
      else if remainingAtts == 1 then
        "one try left"
      else
        "no tries left"
  in
    NativeUi.Elements.view
      [ Ui.style
          [ Style.flex 1
          , Style.flexDirection "column"
          , Style.alignItems "center"
          , Style.backgroundColor "#2a9cfa"
          , Style.alignSelf "stretch"
          ]
      ]
      [ NativeUi.Elements.view
          [ Ui.style
              [ Style.alignItems "center" ]
          ]
          [ statusView address model
          , text
              [ Ui.style
                  [ Style.letterSpacing 4
                  , Style.fontWeight "bold"
                  , Style.fontSize 30
                  , Style.color "#fff"
                  , Style.marginTop 20
                  ]
              ]
              [ Ui.string display ]
          , text
              [ Ui.style
                  [ Style.fontWeight "normal"
                  , Style.fontSize 18
                  , Style.color "#fff"
                  , Style.marginTop 10
                  ]
              ]
              [ Ui.string remainingText ]
          , textInput
              [ value ""
              , placeholder "Type to take your guess"
              , onChangeText address (\s -> TextChanged s)
              , autoFocus True
              , Ui.style
                  [ Style.height 50
                  , Style.width 200
                  , Style.marginBottom 30
                  , Style.textAlign "center"
                  , Style.alignSelf "center"
                  , Style.fontSize 12
                  ]
              ]
              []
          ]
      ]


type Action
  = TextChanged String
  | NewWord
  | Noop


update : Action -> Model -> Model
update action model =
  case action of
    NewWord ->
      newWord model.seed

    TextChanged newText ->
      if hasLost model then
        model
      else
        let
          attempts =
            Set.toList (Set.fromList (model.attempts ++ (String.toList (String.toLower newText))))

          display =
            String.map
              (\char ->
                if (List.member char attempts) then
                  char
                else
                  '_'
              )
              model.solution
        in
          { model
            | display = display
            , attempts = attempts
          }

    Noop ->
      model


port randomSeed : Int
initialSeed : Random.Seed
initialSeed =
  Random.initialSeed randomSeed
