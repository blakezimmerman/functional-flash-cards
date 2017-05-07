import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type Mode
  = Create
  | Study

type alias Card =
  { frontCard : String
  , backCard : String
  }

type alias Model =
  { mode : Mode
  , currentInput : Card
  , cardList : Array Card
  , currentIndex : Int
  , currentCard : Maybe Card
  , showFront : Bool
  }

model : Model
model =
  { mode = Create
  , currentInput = Card "" ""
  , cardList = Array.empty
  , currentIndex = 0
  , currentCard = Nothing
  , showFront = True
  }


-- UPDATE

type Msg
  = CreateMode
  | StudyMode
  | FrontCard String
  | BackCard String
  | AddCard
  | Previous
  | Next
  | Flip

update : Msg -> Model -> Model
update msg model =
  case msg of
    CreateMode ->
      { model
      | mode = Create
      , currentInput = Card "" ""
      }

    StudyMode ->
      { model
      | mode = Study
      , currentIndex = 0
      , currentCard =
          Array.get 0 model.cardList
      , showFront = True
      }

    FrontCard front ->
      { model | currentInput = { frontCard = front, backCard = model.currentInput.backCard } }

    BackCard back ->
      { model | currentInput = { frontCard = model.currentInput.frontCard, backCard = back } }

    AddCard ->
      { model | cardList = Array.push model.currentInput model.cardList }

    Previous ->
      if model.currentIndex > 0 then
      { model
      | currentCard =
          Array.get (model.currentIndex-1) model.cardList
      , currentIndex = model.currentIndex - 1
      , showFront = True
      }
      else model

    Next ->
      if model.currentIndex < (Array.length model.cardList-1) then
      { model
      | currentCard =
          Array.get (model.currentIndex+1) model.cardList
      , currentIndex = model.currentIndex + 1
      , showFront = True
      }
      else model

    Flip ->
      { model | showFront = not model.showFront }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ class "header" ]
        [ text "Flash Cards!"
        ]
    , div []
        [ button [ onClick CreateMode ] [ text "Create Mode" ]
        , button [ onClick StudyMode ] [ text "Study Mode" ]
        ]
    , div []
        [ if model.mode == Create
          then addCardForm model
          else studyCardList model
        ]
    ]


addCardForm : Model -> Html Msg
addCardForm model =
  div [ class "cardInput" ]
    [ div [ class "inputHeader" ] [ text "Add a Card" ]
    , input [ placeholder "Front of Card", onInput FrontCard ] []
    , input [ placeholder "Back of Card", onInput BackCard ] []
    , button [ onClick AddCard, class "addButton" ] [ text "Add Card" ]
    , if Array.length model.cardList /= 0 then renderCardList model.cardList
      else text ""
    ]

renderCardList : Array Card -> Html Msg
renderCardList cardList =
  div [ class "renderCards"]
    [ text "Current Cards"
    , table []
        ( List.append
          [ tr []
              [ td [ class "tableHeader" ] [ text "Front of Card" ]
              , td [ class "tableHeader" ] [ text "Back of Card" ]
              ]
          ]
          ( Array.toList (Array.map (\card ->
            tr []
              [ td [] [ text card.frontCard ]
              , td [] [ text card.backCard ]
              ]
          ) cardList))
        )
    ]

studyCardList : Model -> Html Msg
studyCardList model =
  div []
    [ renderCurCard model
    , div [ class "studyButtons" ]
        [ button [ class "studyButton", onClick Previous ] [ text "Previous Card" ]
        , button [ class "studyButton", onClick Flip ] [ text "Flip Card" ]
        , button [ class "studyButton", onClick Next ] [ text "Next Card" ]
        ]
    ]

renderCurCard : Model -> Html Msg
renderCurCard model =
  div [ class "curCard" ]
    [ if Array.length model.cardList == 0 then text "No cards made yet"
      else
        if model.showFront
        then case model.currentCard of
                Nothing -> text "Out of Range"
                Just currentCard -> div [] 
                  [ div [ class "cardText" ]
                      [ text currentCard.frontCard ]
                  , renderCardPosition model
                  ]
        else case model.currentCard of
                Nothing -> text "Out of Range"
                Just currentCard -> div [] 
                  [ div [ class "cardText" ]
                      [ text currentCard.backCard ]
                  , renderCardPosition model
                  ]
    ]

renderCardPosition : Model -> Html Msg
renderCardPosition model =
  div [ class "cardPosition" ]
    [ text (toString (model.currentIndex+1) ++ " of " ++ toString (Array.length model.cardList))
    ]
