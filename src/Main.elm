import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
  , currentCard : Card
  , cardList: List Card
  }

model : Model
model =
  { mode = Create
  , currentCard = Card "" ""
  , cardList = []
  }


-- UPDATE

type Msg
  = CreateMode
  | StudyMode
  | FrontCard String
  | BackCard String
  | AddCard

update : Msg -> Model -> Model
update msg model =
  case msg of
    CreateMode ->
      { model | mode = Create }

    StudyMode ->
      { model | mode = Study }

    FrontCard front ->
      { model | currentCard = { frontCard = front, backCard = model.currentCard.backCard } }

    BackCard back ->
      { model | currentCard = { frontCard = model.currentCard.frontCard, backCard = back } }

    AddCard ->
      { model | cardList = model.currentCard :: model.cardList }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div []
        [ button [ onClick CreateMode ] [ text "Create Mode" ]
        , button [ onClick StudyMode ] [ text "Study Mode" ]
        ]
    , div []
        [ if model.mode == Create
          then addCardForm model
          else studyCardList model.cardList
        ]
    ]


addCardForm : Model -> Html Msg
addCardForm model =
  div []
    [ input [ placeholder "Front of Card", onInput FrontCard ] []
    , input [ placeholder "Back of Card", onInput BackCard ] []
    , button [ onClick AddCard ] [ text "Add Card" ]
    , renderCardList model.cardList
    ]

renderCardList : List Card -> Html Msg
renderCardList cardList =
  div [ class "renderCards"]
    [ text "Current Cards:"
    , table []
        ( List.append
          [ tr []
              [ td [] [ text "Front of Card" ]
              , td [] [ text "Back of Card" ]
              ]
          ]
          (List.map (\card ->
            tr []
              [ td [] [ text card.frontCard ]
              , td [] [ text card.backCard ]
              ]
          ) cardList)
        )
    ]

studyCardList : List Card -> Html Msg
studyCardList cardList =
  div []
    [ text "Time to Study!"
    ]
