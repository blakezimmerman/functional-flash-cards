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
          else renderCardList model.cardList
        ]
    ]


addCardForm : Model -> Html Msg
addCardForm model =
  div []
    [ input [ placeholder "Front of Card", onInput FrontCard ] []
    , input [ placeholder "Back of Card", onInput BackCard ] []
    , button [ onClick AddCard ] [ text "Add Card" ]
    ]

renderCardList : List Card -> Html Msg
renderCardList cardList =
  ul []
    (List.map (\card ->
      li [] [ text ("Front: " ++ card.frontCard ++ " Back: " ++ card.backCard) ]
    ) cardList)
