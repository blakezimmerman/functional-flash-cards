#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-arch/Fable.Arch.dll"

open Fable.Core
open Fable.Core.JsInterop

open Fable.Arch
open Fable.Arch.App
open Fable.Arch.Html


// MODEL

type Mode =
  | Create
  | Study

type Card =
  { FrontCard : string
    BackCard : string
  }

type Model =
  { Mode : Mode
    CurrentInput : Card
    CardList : Card[]
    CurrentIndex : int
    CurrentCard : Card
    ShowFront : bool
  }

let initialModel =
  { Mode = Create
    CurrentInput = { FrontCard = ""; BackCard = "" }
    CardList = Array.empty
    CurrentIndex = 0
    CurrentCard = { FrontCard = ""; BackCard = "" }
    ShowFront = true
  }

// UPDATE

type Actions = 
  | CreateMode
  | StudyMode
  | FrontCard of string
  | BackCard of string
  | AddCard
  | Previous
  | Next
  | Flip

let update model action =
  match action with
  | CreateMode ->
      { model with
          Mode = Create
          CurrentInput = { FrontCard = ""; BackCard = "" }
      }
  
  | StudyMode ->
      { model with
          Mode = Study
          CurrentIndex = 0
          CurrentCard = model.CardList.[0]
          ShowFront = true
      }
   
  | FrontCard front ->
      { model with
          CurrentInput = { FrontCard = front; BackCard = model.CurrentInput.BackCard }
      }
  
  | BackCard back ->
      { model with
          CurrentInput = { FrontCard = model.CurrentInput.FrontCard; BackCard = back }
      }
  
  | AddCard ->
      { model with
          CardList = Array.append model.CardList [| model.CurrentInput |]
          CurrentInput = { FrontCard = ""; BackCard = "" }
      }
  
  | Previous ->
      if model.CurrentIndex > 0 then
        { model with
            CurrentCard = model.CardList.[model.CurrentIndex - 1]
            CurrentIndex = model.CurrentIndex - 1
            ShowFront = true
        }
      else model
  
  | Next ->
      if model.CurrentIndex < model.CardList.Length - 1 then
        { model with
            CurrentCard = model.CardList.[model.CurrentIndex + 1]
            CurrentIndex = model.CurrentIndex + 1
            ShowFront = true
        }
      else model
  
  | Flip ->
      { model with
          ShowFront = not model.ShowFront
      }

// VIEW

let clickButton className (action:Actions) txt =
    button [ classy className
             onMouseClick (fun _ -> action)
           ]
           [ text txt ]

let textInput placeholder value (action:string->Actions) =
  input [ property "type" "text" 
          property "placeholder" placeholder
          property "value" value
          onEvent "oninput" (fun e -> action (unbox e?target?value))
        ]

let renderCardPosition model =
  div [ classy "cardPosition" ]
    [ text ((model.CurrentIndex + 1).ToString() + " of " + (model.CardList.Length).ToString()) ]

let renderCard cardText model =
    div []
      [ div [ classy "cardText" ]
          [ text cardText ]
        renderCardPosition model
      ]

let renderCurCard model =
  div [ classy "curCard" ]
    [ ( match model.CardList.Length with
        | 0 -> text "No Cards made yet"
        | _ ->  match model.ShowFront with
                | true -> renderCard model.CurrentCard.FrontCard model
                | false -> renderCard model.CurrentCard.BackCard model
      )
    ]

let studyCardList model =
  div []
    [ renderCurCard model 
      div [ classy "studyButtons" ]
        [ clickButton "studyButton" Previous "Previous Card"
          clickButton "studyButton" Flip "Flip Card"
          clickButton "studyButton" Next "Next Card"
        ]
    ]

let tableHeader =
  [ tr []
      [ td [ classy "tableHeader" ] [ text "Front of Card" ]
        td [ classy "tableHeader" ] [ text "Back of Card" ]
      ]
  ]

let tableBody cardList =
  Array.toList (Array.map (fun card ->
        tr []
          [ td [] [ text card.FrontCard ]
            td [] [ text card.BackCard ]
          ]
    ) cardList)

let renderCardList cardList =
    div [ classy "renderCards" ]
      [ text "Current Cards"
        table [] (List.append tableHeader (tableBody cardList))
      ]

let addCardForm model =
  div [ classy "cardInput" ]
    [ div [ classy "inputHeader" ] [ text "Add a Card" ]
      textInput "Front of Card" model.CurrentInput.FrontCard FrontCard
      textInput "Back of Card" model.CurrentInput.BackCard BackCard
      clickButton "addButton" AddCard "Add Card"
      ( if model.CardList.Length <> 0
        then renderCardList model.CardList
        else text ""
      )
    ]

let view (model: Model) =
  div []
    [ div [ classy "header" ]
        [ text "Flash Cards!" ]
      div []
        [ clickButton "" CreateMode "Create Mode"
          clickButton "" StudyMode "Study Mode"
        ]
      div []
        [ ( match model.Mode with
            | Create -> addCardForm model
            | Study -> studyCardList model
          )
        ]
    ]


createSimpleApp initialModel view update Virtualdom.createRender
|> withStartNodeSelector "#app"
|> start
