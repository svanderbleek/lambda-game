import StartApp.Simple
import Html
import Html.Events
import Lambda

main = 
  StartApp.Simple.start {
    model = Lambda.Hole,
    view = view, 
    update = update
  }

type Binding = Free | Bound
type Direction = Left | Right
type Action = Variable | Abstract Binding | Apply Direction

update : Action -> Lambda.Expression String -> Lambda.Expression String
update action model =
  case action of
    Variable -> case model of
      Lambda.Hole -> Lambda.Variable "s"
      _ -> model
    Abstract _ -> case model of
      Lambda.Variable _ -> Lambda.Abstraction "s" model
      _ -> model
    Apply _ -> case model of
      Lambda.Abstraction _ _ -> Lambda.Application model Lambda.Hole
      _ -> model

view address model =
  Html.div [] [
    Html.h1 [] [Html.text "Lambda Game"],
    Html.h2 [] [Html.text "Expression"],
    Html.text (Lambda.view model),
    Html.h2 [] [Html.text "Actions"],
    Html.button [Html.Events.onClick address Variable] [Html.text "Variable"],
    Html.button [Html.Events.onClick address (Abstract Free)] [Html.text "Abstract"],
    Html.button [Html.Events.onClick address (Apply Right)] [Html.text "Apply"]
  ]
