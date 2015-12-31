import StartApp.Simple
import Html
import Html.Events
import Lambda

main = 
  StartApp.Simple.start {
    model = Lambda.Empty,
    view = view, 
    update = update
  }

type Action = Variable | Abstract | Apply

update : Action -> Lambda.Expression String -> Lambda.Expression String
update action model =
  case action of
    Variable -> case model of
      Lambda.Empty -> Lambda.Variable "s"
      _ -> model
    Abstract -> case model of
      Lambda.Variable _ -> Lambda.Abstraction "s" model
      _ -> model
    Apply -> case model of
      Lambda.Abstraction _ _ -> Lambda.Application model Lambda.Empty
      _ -> model

view address model =
  Html.div [] [
    Html.h1 [] [Html.text "Lambda Game"],
    Html.h2 [] [Html.text "Expression"],
    Html.text (Lambda.view model),
    Html.h2 [] [Html.text "Actions"],
    Html.button [Html.Events.onClick address Variable] [Html.text "Variable"],
    Html.button [Html.Events.onClick address Abstract] [Html.text "Abstract"],
    Html.button [Html.Events.onClick address Apply] [Html.text "Apply"]
  ]
