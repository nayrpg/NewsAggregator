port module Ports exposing (storeState, loadState)

port storeState : String -> Cmd msg
port loadState : (String -> msg) -> Sub msg
-- stuff
