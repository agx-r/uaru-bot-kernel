import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer

open Lean Json ToJson FromJson

namespace UaruKernel

----------------------------------------
structure User where
  userId     : Option Nat := none
  firstName  : Option String := none
  lastName   : Option String := none
  username   : Option String := none
  isPremium  : Option Bool := none
  isGuest    : Option Bool := none
  deriving FromJson

structure NewMember where
  user : User
  deriving FromJson

structure Message where
  messageId      : Option Nat := none
  fromUser       : Option User := none
  text           : Option String := none
  replyToMessage : Option Message := none
  isImage        : Option Bool := none
  isFile         : Option Bool := none
  isSticker      : Option Bool := none
  deriving FromJson

--- Возможные event:
-- message sent
structure IPCInput where
  event   : Option String := "empty"
  message : Option Message := none
  date    : Option String := none
  deriving FromJson
----------------------------------------


----------------------------------------
structure SendMessageAction where
  text       : String := "empty"
  replyToId  : Option Nat := none 
  deriving ToJson 

structure BanUserAction where
  userId : Nat := 0 
  deriving ToJson 

structure MuteUserAction where
  userId : Nat := 0 
  duration : Nat := 300 -- in seconds
  deriving ToJson 

structure UnmuteUserAction where
  userId : Nat := 0 
  deriving ToJson 

structure PinMessageAction where
  messageId : Nat := 0 
  deriving ToJson 

structure DeleteMessageAction where
  messageId : Nat := 0 
  deriving ToJson 

inductive ResponseStatus
  | default
  | ok
  | error
  | unknownCmd
  | unknownStatus
  deriving ToJson 

structure IPCResponse where
  status         : ResponseStatus := ResponseStatus.unknownStatus 
  sendMessage    : Option SendMessageAction := none 
  pinMessage     : Option PinMessageAction := none
  deleteMessage  : Option DeleteMessageAction := none
  muteUser       : Option MuteUserAction := none 
  unmuteUser     : Option UnmuteUserAction := none
  banUser        : Option BanUserAction := none 
  deriving ToJson 
----------------------------------------


def handleCommand (jsonStr : String) : String :=
  match Json.parse jsonStr >>= fromJson? with
  | .error e => s!"error parsing JSON: {e}"
  | .ok (input : IPCInput) => 
    match input.event, input.message.bind (·.text) with
    | some "message sent", some "/start" =>
        let resp : IPCResponse := {
          status : ResponseStatus := ResponseStatus.ok,
          sendMessage := some {
            text := "бот работает",
            replyToId := input.message.bind (·.messageId)
          }
        }
        toJson resp |>.compress
    | some "message sent", _ =>
        let resp : IPCResponse := {
          status := ResponseStatus.unknownCmd 
        }
        toJson resp |>.compress
    | _, _ =>
        let resp : IPCResponse := {
          status := ResponseStatus.unknownStatus 
        }
        toJson resp |>.compress

end UaruKernel
