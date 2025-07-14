import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer

open Lean Json ToJson FromJson

namespace UaruKernel


----------------------------------------
inductive IPCLogLevel
  | debug
  | info
  | warning
  | error
  deriving ToJson
  
structure IPCLog where
  level : IPCLogLevel
  msg : String
  deriving ToJson
----------------------------------------

----------------------------------------
inductive UpdateEvent 
  | messageSent
  | userJoined
  | userLeft
  deriving FromJson

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

structure IPCInput where
  event   : Option UpdateEvent := none
  message : Option Message := none
  date    : Option String := none
  deriving FromJson
----------------------------------------


----------------------------------------
structure SendMessageAction where
  text : String := "empty"
  replyToId : Option Nat := none 
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
  | error
  | ok
  deriving ToJson 
 
structure BotActions where
  sendMessage    : Option SendMessageAction := none 
  pinMessage     : Option PinMessageAction := none
  deleteMessage  : Option DeleteMessageAction := none
  muteUser       : Option MuteUserAction := none 
  unmuteUser     : Option UnmuteUserAction := none
  banUser        : Option BanUserAction := none 
  deriving ToJson 

structure IPCResponse where
  status : ResponseStatus
  log : Option IPCLog := none
  actions : Option BotActions := none
  deriving ToJson 
----------------------------------------


def handleCommand (jsonStr : String) : String :=
  match Json.parse jsonStr >>= fromJson? with
  | .error e => 
    let resp : IPCResponse := {
      log : IPCLog := {
        level : IPCLogLevel := IPCLogLevel.error
        msg : String := s!"error parsing JSON: {e}"
      }
      status : ResponseStatus := ResponseStatus.error
    }
    toJson resp |>.compress
  | .ok (input : IPCInput) => 
    match input.event with
    | UpdateEvent.messageSent =>
      match input.message.bind (·.text) with
      | "/start" =>
        let resp : IPCResponse := {
          status : ResponseStatus := ResponseStatus.ok,
          log : IPCLog := {
            level : IPCLogLevel := IPCLogLevel.info
            msg : String := "/start command received"
          }
          actions : BotActions := {
            sendMessage : SendMessageAction := {
                text := "Бот работает",
                replyToId := input.message.bind (·.messageId)
            }
          }
        }
        toJson resp |>.compress
      | _ =>
        let resp : IPCResponse := {
          log : IPCLog := {
            level : IPCLogLevel := IPCLogLevel.debug
            msg : String := "Message unknown"
          }
          status := ResponseStatus.error 
        }
        toJson resp |>.compress
    | _ =>
      let resp : IPCResponse := {
          log : IPCLog := {
            level : IPCLogLevel := IPCLogLevel.warning
            msg : String := "Unknown event type received"
          }
          status := ResponseStatus.error 
      }
      toJson resp |>.compress

end UaruKernel
