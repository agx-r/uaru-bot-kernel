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

--- Возможные status:
-- unknown error
-- ok
structure IPCResponse where
  status         : String := "unknown error"
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
    match input.event, input.message.bind (fun x => x.text) with
    | some "message sent", some "/start" =>
        let resp : IPCResponse := {
          status := "ok",
          sendMessage := some {
            text := "бот работает",
            replyToId := input.message.bind (fun x => x.messageId)
          }
        }
        toJson resp |>.compress
    | some "message sent", some "/mute" =>
        match input.message.bind (fun x => x.replyToMessage) >>= (fun x => x.fromUser) >>= (fun x => x.userId) with
        | some uid =>
            let resp : IPCResponse := {
              status := "ok",
              muteUser := some {
                userId := uid,
                duration := 240
              }
            }
            toJson resp |>.compress
        | none => toJson { status := "cannot mute (no userId)" : IPCResponse } |>.compress
    | some "message sent", some "/unmute" =>
        match input.message.bind (fun x => x.replyToMessage) >>= (fun x => x.fromUser) >>= (fun x => x.userId) with
        | some uid =>
            let resp : IPCResponse := {
              status := "ok",
              unmuteUser := some {
                userId := uid
              }
            }
            toJson resp |>.compress
        | none => toJson { status := "cannot unmute (no userId)" : IPCResponse } |>.compress
    | some "message sent", some "/ban" =>
        match input.message.bind (fun x => x.replyToMessage) >>= (fun x => x.fromUser) >>= (fun x => x.userId) with
        | some uid =>
            let resp : IPCResponse := {
              status := "ok",
              banUser := some { userId := uid }
            }
            toJson resp |>.compress
        | none => toJson { status := "cannot ban (no userId)" : IPCResponse } |>.compress
    | some "message sent", some "/pin" =>
        match input.message.bind (fun x => x.replyToMessage) >>= (fun x => x.messageId) with
        | some mid =>
            let resp : IPCResponse := {
              status := "ok",
              pinMessage := some { messageId := mid }
            }
            toJson resp |>.compress
        | none => toJson { status := "cannot pin (no messageId)" : IPCResponse } |>.compress
    | some "message sent", some "/delete" =>
        match input.message.bind (fun x => x.replyToMessage) >>= (fun x => x.messageId) with
        | some mid =>
            let resp : IPCResponse := {
              status := "ok",
              deleteMessage := some { messageId := mid }
            }
            toJson resp |>.compress
        | none => toJson { status := "cannot delete (no messageId)" : IPCResponse } |>.compress
    | some "message sent", some unknownCmd =>
        let resp : IPCResponse := {
          status := s!"unknown command: {unknownCmd}"
        }
        toJson resp |>.compress
    | _, _ =>
        let resp : IPCResponse := {
          status := "unhandled input"
        }
        toJson resp |>.compress

end UaruKernel
