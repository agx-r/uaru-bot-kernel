import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer

open Lean Json ToJson FromJson

namespace UaruKernel

--- тгшная хуйня

-- юзер
structure User where
  userId     : Option Nat := none
  firstName  : Option String := none
  lastName   : Option String := none
  username   : Option String := none
  isAsshole  : Option Bool := none
  isGuest    : Option Bool := none
  deriving FromJson

-- мсг
structure Message where
  messageId      : Option Nat := none
  fromUser       : Option User := none
  text           : Option String := none
  replyToMessage : Option Message := none
  isImage        : Option Bool := none
  isFile         : Option Bool := none
  isSticker      : Option Bool := none
  deriving FromJson

--- инпут собсна
structure IPCInput where
  event   : Option String := none
  message : Option Message := none
  deriving FromJson

--- аутпут собсна
structure IPCResponse where
  status : String
  output : String
  deriving ToJson 

def handleCommand (jsonStr : String) : String :=
  match Json.parse jsonStr >>= fromJson? with
  | .ok (input : IPCInput) =>
    if input.event == some "message_sent" && input.message.bind (·.text) == some "/start" then
      "bot working"
    else
      "unknown command"
  | .error e => s!"error parsing JSON: {e}"

end UaruKernel
