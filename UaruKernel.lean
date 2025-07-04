import Lean

namespace UaruKernel

structure Command where
  action : String
  subcommand : String
  deriving BEq

def handleCommand (cmd : Command) : String :=
  if cmd.action == "message_sent" && cmd.subcommand == "/start" then
    "bot working"
  else
    "unknown command"

end UaruKernel 
