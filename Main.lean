import UaruKernel
import Lean

def parseArgs (args : List String) : Option UaruKernel.Command :=
  match args with
  | [action, subcommand] => some { action := action, subcommand := subcommand }
  | _ => none

def main (args : List String) : IO Unit := do
  match parseArgs args with
  | some cmd =>
    let response := UaruKernel.handleCommand cmd
    IO.print response
  | none =>
    IO.print "error: expected two arguments (action and subcommand)"
