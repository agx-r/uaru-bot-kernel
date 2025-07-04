import UaruKernel

def main (args : List String) : IO Unit :=
  match args with
  | [jsonStr] => 
    let response := UaruKernel.handleCommand jsonStr
    IO.print response
  | _ => 
    IO.print "Usage: provide a single JSON string argument"
