let run source =
    print_endline source; Ok ()

let report line where message =
    Printf.printf "[line %i] Error %s: %s" line where message

let error line message =
    report line "" message
