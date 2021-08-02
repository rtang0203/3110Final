#!/bin/bash

# Convenience wrapper over [rml_game.byte].
# Uses the correct staff bytecode interpreter depending on the user's OS.
# Note: only supports MacOS and Linux.

if [[ $# -ne 3 ]]; then
    echo "Usage: ./run-staff.sh <MAP> <BLUE_BOT> <RED_BOT>"
    exit 1
fi

pkill rml || true
make build && case "$(uname -s)" in
   Darwin)
     ocamlrun rml_game.byte ./exe/rml_interpreter_mac.byte $1 $2 $3
   ;;
   Linux)
    ocamlrun rml_game.byte ./exe/rml_interpreter_linux.byte $1 $2 $3
   ;;
   *)
    echo 'Unsupported OS'
    exit 1
esac
