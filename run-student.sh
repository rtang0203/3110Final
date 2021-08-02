#!/bin/bash

# Convenience wrapper over [rml_game.byte].
# Runs the student's interpreter, regardless of OS.

if [[ $# -ne 3 ]]; then
    echo "Usage: ./run-student.sh <MAP> <BLUE_BOT> <RED_BOT>"
    exit 1
fi

pkill rml || true
make build && ./rml_game.byte ./rml_interpreter.byte $1 $2 $3
