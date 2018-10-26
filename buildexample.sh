#!/bin/bash
set -e

# Builds the test workflow binary and makes a .alfredworkflow from the skeleton

stack build

file=$(stack path --local-install-root)
file+="/bin/footman"

cp -f "$file" "./testworkflow/footman"
zip -r -X -j footman.alfredworkflow testworkflow


