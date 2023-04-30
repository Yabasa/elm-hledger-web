#! /bin/bash

# Taken from: https://stackoverflow.com/questions/59895/how-do-i-get-the-directory-where-a-bash-script-is-located-from-within-the-script
REPO_ROOT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )

hledger-web --serve-api --cors '*' -f "$REPO_ROOT_DIR/journals/test.journal"
