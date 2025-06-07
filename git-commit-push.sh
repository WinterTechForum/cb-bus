#!/bin/bash

# Get the directory of the script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Change to the project directory
cd "$DIR"

# Add all changes
git add .

# Commit with a default message
git commit -m "Update"

# Push changes
git push 