#!/bin/bash

file_name="Glados"

# Find all instances of the file and store them in an array
file_paths=($(find . -type f -name "$file_name"))

# Check if the array is not empty
if [ ${#file_paths[@]} -gt 0 ]; then
    echo "File $file_name exists in the following locations:"
    # Print each path from the array
    for path in "${file_paths[@]}"; do
        echo "$path"
    done
else
    echo "File $file_name does not exist in the current directory or its subdirectories."
fi