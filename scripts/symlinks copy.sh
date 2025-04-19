#!/bin/bash
# This script exports a function that creates symlinks from CSV file rows.
# The CSV is expected to have a header and four columns:
# name,source,target,description

process_csv() {
  local csv_file="$1"

  # Exit immediately if file doesn't exist
  if [[ ! -f "$csv_file" ]]; then
    echo "CSV file not found: $csv_file" >&2
    return 1
  fi

  # Read CSV file line by line, skipping the header
  tail -n +2 "$csv_file" | while IFS=',' read -r name source target description; do
    # Remove surrounding quotes from description if any
    description="${description//\"/}"

    # Expand environment variables and tilde for the source and target values.
    src_expanded=$(eval echo "$source")
    tgt_expanded=$(eval echo "$target")

    echo "Processing [$name]: $description"

    # Check if both source and target contain a wildcard "*"
    if [[ "$source" == *"*" && "$target" == *"*" ]]; then
      # Assume the target ends with "/*" and remove the trailing wildcard
      tgt_dir="${tgt_expanded%/*}"
      echo "Creating symlinks for all items in [$src_expanded] to directory [$tgt_dir]"

      # Loop over each file that matches the source wildcard pattern
      for file in $src_expanded; do
        base=$(basename "$file")
        # Create the target symlink in the determined directory
        ln -sv "$file" "$tgt_dir/$base"
      done
    else
      # Standard case: create a single symlink
      echo "Linking [$src_expanded] -> [$tgt_expanded]"
      ln -sv "$src_expanded" "$tgt_expanded"
    fi

  done

  return 0
}

# Export the function so that it can be used in sub-shells or sourced elsewhere.
export -f process_csv
