#!/bin/zsh
# This script defines a function that creates symlinks from CSV file rows.
# The CSV is expected to have a header and four columns:
# name,source,target,description

# Set default value for DIR_EMACS if not defined
: "${DIR_EMACS:=$HOME/.emacs.d}"

symlink_all() {
  local csv_file="$1"

  # Exit immediately if file doesn't exist
  if [[ ! -f "$csv_file" ]]; then
    echo "CSV file not found: $csv_file" >&2
    return 1
  fi

  local errors_occurred=0

  # Read CSV file line by line, skipping the header
  # Using process substitution to avoid subshell issues with return/variable scope
  while IFS=',' read -r name source target description || [[ -n "$name" ]]; do # Handle last line without newline
    # Remove surrounding quotes from description if any
    description="${description//\"/}"

    # Use eval echo for robust tilde and variable expansion
    src_expanded=$(eval echo "$source")
    tgt_expanded=$(eval echo "$target")

    echo "Processing [$name]: $description"

    # Check if source contains a wildcard "*"
    if [[ "$source" == *"*"* ]]; then
      # For wildcard sources, the target is the directory itself
      tgt_dir="$tgt_expanded"

      # Ensure target directory exists, create if not
      if [[ ! -d "$tgt_dir" ]]; then
        echo "Target directory does not exist, creating: $tgt_dir" >&2
        if ! mkdir -p "$tgt_dir"; then
          echo "Failed to create target directory: $tgt_dir" >&2
          errors_occurred=1
          continue
        fi
      fi

      echo "Creating symlinks for items matching [$src_expanded] in directory [$tgt_dir]"

      # Use nullglob and loop safely over potentially multiple sources
      local file_found=0
      setopt nullglob               # Make globs expand to nothing if no match
      # Split the expanded source string into an array using spaces as delimiters
      local files_to_link=("${(@s: :)src_expanded}")

      for file in "${files_to_link[@]}"; do # Iterate over array elements
        file_found=1
        # Trim potential leading/trailing whitespace from element
        file="${file## ##}"
        file="${file%% ##}"
        if [[ -z "$file" ]]; then # Skip empty elements resulting from split
          continue
        fi

        if [[ ! -e "$file" ]]; then
          # This check is important as glob expansion might yield non-existent paths
          # if the source pattern included relative paths or complex patterns evaluated by eval.
          echo "Source file/directory not found: [$file]" >&2 # Add brackets for clarity
          errors_occurred=1
          continue
        fi

        base=$(basename "$file")
        target_link="$tgt_dir/$base"

        # Create/update the target symlink, force overwrite
        echo "Linking [$file] -> [$target_link]"
        if ln -svf "$file" "$target_link"; then
          echo "Symlink created/updated successfully"
        else
          echo "Failed to create symlink from [$file] to [$target_link]" >&2
          errors_occurred=1
        fi
      done
      unsetopt nullglob # Disable nullglob

      if [[ $file_found -eq 0 ]]; then
        echo "Warning: No files found matching source pattern: $src_expanded" >&2
      fi

    else
      # Standard case: create a single symlink

      # Ensure parent directory of target exists
      tgt_dir=$(dirname "$tgt_expanded")
      if [[ ! -d "$tgt_dir" ]]; then
        echo "Target directory does not exist, creating: $tgt_dir" >&2
        if ! mkdir -p "$tgt_dir"; then
          echo "Failed to create target directory: $tgt_dir" >&2
          errors_occurred=1
          continue
        fi
      fi

      # Check if source exists
      if [[ ! -e "$src_expanded" ]]; then
        echo "Source does not exist: $src_expanded" >&2
        errors_occurred=1
        continue
      fi

      # Create/update the target symlink, force overwrite
      echo "Linking [$src_expanded] -> [$tgt_expanded]"
      if ln -svf "$src_expanded" "$tgt_expanded"; then
        echo "Symlink created/updated successfully"
      else
        echo "Failed to create symlink from [$src_expanded] to [$tgt_expanded]" >&2
        errors_occurred=1
      fi
    fi

  done < <(tail -n +2 "$csv_file") # Use process substitution here

  return $errors_occurred
}

# Remove the export -f line
# The function is available because the script is sourced by run.sh
