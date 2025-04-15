#!/bin/bash

# === Configuration ===
# Specify the input CSV file here or pass it as the first argument
DEFAULT_CSV_FILE="symlinks.csv"
INPUT_CSV_FILE="${1:-$DEFAULT_CSV_FILE}"

# === Script Logic ===

function symlink_all() {
  # Function to display errors and exit
  error_exit() {
    echo "Error | $1" >&2
    exit 1
  }

  # 1. Check if input CSV file exists and is readable
  if [ ! -f "$INPUT_CSV_FILE" ]; then
    error_exit "Input CSV file not found | '$INPUT_CSV_FILE'"
  fi
  if [ ! -r "$INPUT_CSV_FILE" ]; then
    error_exit "Input CSV file not readable | '$INPUT_CSV_FILE'"
  fi

  echo "Starting symlink creation process from '$INPUT_CSV_FILE'..."
  echo "--------------------------------------------------"

  # 2. Read the CSV file line by line, skipping the header
  #    Uses tail to skip header, and process substitution with while/read loop
  #    Sets IFS to comma for parsing. -r prevents backslash interpretation.
  tail -n +2 "$INPUT_CSV_FILE" | while IFS=, read -r name source target description; do
    # Trim potential surrounding quotes and whitespace from fields.
    # This handles the specific format shown where quotes are used for fields with spaces.
    name=$(echo "$name" | sed -e 's/^[[:space:]"]*//' -e 's/[[:space:]"]*$//')
    source_path=$(echo "$source_path" | sed "s|\$DIR_EMACS|$DIR_EMACS|g")
    target_path=$(echo "$target_path" | sed "s|\$DIR_EMACS|$DIR_EMACS|g")

    echo "Processing Entry | '$name'"

    # Validate that source and target paths are not empty after trimming
    if [ -z "$source_path" ] || [ -z "$target_path" ]; then
      echo "  Skipping '$name' | Source or target path is empty."
      echo "--------------------------------------------------"
      continue
    fi

    echo "  Source | '$source_path'"
    echo "  Target | '$target_path'"

    # 3. Ensure the directory for the target link exists
    target_dir=$(dirname "$target_path")
    if [ ! -d "$target_dir" ]; then
      echo "  Target directory '$target_dir' does not exist. Creating..."
      mkdir -p "$target_dir"
      if [ $? -ne 0 ]; then
        echo "  Error | Failed to create target directory '$target_dir'. Skipping '$name'."
        echo "--------------------------------------------------"
        continue
      fi
      echo "  Target directory created."
    fi

    # 4. Check if the target path already exists (as file, directory, or link)
    if [ -e "$target_path" ] || [ -L "$target_path" ]; then
      echo "  Warning | Target '$target_path' already exists. Removing it first."
      rm -rf "$target_path" # Use rm -rf cautiously; ensures existing item is removed
      if [ $? -ne 0 ]; then
        echo "  Error | Failed to remove existing target '$target_path'. Skipping '$name'."
        echo "--------------------------------------------------"
        continue
      fi
      echo "  Existing target removed."
    fi

    # 5. Create the symbolic link
    echo "  Creating symlink | '$name'"
    ln -sfv "$source_path" "$target_path"

    # 6. Verify link creation and report status
    if [ $? -eq 0 ]; then
      echo "  Success | Symlink created for '$name'."
    else
      echo "  Error | Failed to create symlink for '$name'. Check permissions or if source exists."
      # Add a check if the source exists for better error diagnosis
      if [ ! -e "$source_path" ] && [ ! -L "$source_path" ]; then
        echo "  Diagnosis | Source '$source_path' does not seem to exist."
      fi
    fi
    echo "--------------------------------------------------"

  done

  echo "Symlink creation process finished."
  exit 0
}
