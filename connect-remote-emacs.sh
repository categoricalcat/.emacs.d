#!/bin/zsh

# Configuration
USER="${REMOTE_USER:-fufu}"
HOST="${REMOTE_HOST:-192.168.31.18}"
PORT="${REMOTE_PORT:-13245}"
SERVER_FILE="$HOME/.emacs.d/server/remote-emacs-server"

# File to edit (default to current directory)
FILE_TO_EDIT="${1:-.}"

# Cleanup function
cleanup() {
    [[ $SSH_TUNNEL ]] && kill "$SSH_TUNNEL" 2>/dev/null
}
trap cleanup INT TERM EXIT

# Create SSH tunnel
echo "Creating SSH tunnel to ${USER}@${HOST}:${PORT}..."
echo "You may be prompted for your SSH password..."

# Start SSH tunnel in background after a delay to allow authentication
ssh -f -N -L "${PORT}:localhost:${PORT}" "${USER}@${HOST}"

# Check if SSH command was successful
if [[ $? -ne 0 ]]; then
    echo "Error: Failed to create SSH tunnel"
    exit 1
fi

# Get the PID of the SSH tunnel
SSH_TUNNEL=$(pgrep -f "ssh -f -N -L ${PORT}:localhost:${PORT}")

# Verify tunnel process is running
if [[ -z "$SSH_TUNNEL" ]]; then
    echo "Error: SSH tunnel process not found"
    exit 1
fi

echo "SSH tunnel established (PID: $SSH_TUNNEL)"

# Wait a moment for tunnel to fully establish
sleep 1

# Connect with emacsclient
echo "Connecting to remote Emacs server..."
emacsclient -c -f "$SERVER_FILE" "$FILE_TO_EDIT"

# Clean up
cleanup
