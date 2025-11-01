#!/usr/bin/env bash

set -e

echo "Running session start script..."

# Check if mise is installed
if ! command -v mise &> /dev/null; then
    echo "mise not found. Installing mise..."
    cargo install cargo-binstall
    cargo binstall mise
    mise --version
    echo "mise installed successfully."
else
    echo "mise is already installed."
fi

# Run mise trust in the current directory
echo "Running mise trust..."
mise trust

# Append mise activation to CLAUDE_ENV_FILE if not already present
if [ -n "$CLAUDE_ENV_FILE" ]; then
    MISE_ACTIVATION='eval "$(mise activate bash)"'
    if ! grep -qF "$MISE_ACTIVATION" "$CLAUDE_ENV_FILE"; then
        echo "Adding mise activation to $CLAUDE_ENV_FILE..."
        echo "$MISE_ACTIVATION" >> "$CLAUDE_ENV_FILE"
        echo "mise activation added to environment file."
    else
        echo "mise activation already present in $CLAUDE_ENV_FILE."
    fi
else
    echo "Warning: CLAUDE_ENV_FILE is not set. Skipping mise activation setup."
fi

echo "Session start script completed."
