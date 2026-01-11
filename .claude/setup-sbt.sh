#!/bin/bash
set -e

echo "========================================="
echo "Setting up SBT cache for cloud mode..."
echo "========================================="

# Ensure SBT cache directories exist
mkdir -p "$HOME/.sbt" "$HOME/.ivy2"

# Check if build.sbt exists
if [ -f "build.sbt" ]; then
  echo ""
  echo "Step 1: Initializing SBT (may take a moment)..."
  timeout 120 ./sbt "show version" 2>&1 || {
    echo "Note: Initial SBT setup timed out or failed, but cache may be partially populated"
  }

  echo ""
  echo "Step 2: Pre-downloading project dependencies..."
  timeout 180 ./sbt "update" 2>&1 || {
    echo "Note: Dependency update timed out or failed, but cache may be partially populated"
  }

  echo ""
  echo "========================================="
  echo "SBT cache initialization complete!"
  echo "========================================="
else
  echo "Warning: build.sbt not found in current directory"
fi
