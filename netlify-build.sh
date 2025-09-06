#!/bin/bash

# Exit on error
set -e

echo "Starting Netlify build process..."

# Check if sbt is available
if ! command -v sbt &> /dev/null; then
    echo "SBT not found. Installing SBT..."
    
    # Install sbt using the included sbt-launch.jar
    echo "Using local sbt-launch.jar..."
fi

# Run the build using the local sbt script
echo "Building frontend with fullOptJS..."
./sbt 'frontend/fullOptJS'

echo "Building service worker with fullOptJS..."
./sbt 'sw/fullOptJS'

echo "Build completed successfully!"

# List the generated files for verification
echo "Generated files:"
ls -la frontend/src/main/resources/compiledJavascript/
ls -la frontend/src/main/resources/sw-opt.js*