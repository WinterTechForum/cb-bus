# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Scala.js-based bus tracking web application for the Crested Butte RTA (Regional Transportation Authority). The project is built using SBT with two main modules:

- **frontend**: The main web application built with Laminar (reactive UI library)
- **sw**: Service worker for PWA functionality

## Build Commands

### Development
```bash
# Compile and build frontend for development (fast compilation)
sbt frontend/fastOptJS

# Full optimization build for production
sbt frontend/fullOptJS

# Compile service worker
sbt sw/fastOptJS

# Run all tests
sbt test

# Format code
sbt scalafmt
```

### Local Development Server
```bash
# Build the application
sbt fastOptJS

# Start local HTTP server
python3 -m http.server --directory frontend/src/main/resources

# Open in browser at localhost:8000
```

## Architecture

### Frontend Structure
- **Entry Point**: `MyApp.scala` - Initializes the Laminar application
- **Routing**: `RoutingStuff.scala` - Handles URL routing using Waypoint
- **UI Components**: `laminar/Components.scala` - Main UI components using Laminar
- **Bus Data**: `routes/` directory contains route definitions (RTA.scala, etc.)
- **PWA Features**: `pwa/Persistence.scala` handles local storage and offline functionality

### Key Libraries
- **Laminar**: Reactive UI framework (v16.0.0)
- **Airstream**: Reactive streams for Laminar
- **ZIO JSON**: JSON serialization (v0.6.2)
- **Waypoint**: Client-side routing (v7.0.0)
- **Animus**: Animation library (v0.6.5)

### Application Flow
1. `MyApp` renders the main app into `#landing-message` element
2. `RoutingStuff.app` handles page routing and URL parsing
3. `Components.FullApp` contains the main application logic
4. Bus schedule data is defined in `routes/` classes like `RTA.scala`

### Testing
- Tests are located in `frontend/src/test/scala/`
- Uses ZIO Test framework
- Key test files include route validation and time calculations

### Service Worker
- Separate Scala.js project in `sw/` directory
- Handles PWA caching and offline functionality
- Compiled to `sw-opt.js` in resources

## Development Notes

- JavaScript output goes to `frontend/src/main/resources/compiledJavascript/main.js`
- The project uses Scala 3.3.6
- Static assets (images, CSS) are in `frontend/src/main/resources/`
- Uses Bulma CSS framework for styling

## Special Commands

When making UI changes, always run:
```bash
sbt frontend/fastOptJS
```

This ensures compilation without needing a separate compile step.