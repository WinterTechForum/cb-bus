# Unofficial RTA App

## Local Development

To run this project locally:

    sbt ~frontend/fastOptJS ~sw/fastOptJS
    python3 -m http.server --directory frontend/src/main/resources
    firefox http://localhost:8000/index.html
    
In your editor, you might want to exclude `./frontend/src/main/resources/compiledJavascript`

## Production Build

To build for production:

    sbt frontend/fullOptJS sw/fullOptJS

## Deployment

This project is configured for automatic deployment on Netlify. The build process:

1. Netlify automatically runs `./netlify-build.sh` on each push
2. The build script executes `sbt frontend/fullOptJS` and `sbt sw/fullOptJS`
3. The compiled JavaScript files are generated in `frontend/src/main/resources/`
4. Netlify serves the contents of `frontend/src/main/resources/` as the static site

**Note:** The compiled JavaScript files (`main.js`, `sw-opt.js`, etc.) are now excluded from git and only exist during deployment. This keeps the repository clean and ensures builds are always fresh.
