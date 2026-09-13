# Unofficial RTA App

## Mobile trips and offline use

- **Use this trip now** selects upcoming departures from the bundled timetable,
  preserving the stops, direction and planned stopovers. It changes the working
  draft; the named saved trip changes only after Save. If any ride cannot fit
  today, the whole operation is declined instead of wrapping to a past departure.
- Working trips automatically recover from device storage, including edits to
  saved trips. Named trips remain separate templates. Damaged current-trip data
  is retained under a recovery key without clearing saved trips.
- Departure cards show scheduled times in Mountain Time. Change time opens an
  editor; stop search also matches landmarks and towns. Favorites and recent
  stops stay on the device.
- On the first online visit, wait for **Available offline**. The service worker
  precaches the app, schedule bundle, styles and images, including the shell used
  by previously unvisited shared-trip URLs. Browser storage must remain available;
  clearing site data removes downloads and trips.
- Updates install into a separate versioned cache and wait for **Update app**.
  They do not reload an open trip automatically. The build generates the cache
  version and asset list from source inputs; deploy frontend and worker together.

Relevant regression suites: `crestedbutte.TripPlanningSpec`,
`crestedbutte.e2e.MobileOfflineSpec`, `crestedbutte.e2e.PlanningFlowsSpec`, and
`crestedbutte.e2e.SavePlanPersistenceSpec`. Run relevant tests through the Scala
Metals MCP server as specified in AGENTS.md.

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
