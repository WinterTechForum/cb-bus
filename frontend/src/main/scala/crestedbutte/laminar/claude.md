When confirming any UI changes in the application, this command should be executed:

    sbt frontend/fastOptJS

It will ensure that things compile without needing to first do a separate

    sbt frontend/compile command
