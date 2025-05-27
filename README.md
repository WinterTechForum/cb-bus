To run this project:

    sbt fastOptJs
    python3 -m http.server --directory frontend/src/main/resources
    firefox src/main/resources/html/index.html
    
In your editor, you might want to exclude ./src/main/resources/compiledJavascript
