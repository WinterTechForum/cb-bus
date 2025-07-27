#!/bin/bash

# Exit on any error
set -e

echo "Starting build process..."

# Install Java if not present
if ! command -v java &> /dev/null; then
    echo "Java not found, installing OpenJDK 17..."
    apt-get update
    apt-get install -y openjdk-17-jdk
fi

# Set JAVA_HOME
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
export PATH=$JAVA_HOME/bin:$PATH

echo "Java version:"
java -version

# Install SBT if not present
if ! command -v sbt &> /dev/null; then
    echo "SBT not found, installing..."
    curl -L -o sbt.deb https://repo.scala-sbt.org/scalasbt/debian/sbt-1.9.0.deb
    dpkg -i sbt.deb || true
    apt-get install -f -y
    rm sbt.deb
fi

echo "SBT version:"
sbt --version

# Run the build
echo "Running SBT fullOptJS..."
sbt frontend/fullOptJS

echo "Build completed successfully!" 