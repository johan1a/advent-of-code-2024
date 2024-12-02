# Advent of code 2024

[![Build Status](https://drone.johanandersson.io/api/badges/johan1a/advent-of-code-2024/status.svg)](https://drone.johanandersson.io/johan1a/advent-of-code-2024)

## Running

Run all days:
```
sbt test
```

Run a single day:
```
sbt 'Test / testOnly *Day15*'
```

## Get input

Setup:
Log in to the Advent of code site in a browser and copy the contents of the session cookie, and save it to a file called `.session`.

Get input for day n:
```
./get_input.sh n
```

## Format code
```
sbt scalafmtAll
```
