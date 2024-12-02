#!/usr/bin/fish

set userAgent 'httpie (+https://gitlab.com/johan1a/advent-of-code-2024/)'


mkdir -p ./info

# Extract the year from the last 4 chars of the directory name.
# Assumes the project is named advent-of-code-YYYY.
set year (echo (basename $PWD) | string sub --start 16 --length 4)

set day $argv

set session (cat .session)

if [ -z $day ] ;
  echo 'Usage: ./get_input.sh x'
  exit 1
end

if [ -z $session ] ;
  echo 'Could not fetch session from .session'
  exit 1
end

if [ $day -lt "10" ];
  set paddedDay "0$day"
else
  set paddedDay $day
end

set currentDateTime (date -Is)
set minDateTime $year-12-{$paddedDay}T06:00:00+01:00

set currentDateTimeSeconds (date -d $currentDateTime +%s)
set minDateTimeSeconds (date -d $minDateTime +%s)

if [ "$currentDateTimeSeconds" -lt "$minDateTimeSeconds" ];
  echo This puzzle is not available yet. Try again in (math $minDateTimeSeconds - $currentDateTimeSeconds) seconds.
  exit 1
end

set existingInputFileSize (wc -l ./src/test/resources/day$paddedDay/input.txt | awk '{print $1}')

if [ $existingInputFileSize -eq "0" ];
  echo Downloading input...
  http "https://adventofcode.com/$year/day/$day/input" cookie:session=$session User-Agent:$userAgent > ./src/test/resources/day$paddedDay/input.txt
  echo Done
end

if [ ! -f "./info/day$paddedDay.html" ];
  echo Downloading info...
  http "https://adventofcode.com/$year/day/$day" cookie:session=$session User-Agent:$userAgent > ./info/day$paddedDay.html
  echo Done...
end

#elinks info/day$paddedDay.html

echo My work here is done... Good luck, and Ho ho ho!
