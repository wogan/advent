#!/bin/bash

# fail if SESSION_COOKIE environment variable is not set
if [ -z "$SESSION_COOKIE" ]; then
  echo "SESSION_COOKIE environment variable is not set"
  exit 1
fi

# Use the current day of the month if $1 is empty
DAY=${1:-$(date +'%d')}

# Use the current date to get the YEAR if $2 is empty
YEAR=${2:-$(date +'%Y')}

# Ensure DAY is a number between 1 and 25 inclusive
if ! [[ "$DAY" =~ ^[0-9]+$ ]] || [ "$DAY" -lt 1 ] || [ "$DAY" -gt 25 ]; then
  echo "Invalid day number: $DAY"
  exit 1
fi

# Ensure YEAR is a number between 2015 and the current year inclusive
if ! [[ "$YEAR" =~ ^[0-9]+$ ]] || [ "$YEAR" -lt 2015 ] || [ "$YEAR" -gt "$(date +'%Y')" ]; then
  echo "Invalid year: $YEAR"
  exit 1
fi

OUTFILE=$(printf "input/%04d/day%02d.txt" "$YEAR" "$DAY")
if curl "https://adventofcode.com/$YEAR/day/$DAY/input" -sSf --create-dirs -H "cookie: session=$SESSION_COOKIE" -o "$OUTFILE"
then
  echo "Saved input for $YEAR day $DAY to $OUTFILE"
else
  exit 1
fi
