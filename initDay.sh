#!/bin/sh

DAY="$1"
mkdir -p "day$DAY"
curl -H @session "https://adventofcode.com/2020/day/$DAY/input" > day$DAY/input
cd "day$DAY"
cabal init --exe -d "base ^>=4.14.2.0" -d attoparsec -d text -d containers
