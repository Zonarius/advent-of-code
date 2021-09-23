#!/bin/sh

DAY="$1"
mkdir -p "day$DAY"
curl -H @session "https://adventofcode.com/2020/day/$DAY/input" > day$DAY/input
