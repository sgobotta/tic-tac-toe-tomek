#!/bin/bash

INPUT_SIZE=$1
FILE_NAME=''
OUTPUT_FILE="$(pwd)/output"

if [ $1 = 'large' ]
then
  FILE_NAME='A-small-practice.in'
else
  FILE_NAME='A-large-practice.in'
fi

FILE="$(pwd)/input/$FILE_NAME"

ghc -o main solution.hs && cat $FILE | ./main > output
