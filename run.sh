#!/bin/bash

INPUT_SIZE=$1
FILE_NAME=''
OUTPUT_FILE="$(pwd)/output"

if [ $1 = 'small' ]
then
  FILE_NAME='A-small-practice.in'
else
  FILE_NAME='A-large-practice.in'
fi

if [ $1 = 'sample' ]
then
  FILE_NAME='sample.in'
fi

FILE="$(pwd)/input/$FILE_NAME"

ghc -o main solution.hs && cat $FILE | ./main > output
