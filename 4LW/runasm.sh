#!/bin/bash
fname=$1
cd ../programs
binary=$(python3 ../assembler/assembler.py $fname)
cd ../4LW
stack exec 4LW -- <(echo $binary) -T tape
