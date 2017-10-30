#!/bin/bash

# tester5.sh - Revised version of tester.sh for HW #5
#
# This script will flex whatever .l file(s) and bison whatever
# .y file(s) are in the directory from which it is run. It will
# then g++ the resulting .tab.c file(s) creating an executable
# named a.out
#
# Then it will run a.out on every file in a directory named
# sample_input writing the output to a directory named
# actual_output
#
# DO NOT RUN THIS ON INPUT FILES THAT EXPECT USER INPUT!!!
# REMOVE THEM FROM THE SAMPLE_INPUT DIRECTORY BEFORE RUNNING
# THIS. OTHERWISE, THIS WILL JUST APPEAR TO 'HANG' BECAUSE IT
# IS WAITING FOR USER INPUT!!!
# ALSO, THERE IS NO CORRESPONDING FILE IN THE EXPECTED_OUTPUT
# DIRECTORY FOR THE SAMPLE_INPUT FILES THAT REQUIRE USER INPUT!
#
# Finally, it will do a diff on each file in actual_output
# against the file with the same name that is in a directory
# named expected_output and report the differences in a
# directory named reports
#
# To run this script, do the following:
# (1) Make sure you do NOT have a directory named actual_output
# (2) Make sure you do NOT have a directory named reports
# (3) Make sure you have a directory named sample_input that
#     contains ONLY the sample input files you want to test
# (4) Make sure you have a directory named expected_output
#     that contains ONLY the expected output files, one for each
#     of the files in the sample_input directory
# (5) Make sure you have ONLY a single .l file in this directory
# (6) Make sure you have ONLY a single .y file in this directory
# (7) bash tester.sh

# If you make any changes to this script in Windows, you must
# run dos2unix on it before running it with bash
# The following is also equivalent to running dos2unix on this
# file to get rid of the \r's:
# perl -i -p -e "s/\r//" tester5.sh

mkdir ./actual_output
mkdir ./reports
rm ./*.tab.c
rm ./a.out

flex *.l
bison *.y
g++ *.tab.c -o a.out

inputs=`ls sample_input --ignore-backups`
for i in $inputs; do
  ./a.out ./sample_input/$i > ./actual_output/$i.out

  diff ./actual_output/$i.out ./expected_output/$i.out > \
       ./reports/$i --ignore-space-change --ignore-case \
       --side-by-side

  lines=`diff ./actual_output/$i.out ./expected_output/$i.out \
         --ignore-space-change --ignore-case  | wc --lines`
  echo $lines unexpected lines in $i.out \
       >> ./reports/summary.txt
 done

 echo >> ./reports/summary.txt
