#!/usr/bin/env bash

# top-level test dir (directory of this script)
# https://stackoverflow.com/a/246128/1202636
testsDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# generate files here
buildDir="$testsDir/build"

projectDir="$testsDir/../.."
compiler="$projectDir/compiler/build/libs/rml-compiler.jar"
monitor="$projectDir/monitor/monitor.sh"

# test cases (all dirs except build dir)
for caseDir in `find "$testsDir"/* -type d | grep -v "$buildDir"` ;
do
	# test case name
	caseName=`basename "$caseDir"`
    
    # make a directory for files generated in this test case
    caseBuildDir="$buildDir"/"$caseName"
    mkdir -p "$caseBuildDir"
    cd "$caseBuildDir"
    
    # compile RML to Prolog
    java -jar "$compiler" <"$caseDir"/spec.rml >spec.pl
    
    # good.log should be accepted
    echo -n "$caseName-good... "
    "$monitor" spec.pl "$caseDir"/good.log --silent
    if [[ $? == 0 ]]; then
    	echo "done"
    else
    	echo "FAIL"
    fi
    
    # bad.log should be rejected
    echo -n "$caseName-bad... "
    "$monitor" spec.pl "$caseDir"/bad.log --silent
    if [[ $? == 1 ]]; then
    	echo "done"
    else
    	echo "FAIL"
    fi
done

# go back to the starting dir
cd "$PWD"
