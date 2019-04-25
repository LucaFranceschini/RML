#!/usr/bin/env bash

# instrument a JavaScript file on-the-fly, run and log trace
# args:
# - log file for the trace
# - program to run
# - other args are forwarded to the program

# check args
if [ $# -lt 2 ]
	then
		echo "expected args: log-file program [programArgs ...]"
		exit 1
fi

# convert relative path to absolute
function realpath { echo $(cd $(dirname $1); pwd)/$(basename $1); }

logFile=$(realpath "$1")
program=$(realpath "$2")

# instrument & go
DIR="$PWD"/`dirname "$0"`
npm run --prefix="$DIR" --silent jalangi -- --initParam log:"$logFile" --inlineIID --inlineSource --analysis functionInvocationAnalysis.js "$program" "${@:3}"
