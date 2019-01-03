#!/usr/bin/env bash

# instrument a JavaScript file on-the-fly, run and log trace
# args:
# - log file for the trace
# - program to run
# - other args are forwarded to the program

# check args
if [ $# -ne 2 ]
	then
		echo "expected args: log-file program [programArgs ...]"
		exit 1
fi

# instrument & go
DIR="$PWD"/`dirname "$0"`
npm run --prefix="$DIR" --silent jalangi -- --initParam log:"$PWD/$1" --inlineIID --inlineSource --analysis functionInvocationAnalysis.js "$PWD/$2" "${@:3}"
