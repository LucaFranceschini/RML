#!/usr/bin/env bash

# specify monitor alias path for modules imported by the spec (like deep_subdict)
here="$PWD"/`dirname "$0"`
swipl -p monitor="$here" "$here"/monitor.pl -- "$@"
