#! /bin/bash
# This script should be run on dv1 as root
# It deletes the /awips2/data symlink if present

echo "Running delta script ${0} for RODO DR 8226"

if [[ -h /awips2/data ]]; then
    rm --interactive=never /awips2/data
fi

echo "Delta script ${0} complete"
