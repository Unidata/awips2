#!/bin/bash
#
# Updates the __init__.py files under dynamicserialize/dstypes/ to include
# all contributed python packages and modules within __all__ in the packages'
# __init__.py
#

if [[ ! -n ${1} ]]
then
    echo "Usage: $(basename "$0") /path/to/dynamicserialize/"
    echo "  e.g: $(basename "$0") /awips2/python/lib/python2.7/site-packages/dynamicserialize"
    exit 1
fi

if [[ ! -d "${1}/dstypes" ]]
then
    echo "${1}/dstypes does not exist."
    exit 1
fi

echo "Updating ${1}/dstypes"

# Update __all__  for every package under dstypes
for package in `find ${1}/dstypes -name __init__.py -printf '%h '`
do
    pushd $package > /dev/null
    # find non-hidden packages
    subpackages=(`find . -maxdepth 1 -type d ! -name ".*" ! -name "__pycache__" -printf '%f\n' | sort`)

    # find non-hidden python modules
    modules=(`find . -maxdepth 1 -type f \( -name "*.py" ! -name "__init__.py" ! -name ".*" \) -printf '%f\n' | sed 's/\.py//' | sort`)

    # join subpackages and modules into a single list, modules first
    all=("${subpackages[@]}" "${modules[@]}")
    joined=$(printf ",\n            \'%s\'" "${all[@]}")

    #replace the current __all__ definition with the rebuilt __all__, which now includes all contributed packages and modules.
    #-0777 allows us to match the multi-line __all__ definition
    perl -0777 -p -i -e "s/__all__ = \[[^\]]*\]/__all__ = \[`echo \"${joined:1}\"`\n          \]/g" __init__.py

    popd > /dev/null
done

echo "Done"
