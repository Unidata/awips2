#!/bin/bash

# #5959 - Replace intValue(), toString() and friends with int(), float(), etc.
#   in non-base localization python files.
#
# Author: tgurney

if [[ "$1" == "-h" || "$1" == "--help" ]] ; then
    echo "Usage: $0 [path]..."
    echo "Update all python files at the specified path(s). Will use a default"
    echo "list of paths if no path is specified on the command line."
    exit 1
fi

my_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
file_list=$(mktemp || exit 1)
backup_tar="/awips2/edex/data/utility/5959_backup_$(date +%Y%m%d_%H%M%S).tar"

write="-w"
# uncomment for testing
#write=

echo INFO: Starting update of localization .py files

# Build list of files that need to be fixed
if [[ "$1" != "" ]]; then
    find $* -type f -name '*.py' 2>/dev/null | xargs grep -El \
        '((int|long|short|double|float)Value|toString)' >> ${file_list}
else
    while IFS='' read -r line; do
        find $line -type f -name '*.py' 2>/dev/null | xargs grep -El \
            '((int|long|short|double|float)Value|toString)' >> ${file_list}
    done << 'EOF'
/awips2/edex/data/utility/cave_static/*/*/gfe/userPython/gfeConfig/
/awips2/edex/data/utility/cave_static/*/*/gfe/userPython/procedures/
/awips2/edex/data/utility/cave_static/*/*/gfe/userPython/smartTools/
/awips2/edex/data/utility/cave_static/*/*/gfe/userPython/textProducts/
/awips2/edex/data/utility/cave_static/*/*/gfe/userPython/textUtilities/regular/
/awips2/edex/data/utility/cave_static/*/*/gfe/userPython/utilities/
/awips2/edex/data/utility/common_static/*/*/derivedParameters/functions/
/awips2/edex/data/utility/common_static/*/*/gfe/smartinit/
/awips2/edex/data/utility/common_static/*/*/gfe/textProducts/templates/product/
/awips2/edex/data/utility/common_static/*/*/gfe/textProducts/templates/utility/
EOF
fi

# Temporary install 2to3 fixer modules
for file in ${my_dir}/fix_j*.py; do
    ln -sf $file /awips2/python/lib/python2.7/lib2to3/fixes
done

# Fix the files
while IFS='' read -r line; do
    /awips2/python/bin/2to3 $write \
        -f jintvalue \
        -f jshortvalue \
        -f jlongvalue \
        -f jfloatvalue \
        -f jdoublevalue \
        -f jtostring \
        $line
    if [[ -f "${line}.bak" ]]; then
        if [[ -f "${backup_tar}" ]]; then
            tar rf "${backup_tar}" ${line}.bak
        else
            tar cf "${backup_tar}" ${line}.bak
        fi
        rm "${line}.bak"
    fi
done < ${file_list}

if [[ -f ${backup_tar} ]]; then
    gzip --fast ${backup_tar}
    echo INFO: Original files are saved at ${backup_tar}.gz
fi

# Remove temporarily installed modules
cd $my_dir
for file in fix_j*.py; do
    rm /awips2/python/lib/python2.7/lib2to3/fixes/$file
done

rm -f ${file_list}

echo INFO: Done.
