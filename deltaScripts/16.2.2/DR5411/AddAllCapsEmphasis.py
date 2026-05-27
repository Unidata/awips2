#!/usr/bin/env python
# Delta script to insert productId tag into warngen .vm and .xml files
# Original files will be backed up in *.backup[n]
# 

import glob
import os
import re
import shutil

SITE_TEMPLATE_PATH = "/awips2/edex/data/utility/common_static/site/*/warngen/*.vm"

PATTERNS = [
   re.compile(r'\bTAKE\s+COVER\s+NOW\b', re.IGNORECASE | re.MULTILINE),
   re.compile(r'\bSEEK\s+SHELTER\s+NOW\b', re.IGNORECASE | re.MULTILINE),
   re.compile(r'\bSEEK\s+SHELTER\s+IMMEDIATELY\b', re.IGNORECASE | re.MULTILINE),

   re.compile(r'\bIMMINENT\s+DANGEROUS\s+WEATHER\s+CONDITIONS\b', re.IGNORECASE | re.MULTILINE),
   re.compile(r'\bIMMINENT,\s+DANGEROUS,\s+AND\s+POTENTIALLY\s+LIFE-THREATENING\s+WEATHER\s+CONDITIONS\b', re.IGNORECASE | re.MULTILINE),
   re.compile(r'\b((EXTREMELY|VERY|PARTICULARLY)\s+)?DANGEROUS\s+SITUATION\b', re.IGNORECASE | re.MULTILINE),
]

def __backupFile(orig):
    """create a unique backup file name from orig"""

    suffix = ".backup"
    backup = orig + suffix
    count = 0
    while os.path.exists(backup):
        count += 1
        backup = orig + suffix + str(count)
    
    return backup

def __replaceWithUpper(matchobj):
    orig = matchobj.group(0)
    repl = orig.upper()
    return repl

def main():

    for template in glob.glob(SITE_TEMPLATE_PATH):
        with open(template, "r") as templateFile:
            originalContents = templateFile.read()

        newContents = originalContents
        for pattern in PATTERNS:
            newContents = re.sub(pattern, __replaceWithUpper, newContents)
        
        if newContents != originalContents:
            print "Updating", template
            shutil.copy2(template, __backupFile(template))
            with open(template, "w") as templateFile:
                templateFile.write(newContents)

if __name__ == "__main__":
    main()
