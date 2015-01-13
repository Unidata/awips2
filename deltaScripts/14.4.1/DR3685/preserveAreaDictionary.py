#!/usr/bin/env python
# This script will preserve the site's current configured AreaDictionary.py file as 
# a site level file if one does not already exist.

AREA_DICTIONARY_PATH = "/awips2/edex/data/utility/cave_static/configured/*/gfe/userPython/textUtilities/regular/AreaDictionary.py"

import glob
import os
import shutil
import traceback

def main():
    # for each configured AreaDictionary.py file
    for configFile in glob.glob(AREA_DICTIONARY_PATH):
        siteFile = configFile.replace("cave_static/configured", "cave_static/site")
        
        # if site file does not exist
        if not os.path.exists(siteFile):
            # create site directory if necessary
            try:
                os.makedirs(os.path.dirname(siteFile))
            except OSError as e:
                import errno
                if e.errno != errno.EEXIST:
                    print "Error copying", configFile, "\n           to", siteFile, \
                          "\nPlease manually copy this file before starting EDEX"
                    traceback.print_exc()
                    continue
            
            # copy configured file to site.
            print "Preserving", siteFile
            try:
                shutil.copy(configFile, siteFile)
            except:
                print "Error copying", configFile, "\n           to", siteFile, \
                      "\nPlease manually copy this file before starting EDEX"
                traceback.print_exc()
        else:
            print "Skipping  ", configFile, "\n          ", siteFile, "exists"

if __name__ == "__main__":
    main()
