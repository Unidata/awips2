#!/awips2/python/bin/python

# Updates all purge xmls.
# Where versionsToKeep is 0, replace with null (by deleting the XML element)
# New behavior of 0 is to purge all data. null now has the old behavior of
# 0, which was to ignore versionsToKeep
#
# Author: tgurney

from __future__ import print_function

import os
import shutil
import sys
import subprocess
import traceback
import xml.etree.ElementTree as ET

prefix = '/awips2/edex/data/utility/common_static/'

def main():
    print("INFO: Starting update of versionsToKeep in purge XMLs")
    paths = subprocess.check_output("find " + prefix + "*/*/purge -type f -regex '.*\\.xml'", shell=True)
    paths = paths.strip().split('\n')
    for path in paths:
        print('INFO: Checking ' + path[len(prefix):] + '...', end='')
        tree = ET.parse(path)
        root = tree.getroot()
        modified = False
        for rule in root:
            el = rule.find('versionsToKeep')
            if el is not None:
                try:
                    if int(el.text) == 0:
                        rule.remove(el)
                        modified = True
                except Exception:
                    # not an int
                    traceback.print_exc()
        if modified:
            print('needs update.')
            try:
                shutil.copyfile(path, path + ".bak")
                print("INFO: Updating " + path)
                try:
                    tree.write(path, encoding="UTF-8", xml_declaration=True)
                except Exception:
                    traceback.print_exc()
                    print("INFO: Restoring " + path + " from backup")
                    shutil.copyfile(path + ".bak", path)
                try:
                    os.remove(path + ".bak")
                except Exception:
                    pass
            except Exception:
                traceback.print_exc()
                continue
        else:
            print('no update needed')
    print("INFO: Done.")


if __name__ == '__main__':
    sys.exit(main())
