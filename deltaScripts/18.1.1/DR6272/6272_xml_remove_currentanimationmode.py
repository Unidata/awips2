#!/awips2/python/bin/python

# Removes "currentAnimationMode" elements and attributes from bundle xml files
# Author: tgurney

from __future__ import print_function

import os
import shutil
import sys
import subprocess
import traceback
import xml.etree.ElementTree as ET

def main():
    print("INFO: Starting update of bundle XMLs")
    paths = subprocess.check_output("find /awips2/edex/data/utility/*/*/*/bundles -type f -regex '.*\\.xml'", shell=True)
    paths = paths.strip().split('\n')
    for path in paths:
        tree = ET.parse(path)
        try:
            node = tree.getroot().find('displayList').find('displays').find('descriptor')
        except AttributeError as a:
            # one of the elements was not found
            continue
        elementToRemove = node.find('currentAnimationMode')
        if elementToRemove is not None or 'currentAnimationMode' in node.attrib:
            try:
                shutil.copyfile(path, path + ".bak")
                print("INFO: Updating " + path)
                node.attrib.pop('currentAnimationMode', None)
                if elementToRemove is not None:
                    node.remove(elementToRemove)
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
    print("INFO: Done.")


if __name__ == '__main__':
    sys.exit(main())
