from __future__ import print_function
import os
import traceback
from datetime import datetime
from xml.etree import ElementTree

# Update script for Omaha #7724
# Removes the jmsConnection element from comms.xml and comms_practice.xml.
# Run on the BMH server.
#
# Author: tgurney

PATHS = ('/awips2/bmh/conf/comms.xml', '/awips2/bmh/conf/comms-practice.xml')

def rewrite_comms_xml(path):
    print("INFO: Checking " + path)

    if not os.path.exists(path):
        print("INFO: " + path + " does not exist. Skipping this file")
        return

    with open(path) as f:
        document = ElementTree.parse(f)
        root = document.getroot()

    elements = root.findall('jmsConnection')
    if not elements:
        print("INFO: No update needed in " + path)
        return

    backup_path = path + '.' + datetime.strftime(datetime.now(), '%Y%m%d')
    print("INFO: Writing backup of " + path + " to " + backup_path)
    document.write(backup_path)

    print("INFO: Removing jmsConnection element from " + path)
    for e in elements:
        root.remove(e)

    print("INFO: Writing out updated " + path)
    document.write(path)

def main():
    for path in PATHS:
        try:
            rewrite_comms_xml(path)
        except Exception as e:
            print("ERROR: Failed to update " + path)
            traceback.print_exc()

if __name__ == '__main__':
    main()
