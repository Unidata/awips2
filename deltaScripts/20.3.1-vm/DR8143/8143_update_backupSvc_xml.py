import glob
import os
import re
import shutil
import traceback
from datetime import datetime
from xml.etree import ElementTree

# Updates the host elements in backupSvc.xml for the VM hostname changes
# Run on dv1
#
# Author: tgurney

def fix(path):
    print("INFO: Checking " + path)

    if not os.path.exists(path):
        print("INFO: " + path + " does not exist. Skipping this file")
        return

    with open(path) as f:
        document = ElementTree.parse(f)
        root = document.getroot()

    elements = root.findall('hosts')
    if len(elements) != 1:
        print("WARN: " + path + " is invalid, it does not have exactly " +
                "one <hosts> element. Skipping this file")
        return

    changed = False
    hosts = elements[0]
    for host in hosts:
        for elem in host:
            if elem.tag == 'name':
                old_text = elem.text
                elem.text = re.sub('dx[1-8]-|ec-', 'edexcluster-', elem.text)
                if elem.text != old_text:
                    changed = True

    if not changed:
        print("INFO: No update needed in " + path)
        return

    backup_path = path + '.' + datetime.strftime(datetime.now(), '%Y%m%d%H%M%S')
    print("INFO: Writing backup of " + path + " to " + backup_path)
    shutil.copyfile(path, backup_path)

    print("INFO: Writing out updated " + path)
    document.write(path)

def main():
    for path in glob.glob("/awips2/edex/data/utility/common_static/site/*/backupsvc/backupSvc.xml"):
        try:
            fix(path)
        except Exception as e:
            print("ERROR: Failed to update " + path)
            traceback.print_exc()

if __name__ == '__main__':
    main()
