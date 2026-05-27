#!/awips2/python/bin/python3
#
# This script should be run on dx3/dv3
#
# If needed you can run this script with a command line parameter to update
# additional files not located in the expected localization directories
# Example:
# updateCoordinates.py ~/procedures/*.xml
#

import copy
import glob
import logging
import os
import re
import shutil
import sys

DR_NUMBER = "7596"

CONFIG_PATHS = ["/awips2/edex/data/utility/*/*/*/perspectives/*.xml",
                "/awips2/edex/data/utility/*/*/*/procedures/*.xml",
                "/awips2/edex/data/utility/*/*/*/bundles/**/*.xml",
                ]

# Old format. NOTE <point> could have any name
#    <point>
#        <x>-96.35194867633048</x>
#        <y>41.22718655141241</y>
#        <z>NaN</z>
#    </point>
#
# Only coordinates with z=NaN will be replaced since the CoordAdapter only supports 2D coordinates
COORDINATE_RE = re.compile(r"<(\w+)>\s*<x>(.*?)</x>\s*<y>(.*?)</y>\s*<z>(.*?)</z>NaN</\1>")

# New format:
#    <point>-96.35194867633048,41.22718655141241</point>
REPLACEMENT_RE = r"<\1>\2,\3</\1>"

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.INFO)
log = logging.getLogger("updateCoordinates.py")


def processDir(directory):
    status = 0
    log.info("Processing directory: %s", directory)
    for root, directories, files in os.walk(directory, topdown=False):
        for filePath in files:
            if str(filePath).endswith(".xml"):
                fullPath = os.path.join(root, filePath)
                status |= updateFile(fullPath)
    return status


def updateFile(path):
    log.info("Processing file: %s", path)

    try:
        with open(path, 'r') as f:
            contents = f.read()
    except Exception as e:
        log.exception("Error reading file: %s", (path))

    newContents, count = COORDINATE_RE.subn(REPLACEMENT_RE, contents)

    if count:
        log.info("Updating file: %s", path)
        backupPath = f"{path}.DR{DR_NUMBER}.bak"
        if not os.path.exists(backupPath):
            shutil.copy(path, backupPath)

        try:
            with open(path, 'w') as outFile:
                outFile.write(newContents)
        except Exception:
            log.exception("Unable to update %s", path)
            return 1
    return 0


def main():
    log.info(f"Running delta script for RODO DR {DR_NUMBER}...")
    status = 0

    searchPaths = CONFIG_PATHS
    if len(sys.argv) > 1:
        searchPaths = sys.argv[1:]
    log.debug("searchPaths: %s", searchPaths)
    for sp in searchPaths:
        paths = glob.glob(sp)
        log.debug("paths: %s", paths)
        for path in paths:
            if os.path.isdir(path):
                status |= processDir(path)
            elif os.path.isfile(path):
                status |= updateFile(path)

    if status:
        log.error(f"delta script for RODO {DR_NUMBER} complete with errors")
    else:
        log.info(f"delta script for RODO {DR_NUMBER} complete")
    return status


if __name__ == '__main__':
    sys.exit(main())
