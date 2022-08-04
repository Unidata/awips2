#!/awips2/python/bin/python
import glob
import logging
import os
import shutil
import sys

ROOT_PATH = "/awips2/edex/data/utility/cave_static/*/*"

TO_BE_MOVED = [
    ("alertVizAudio", "alertViz/audio"),
    ("monitorIcons","alertViz/monitorIcons"),
]

TO_BE_DELETED = [
    "alertVizIcons",
]

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("moveAlertVizFiles.py")

def main():
    log.info("Running delta script for RODO DR 7512...")
    status = 0
    
    rootPaths = glob.glob(ROOT_PATH)
    for root in rootPaths:
        for oldPath, newPath in TO_BE_MOVED:
            try:
                src = os.path.join(root, oldPath)
                if os.path.isdir(src):
                    dest = os.path.join(root, newPath)
                    if not os.path.exists(dest):
                        log.info("Moving directory %s to %s", src, dest)
                        shutil.move(src, dest)
                    elif os.path.isdir(dest):
                        log.info("Moving contents of directory %s to %s", src, dest)
                        for path in glob.glob(os.path.join(src, "*")):
                            shutil.move(path, dest)
                        os.rmdir(src)
                    else:
                        log.error("Unable to move %s to %s. Please perform the move manually.", src, dest)

            except:
                status = 1
                log.exception("Error moving directory %s to %s. Please perform the move manually.", src, dest)
            
        for path in TO_BE_DELETED:
            try:
                fullPath = os.path.join(root, path)
                if os.path.exists(fullPath):
                    log.info("Deleting directory %s", fullPath)
                    shutil.rmtree(fullPath)
            except:
                status = 1
                log.exception("Error deleting directory %s. Please delete it manually.", fullPath)

    if status:
        log.error("delta script for RODO DR 7512 complete with errors")    
    else:
        log.info("delta script for RODO DR 7512 complete")    
    return status

if __name__ == '__main__':
    sys.exit(main())
