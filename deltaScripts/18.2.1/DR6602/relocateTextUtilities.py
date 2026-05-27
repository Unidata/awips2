#!/awips2/python/bin/python2

##
# DR 6602 - This script will move the TextUtility scripts from 
# /awips2/edex/data/utility/cave_static/<level>/gfe/userPython/textUtilities/regular/
# to /awips2/edex/data/utility/cave_static/<level>/gfe/userPython/textUtilities/.
# This will only affect the SITE and USER levels.
##

import logging
import glob
import os.path
import shutil
import subprocess

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s', 
                    datefmt='%Y-%m-%d %H:%M:%S',
#                     level=logging.DEBUG)
                    level=logging.INFO)
log = logging.getLogger("relocateTextUtilities.py")

TEXT_UTILS_BASE_PATH = "/awips2/edex/data/utility/cave_static/{locLevel}/*/gfe/userPython/textUtilities/regular/"
TEXT_UTILS_FILE_PATTERN = "*.py"
SITE_CONFIG_PATTERN = "/awips2/edex/data/utility/common_static/site/*/gfe/config/siteConfig.py"


def relocate_text_utils(level):
    for path in glob.iglob(TEXT_UTILS_BASE_PATH.format(locLevel=level)):
        for text_utility in glob.iglob(os.path.join(path, TEXT_UTILS_FILE_PATTERN)):
            new_path = os.path.join(os.path.dirname(os.path.dirname(path)), os.path.basename(text_utility))
            log.info("Moving file [%s] to new location [%s]", text_utility, new_path)
            try:
                shutil.move(text_utility, new_path)
            except:
                log.exception("Unable to move file [%s] to new location [%s]", text_utility, new_path)
        
        log.info("Deleting tree [%s]", path)
        try:
            shutil.rmtree(path)
        except:
            log.exception("Unable to delete directory [%s]", path)

def cleanup_configured_utils():
    for path in glob.iglob(TEXT_UTILS_BASE_PATH.format(locLevel="configured")):
        log.info("Deleting tree [%s]", path)
        try:
            shutil.rmtree(path)
        except:
            log.exception("Unable to delete directory [%s]", path)

def force_regen_area_dictionary():
    for path in glob.iglob(SITE_CONFIG_PATTERN):
        log.debug("Touching file [%s]", path)
        try:
            subprocess.check_call(["touch", path])
        except subprocess.CalledProcessError as e:
            log.exception("touch command returned non-zero exit code [%d] for file [%s]", e.returncode, path)

def main():
    log.info("Starting delta script for DR #6602: Relocate SITE and USER level Text Utilities...")
    
    relocate_text_utils("site")
    cleanup_configured_utils()
    
    relocate_text_utils("user")
    
    force_regen_area_dictionary()
    
    log.info("Delta script complete.")



if __name__ == '__main__':
    main()
