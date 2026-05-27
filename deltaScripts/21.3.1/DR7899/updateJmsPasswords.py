#!/awips2/python/bin/python3

# Delta script that creates a passwords.properties file which is used to encrypt/decrypt the 
# Java keystores that are used when connecting to the QPID broker.
#
# This delta script will:
#  1. Generate a new passwords.properties file.
#  2. For each location it is needed copy the new file to that location.
#  3. Delete the existing .jks files to force them to be created the next time EDEX or QPID
#     is started.
#
# Author: dgilling

import argparse
import getpass
import glob
import grp
import logging
import pwd
import os
import random
import shutil
import string
import sys
import tempfile

from ufpy import jms_password


DR_NUMBER = "7899"

PASSWORD_LENGTH = 16

UPDATED_CONFIG_FILE = None

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger("updateJmsPasswords.py")



def processDir(directory):
    log.info(f"Processing directory: {directory}")

    backup_dir = f"{os.path.dirname(directory)}_bak_{DR_NUMBER}"
    log.info(f"Saving backup of {directory} to {backup_dir}")
    try:
        shutil.copytree(directory, backup_dir)
    except FileExistsError:
        log.warn(f"Delta script appears to have already been run before for path [{directory}].")
        return True
    except (OSError, shutil.Error):
        log.exception(f"Could not create backup for directory [{directory}]")
        return True

    (user, group) = get_owner_group(directory)
    log.info(f"Setting owner of {backup_dir} to '{user}:{group}'")
    shutil.chown(backup_dir, user, group)
    
    # copy in new passwords.properties
    new_config_file = os.path.join(directory, "passwords.properties")
    log.debug(f"Creating new passwords file [{new_config_file}]")
    shutil.copy(UPDATED_CONFIG_FILE, new_config_file)
    log.info(f"Setting owner of {new_config_file} to '{user}:{group}'")
    shutil.chown(new_config_file, user, group)

    # delete *.jks files
    files_to_delete = os.path.join(directory, "*.jks")
    for f in glob.glob(files_to_delete):
        log.debug(f"Removing key store file [{f}]")
        os.remove(f)

    return False

def get_owner_group(path):
    stat_info = os.stat(path)
    owner = pwd.getpwuid(stat_info.st_uid).pw_name
    group = grp.getgrgid(stat_info.st_gid).gr_name
    return owner, group

def update_password():
    new_password = ''.join(random.SystemRandom().choice(string.ascii_letters + string.digits + string.punctuation) for _ in range(PASSWORD_LENGTH))

    try:
        encrypted_password = jms_password.encryptJMSPassword(new_password)
    except jms_password.JmsConfigurationException:
        log.exception("Could not encrypt new password.")
        return True
    
    try:
        with tempfile.NamedTemporaryFile(mode="w", prefix="passwords", suffix="properties", delete=False) as temp_file:
            global UPDATED_CONFIG_FILE
            UPDATED_CONFIG_FILE = temp_file.name
            line = f'a2.jms.connection.password={encrypted_password}\n'
            temp_file.write(line)
    except IOError:
        log.exception(f"Failed to save updated password to {UPDATED_CONFIG_FILE}.")
        return True

    return False

def process_args():
    DEFAULT_CONFIG_PATHS = ["/awips2/edex/conf/jms/auth/", "/home/*/.qpid/"]
    DESCRIPTION= (
                    "Delta script that creates a passwords.properties file which is used to encrypt/decrypt "
                    "the Java keystores that are used when connecting to the QPID broker."
                )

    parser = argparse.ArgumentParser(prog="updateJmsPasswords.py", description=DESCRIPTION)
    parser.add_argument("paths",
                    nargs="*",
                    default=DEFAULT_CONFIG_PATHS,
                    metavar="PATH",
                    help="specify paths for delta script to update.")
    return parser.parse_args()


def main():
    log.info(f"Running delta script for RODO DR {DR_NUMBER}...")
    status = False

    args = process_args()
    log.debug(f"Command-line args: {args}")

    status = update_password()
    if not status:
        searchPaths = args.paths
        for sp in searchPaths:
            paths = glob.glob(sp)
            for path in paths:
                if os.path.isdir(path):
                    status |= processDir(path)

    if status:
        log.error(f"delta script for RODO {DR_NUMBER} complete with errors")
    else:
        log.info(f"delta script for RODO {DR_NUMBER} complete")
    
    return status



if __name__ == '__main__':
    sys.exit(1 if main() else 0)
