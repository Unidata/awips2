# THIS SCRIPT SHOULD BE RUN BY FOCAL POINT, NOT AUTOMATICALLY AT INSTALL.
#
# This script capitalize name and county attributes in file cities.xml
# Run this script in /awips2/edex/data/utility/common_static/[site/user]/XXX/psh/setup
# Usage: python convertCities.py

from datetime import datetime
import xml.etree.ElementTree as ET
import os
from getpass import getuser
from pathlib import Path

# Verify user is should be awips
if getuser() != "awips":
    print("This script should be run as the awips user")
    exit()

FILE_NAME='cities.xml'

# Verify file exist
my_file = Path(FILE_NAME)
if not my_file.is_file():
    print("File " + FILE_NAME + " does not exist in current directory.")
    exit()

# backup file
today = datetime.today()
dayStr = today.strftime("_BACKUP_%y%m%d%H%M%S")
newFileName=FILE_NAME+dayStr
os.rename(FILE_NAME,newFileName)
print ("Back up file " + FILE_NAME + " to "+newFileName)

tree = ET.parse(newFileName)
root = tree.getroot()
for item in root.findall('City'):
  value = item.get('Name').title()
  item.set('Name',value)
  value = item.get('County').title()
  item.set('County',value)

tree.write(FILE_NAME)

print ("Name and county attributes in file "+FILE_NAME+" have been capitalized. Please verify that they are correct and edit if necessary.")


