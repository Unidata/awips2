# THIS SCRIPT SHOULD BE RUN BY FOCAL POINT, NOT AUTOMATICALLY AT INSTALL.
#
# This script capitalize FullName and Name elements in marine_stationinfo.xml, metar_stationinfo.xml, or non_metar_stationinfo.xml
# Run this script in /awips2/edex/data/utility/common_static/[site/user]/XXX/psh/setup
# Usage: python convertStations.py

from datetime import datetime
import xml.etree.ElementTree as ET
import os
from getpass import getuser
from pathlib import Path

# Verify user is should be awips
if getuser() != "awips":
    print("This script should be run as the awips user")
    exit()

MARINE_FILE='marine_stationinfo.xml'
METAR_FILE='metar_stationinfo.xml'
NONMETAR_FILE='non_metar_stationinfo.xml'

# backup file
today = datetime.today()
dayStr = today.strftime("_BACKUP_%y%m%d%H%M%S")

def convert(FILE_NAME):
# Verify file exist
  my_file = Path(FILE_NAME)
  if not my_file.is_file():
      print("File " + FILE_NAME + " does not exist in current directory.")
      return

  newFileName=FILE_NAME+dayStr
  os.rename(FILE_NAME,newFileName)
  print ("Back up file " + FILE_NAME + " to "+newFileName)

  tree = ET.parse(newFileName)
  root = tree.getroot()
  for item in root.findall('Station/FullName'):
    value = item.text.title()
    item.text=value
  for item in root.findall('Station/Name'):
    value = item.text.title()
    item.text=value

  tree.write(FILE_NAME)

  print ("Name and county attributes in file "+FILE_NAME+" have been capitalized. Please verify that they are correct and edit if necessary.")

convert(MARINE_FILE)
print("\n")
convert(METAR_FILE)
print("\n")
convert(NONMETAR_FILE)

