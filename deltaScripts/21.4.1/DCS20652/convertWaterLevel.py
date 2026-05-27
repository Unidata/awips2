# THIS SCRIPT SHOULD BE RUN BY FOCAL POINT, NOT AUTOMATICALLY AT INSTALL.
#
# This script capitalize the 1st/city and 2nd/county columns in file Water_Level_Stations.txt
# Run this script in /awips2/edex/data/utility/common_static/[site/user]/XXX/psh/setup
# Usage: python convertWaterLevel.py

import csv
import os
from datetime import datetime
from getpass import getuser
from pathlib import Path

# Verify user is should be awips
if getuser() != "awips":
    print("This script should be run as the awips user")
    exit()

FILE_NAME='Water_Level_Stations.txt'
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

#read file
with open(newFileName,mode='r') as f:
    reader = csv.reader(f, delimiter='|')
    rows=[];
    for row in reader:
        row[0] = row[0].title()
        row[1] = row[1].title()
        rows.append('|'.join(row) + '\n')

#write to file
with open(FILE_NAME, mode='w') as w:
  w.writelines(rows)

print ("City(column 1) and county(column 2) names in file "+FILE_NAME+" have been capitalized. Please verify that they are correct and edit if necessary.")

