#!/awips2/python/bin/python
# Adds the gfe/combinations permission to user ALL for all site level userRoles.xml files 

USER_ROLES_PATH = "/awips2/edex/data/utility/common_static/site/*/roles/userRoles.xml" 
COMBINATIONS_PERMISSION = "com.raytheon.localization.site/cave_static/gfe/combinations"

import glob
import sys
import xml.etree.ElementTree as ET

def main():
    for path in glob.iglob(USER_ROLES_PATH):
        print "Updating", path
        
        tree = ET.parse(path)
        root = tree.getroot()
        for user in root.iterfind("user"):
            userId = user.attrib["userId"]
            if userId == "ALL":
                found = False
                for userPermission in user.iterfind("userPermission"):
                    if userPermission.text == COMBINATIONS_PERMISSION:
                        found = True
                        break
                if found:
                    print "userId", userId, "already has", COMBINATIONS_PERMISSION
                else:
                    print "Adding", COMBINATIONS_PERMISSION, "to userId", userId
                    sub = ET.SubElement(user, "userPermission")
                    sub.text = COMBINATIONS_PERMISSION
                    
                    # write out the updated file
                    tree.write(path)
                break

if __name__ == '__main__':
    main()
