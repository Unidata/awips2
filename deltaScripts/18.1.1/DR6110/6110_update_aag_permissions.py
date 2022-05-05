#!/usr/bin/env python2

# This script will add the new AAG permission to userRoles.xml.
# Modified from DR5816/UpdateGFEPermissions.py

USER_ROLES_PATH = "/awips2/edex/data/utility/common_static/site/*/roles/userRoles.xml"

# New permissions to be added
PERMISSIONS_TO_BE_ADDED = [
    "com.raytheon.localization.site/common_static/aviation/aag"
]

import glob
import os
import shutil
import sys
import xml.etree.ElementTree as ET

def main():
    for path in glob.iglob(USER_ROLES_PATH):
        print "Updating", path
        shutil.copyfile(path, path + ".bak")

        # Ensure we have an nwsRoleData tree
        tree = ET.parse(path)
        root = tree.getroot()
        if root.tag != "nwsRoleData":
            print "ERROR: not a valid userRoles.xml file"
            continue

        # Ensure the application for this tree is Localization
        app = root.find("application")
        if app is None or app.text != "Localization":
            print "ERROR: not a localization permissions file"
            continue

        # Add new permissions
        for id in PERMISSIONS_TO_BE_ADDED:
            # see if permission already present
            found = False
            for permission in root.iter(tag="permission"):
                if permission.attrib["id"] == id:
                    found = True
                    break

            # if permission not present add it
            if not found:
                print "Adding new permission %s" % id
                ET.SubElement(root, "permission", attrib={"id": id})
            else:
                print "Permission %s already present" % id

        for user in root.iterfind("user"):
            userId = user.attrib["userId"]
            if userId == "ALL":
                for permission in PERMISSIONS_TO_BE_ADDED:
                    found = False
                    for userPermission in user.iter(tag="userPermission"):
                        if userPermission.text == permission:
                            found = True
                            break

                    if not found:
                        print "Adding permission %s for ALL users" % permission
                        element = ET.SubElement(user, "userPermission")
                        element.text = permission
                    else:
                        print "Permission %s already present for ALL users" % permission

        tree.write(path, encoding="UTF-8", xml_declaration=True)

if __name__ == '__main__':
    sys.exit(main())
