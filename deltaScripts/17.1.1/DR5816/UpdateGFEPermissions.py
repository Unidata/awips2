#!/usr/bin/env python
# ****
# **** NOTE: This delta script must be run AFTER the delta script for DR 5211
# ****
# This delta script will add the new "GFE Focal Point" role to the site's userRoles.xml file.
# This role allows a user to edit GFE and GFE Server site level configuration files.
#
# It will remove the following permissions from all users and will grant the new "GFE Focal Point"
# role to any other users who previously had either of these permissions.
#
#       com.raytheon.localization.site/cave_static/gfe
#       com.raytheon.localization.site/common_static/gfe

GFE_CONFIG_PATH = "/awips2/edex/data/utility/common_static/site/*/gfe/config/siteConfig.py"
USER_ROLES_PATH = "/awips2/edex/data/utility/common_static/site/*/roles/userRoles.xml"

# New permissions to be added
PERMISSIONS_TO_BE_ADDED = [
    "com.raytheon.localization.site/cave_static/gfe/drafts",
    "com.raytheon.localization.site/common_static/vtec",
    "com.raytheon.localization.site/common_static/grid/parameterInfo"
]

# New roles to be added
ROLES_TO_BE_ADDED = {
    "GFE Focal Point" : {
        "roleDescription" : "This role is a grouping of permissions for GFE Focal Points",
        "rolePermission"  : [
            "com.raytheon.localization.site/cave_static/gfe",
            "com.raytheon.localization.site/common_static/gfe",
            "com.raytheon.localization.site/common_static/vtec",
            "com.raytheon.localization.site/common_static/grid/parameterInfo"
        ]
    }
}

PERMISSIONS_TO_BE_ADDED_TO_ALL = [
    "com.raytheon.localization.site/cave_static/gfe/drafts",
]

# GFE SITE level permissions
# These will be removed from ALL
# Any other users who had any of these permissions will be granted
# the GFE Focal Point role
GFE_SITE_PERMISSIONS = [
    "com.raytheon.localization.site/cave_static/gfe",
    "com.raytheon.localization.site/common_static/gfe"
]

import glob
import os
import shutil
import sys
import xml.etree.ElementTree as ET

def main():
    if not glob.glob(GFE_CONFIG_PATH):
        print "WARNING: You have not yet run the delta script for DR 5211 or you have no GFE sites configured."
        print "         Please re-run this delta script after running the delta script for DR 5211."
        print "         If you have no GFE sites configured you do not need to run this delta script."
        return 1

    for path in glob.iglob(USER_ROLES_PATH):
        print "Updating", path
        shutil.copyfile(path, path + ".bak")
        
        focalPoints = []
        gfePath = path.replace("userRoles", "gfeRoles")
        if os.path.isfile(gfePath):
            gfeRoles = ET.parse(gfePath)
            for user in gfeRoles.iter(tag="user"):
                for userRole in user.iter(tag="userRole"):
                    if userRole.text == "focalPoint":
                        userId = user.attrib["userId"]
                        print "Identified GFE focal point %s" % userId
                        focalPoints.append(userId)
                        break;

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

            # if permission not preset add it
            if not found:
                print "Adding new permission %s" % id
                ET.SubElement(root, "permission", attrib={"id": id})
            else:
                print "Permission %s already present" % id

        # Add new roles
        for roleId in ROLES_TO_BE_ADDED:
            found = False
            for role in root.iter(tag="role"):
                if role.attrib["roleId"] == roleId:
                    found = True
                    break

            if not found:
                print "Adding new role %s" % roleId
                role = ET.SubElement(root, "role", attrib={"roleId": roleId})
                roleData = ROLES_TO_BE_ADDED[roleId]
                for key in roleData:
                    values = roleData[key]
                    if type(values) is not list:
                        values = [values]

                    for value in values:
                        element = ET.SubElement(role, key)
                        element.text = value
            else:
                print "Role %s already preset" % roleId

        # Remove GFE site level permissions
        for user in root.iterfind("user"):
            userId = user.attrib["userId"]
            toRemove = []
            for userPermission in user.iter(tag="userPermission"):
                if userPermission.text in GFE_SITE_PERMISSIONS:
                    toRemove.append(userPermission)
                    
            for userPermission in toRemove:
                print "Removing userPermission %s for user %s" % (userPermission.text, userId)
                user.remove(userPermission)
            
            removed = len(toRemove) > 0

            # Add new ALL permissions
            if userId == "ALL":
                for permission in PERMISSIONS_TO_BE_ADDED_TO_ALL:
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

            # If user had GFE site permissions or has GFE focalPoint role
            # Add the GFE Focal Point role
            elif removed or userId in focalPoints:
                    found = False
                    for userRole in user.iter(tag="userRole"):
                        if userRole.text == "GFE Focal Point":
                            found = True
                            break

                    if not found:
                        print "Adding role GFE Focal Point for user %s" % userId
                        element = ET.SubElement(user, "userRole")
                        element.text = "GFE Focal Point"
                    else:
                        print "GFE Focal Point role already present for user %s" % userId
                    
                    if userId in focalPoints:
                        focalPoints.remove(userId)
        
        # Add GFE Focal Point role for any remaining focalPoints
        for userId in focalPoints:
            user = ET.SubElement(root, "user")
            user.attrib["userId"] = userId

            print "Adding role GFE Focal Point for user %s" % userId
            element = ET.SubElement(user, "userRole")
            element.text = "GFE Focal Point"

        tree.write(path, encoding="UTF-8", xml_declaration=True)

if __name__ == '__main__':
    sys.exit(main())
