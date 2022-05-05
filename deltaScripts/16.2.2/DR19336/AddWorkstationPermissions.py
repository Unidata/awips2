#!/awips2/python/bin/python
# Adds workstation-level permissions to user ALL for all site level userRoles.xml files

USER_ROLES_PATH = "/awips2/edex/data/utility/common_static/site/*/roles/userRoles.xml"
WORKSTATION_PERMISSIONS = [
        "com.raytheon.localization.workstation/cave_static",
        "com.raytheon.localization.workstation/common_static",
        ]

import glob
import sys
import xml.etree.ElementTree as ET

def main():
    for path in glob.iglob(USER_ROLES_PATH):
        print "Updating", path

        tree = ET.parse(path)
        root = tree.getroot()
        changed = False
        for perm in WORKSTATION_PERMISSIONS:
            found = False
            for elem in root.iterfind("permission"):
                if elem.attrib.get('id') == perm:
                    found = True
                    break
            if found:
                print "Already has definition for", perm
            else:
                print "Adding definition for", perm
                sub = ET.SubElement(root, "permission")
                sub.attrib['id'] = perm
                changed = True

            for user in root.iterfind("user"):
                userId = user.attrib["userId"]
                if userId == "ALL":
                    found = False
                    for userPermission in user.iterfind("userPermission"):
                        if userPermission.text == perm:
                            found = True
                            break
                    if found:
                        print "userId", userId, "already has", perm
                    else:
                        print "Adding", perm, "to userId", userId
                        sub = ET.SubElement(user, "userPermission")
                        sub.text = perm
                        changed = True
                    break
        if changed:
            # write out the updated file
            tree.write(path)


if __name__ == '__main__':
    main()
