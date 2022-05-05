#!/awips2/python/bin/python
#
# This script will grant the new awipsAdmin permission to any user
# having the old awips.user.admin permission.
#
# This will get admins assigned without requiring running a CAVE session
# as user awips or manually editing the users.ini file.
#
# All other users will be granted the default awipsUser role.
#
# This will get most users into the users.ini file so the admin
# can assign permissions without needing them to open a CAVE session
#

import errno
import glob
import os
import pwd
import re
import stat
import sys
import traceback
import xml.etree.ElementTree as ET

SETUP_ENV_PATH = "/awips2/edex/bin/setup.env"
SITEID_PATTERN = r'.*\nexport AW_SITE_IDENTIFIER=(\w+)\W.*'

SITE_ROLES_PATH = "/awips2/edex/data/utility/common_static/site/%s/roles/*.xml"
USERS_INI_PATH = "/awips2/edex/data/utility/common_static/configured/%s/roles/users.ini"
CURRENT_USERS_PATH = "/awips2/edex/data/utility/common_static/site/%s/roles/currentusers.txt"

SPECIAL_USERS = set(["ALL", "awips", "root"])
USERNAME_PATTERN = r'^[a-z_][a-z0-9_]{0,30}$'

USERS_INI_HEADER = """# -----------------------------------------------------------------------------
# This file should not be manually edited.
# Please use the user administration GUI to modify user roles/permissions.
# -----------------------------------------------------------------------------
# [users] section defines users and their (optional) assigned roles
# Users may only be assigned roles, they may not be assigned permissions.
#
# username = password, role1, role2, ..., roleN
# -----------------------------------------------------------------------------
[users]
"""

# The following dicts are for validation of the old nwsroles xml files

USER_PERMISSION = {
    "tag": "userPermission",
    "text": True,
    }

USER_ROLE = {
    "tag": "userRole",
    "text": True,
    }

USER = {
    "tag": "user",
    "text": False,
    "attrib": {
        "userId": (True, str),
        },
    "elements" : [
        # (required, multiplicity, schema)
        (False, 2, USER_PERMISSION),
        (False, 2, USER_ROLE),
        ]
    }

ROLE_DESCRIPTION = {
    "tag": "roleDescription",
    "text": True,
    }

ROLE_PERMISSION = {
    "tag": "rolePermission",
    "text": True,
    }

ROLE = {
    "tag": "role",
    "text": False,
    "attrib": {
        "roleId": (True, str)
        },
    "elements": [
        # (required, multiplicity, schema)
        (False, 1, ROLE_DESCRIPTION),
        (True, 2, ROLE_PERMISSION),
        ]
    }

PERMISSION_DESCRIPTION = {
    "tag": "description",
    "text": True,
    }

PERMISSION = {
    "tag": "permission",
    "text": False,
    "attrib": {
        "id": (True, str)
        },
    "elements": [
        # (required, multiplicity, schema)
        (False, 1, PERMISSION_DESCRIPTION)
        ]
    }

APPLICATION = {
    "tag": "application",
    "text": True,
    }

NWS_ROLE_DATA = {
    "tag": "nwsRoleData",
    "text": False,
    "elements": [
        # (required, multiplicity, schema)
        (True, 1, APPLICATION),
        (True, 2, PERMISSION),
        (False, 2, ROLE),
        (False, 2, USER)
        ]
    }

def formatElement(element):
    s = "<" + element.tag
    if element.attrib:
        for id, value in element.items():
            s += ' %s="%s"' % (id, value)
    s += ">"
    return s

def validateSchema(element, schema):
    # validates the xml is syntactically correct based on the provided schema
    # there is no validation of the content, just the format

    valid = True

    # validate tag
    if element.tag != schema["tag"]:
        print "   ERROR: Unrecognized element <%s>, skipping..." % element.tag
        return False

    # validate text ignoring whitespace
    text = element.text
    if text:
        text = text.strip()

    if text:
        if not schema["text"]:
            print 'ERROR: Unexpected text %s found in element <%s>' % (repr(text), element.tag)
            valid = False
    elif schema["text"]:
        print "ERROR: Element <%s> missing text" % element.tag
        valid = False

    # validate attributes
    expectedAttrib = schema.get("attrib", {})

    # ensure existing attributes are valid
    for key in element.attrib:
        if key in expectedAttrib:
            value = element.attrib[key]
            expectedType = expectedAttrib[key][1]
            if type(value) is not expectedType:
                print "   ERROR: Attribute %s:[%s] of element <%s> is not of expected type %s" % \
                    (key, str(value), element.tag, str(expectedType))
                valid = False
        else:
            print 'ERROR: Unexpected attribute "%s" found in element<%s>' % (key, element.tag)
            valid = False

    # ensure required attributes are present
    for key in expectedAttrib:
        required = expectedAttrib[key][0]
        if required and key not in element.attrib:
            print 'ERROR: Missing attribute "%s" in element <%s>' % (key, element.tag)
            valid = False

    # validate child elements
    expectedElements = schema.get("elements", [])

    # ensure existing child elements are valid
    childCount = {}
    for child in element:

        # find matching child schema
        found = False
        for required, multiplicity, childSchema in expectedElements:
            if child.tag == childSchema["tag"]:
                found = True

                # update child count
                childCount[child.tag] = childCount.get(child.tag, 0) + 1

                # validate child element
                valid &= validateSchema(child, childSchema)

        if not found:
            print 'ERROR: Unexpected child element %s found in element %s' % \
                (formatElement(child), formatElement(element))
            valid = False

    # ensure required children were found and multiplicity was valid
    for required, multiplicity, childSchema in expectedElements:
        count = childCount.get(childSchema["tag"], 0)
        if count == 0 and required:
            print 'ERROR: Element %s is missing required child element <%s>' % \
                (formatElement(element), childSchema["tag"])
            valid = False
        elif count > 1 and multiplicity == 1:
            print 'ERROR: %d  <%s> child elements found in element %s where only 1 is allowed' % \
                (count, childSchema["tag"], formatElement(element))
            valid = False

    return valid

def parseRolesPermissions(root):
    permissions = {}
    roles = {}
    users = {}
    application = root.find("application").text.strip()

    # parse permissions
    for permission in root.iterfind("permission"):
        id = permission.attrib["id"]
        description = permission.find("description")

        if description is not None:
            description = description.text

        if description is not None:
            description = description.strip()

        permissions[id] = description


    # parse roles
    for role in root.iterfind("role"):
        roleId = role.attrib["roleId"].strip()
        roleDescription = role.find("roleDescription")

        if roleDescription is not None:
            roleDescription = roleDescription.text

        if roleDescription is not None:
            roleDescription = roleDescription.strip()

        rolePermissions = set()
        for rolePermission in role.iterfind("rolePermission"):
            rolePermissions.add(rolePermission.text.strip())

        roles[roleId] = {"roleDescription":roleDescription,
                         "rolePermissions": rolePermissions
                        }


    # parse users
    for user in root.iterfind("user"):
        userId = user.attrib["userId"].strip()

        userPermissions = set()
        for userPermission in user.iterfind("userPermission"):
            userPermissions.add(userPermission.text.strip())

        userRoles = set()
        for userRole in user.iterfind("userRole"):
            userRoles.add(userRole.text.strip())

        users[userId] = { "userRoles": userRoles,
                          "userPermissions": userPermissions
                        }


    return application, permissions, roles, users


def main():
    userName = pwd.getpwuid(os.getuid()).pw_name
    if userName not in ['awips', 'root']:
        print "ERROR: This script must be run as user root or awips"
        return 1

    # parse site identifier from setup.env
    siteId = None
    try:
        with open(SETUP_ENV_PATH, "r") as env:
            contents = env.read()
            m = re.match(SITEID_PATTERN, contents)
            if m is not None:
                siteId = m.group(1)
    except:
        print "ERROR: Unable to read", SETUP_ENV_PATH, "exiting"
        traceback.print_exc()
        return 1

    if siteId is None:
        print "ERROR: AW_SITE_IDENTIFIER not found in", SETUP_ENV_PATH
        return 1

    # if users.ini already exists just exit
    iniPath = USERS_INI_PATH % siteId
    if os.path.exists(iniPath):
        print "WARN:", iniPath, "already exists, exiting."
        return 1

    # loop over all user roles xml files looking for users and/or admins
    allUsers = set()
    admins = set()
    paths = glob.glob(SITE_ROLES_PATH % siteId)
    for path in paths:
        print "INFO: Processing file:", path
        try:
            tree = ET.parse(path)
            root = tree.getroot()
        except:
            print "ERROR: Unable to parse XML file: %s" % path
            traceback.print_exc()
            continue

        # ensure file contains valid XML
        if not validateSchema(root, NWS_ROLE_DATA):
            print "ERROR:", path, "does not contain valid nwsRoleData xml, skipping"
            continue

        # parse out roles and permissions into pythn dicts
        application, permissions, roles, users = parseRolesPermissions(root)

        for user in users:
            allUsers.add(user)
            if application == "User Administration" and \
                "awips.user.admin" in users[user]["userPermissions"]:
                admins.add(user)


    # set allUsers to the content of
    # /awips2/edex/data/utility/common_static/site/XXX/roles/currentUsers.txt
    # if it exists
    currentUsersPath = CURRENT_USERS_PATH % siteId
    currentUsers = None
    try:
        with open(currentUsersPath, 'r') as f:
            currentUsers = f.readlines()
        currentUsers = [x.strip() for x in currentUsers]
    except IOError as e:
        if e.errno == errno.ENOENT:
            print "WARN: %s file not found,\n    using list of users for existing roles files" % currentUsersPath
        elif e.errno == errno.EACCES:
            print "ERROR: Unable to read %s,\n    correct file permissions and re-run this script" % currentUsersPath
            return 1
        else:
            print "ERROR: Error reading %s,\n    fix the file and re-run this script" % currentUsersPath
            traceback.print_exc()
            return 1

    if currentUsers:
        # remove None or empty strings
        currentUsers = filter(None, currentUsers)

        # validate user names
        for user in currentUsers:
            if not re.match(USERNAME_PATTERN, user):
                print "ERROR: %s\n    contains an invalid username: '%s'\n    correct and re-run this script" % (currentUsersPath, user)
                return 1

        allUsers = set(currentUsers)

    # remove special users
    allUsers -= SPECIAL_USERS

    # remove admins that are not in allUsers
    admins &= allUsers

    # convert allUsers set to a sorted list
    # This just makes the file easier for a human
    # to look at after running the delta script.
    # The GUI will always sort the user names
    allUsers = sorted(allUsers)

    # output users.ini file
    try:
        dirPath = os.path.dirname(iniPath)
        try:
            os.makedirs(dirPath, 0750)
        except OSError, e:
            if e.errno != errno.EEXIST:
                raise

        with open(iniPath, 'w') as out:
            out.write(USERS_INI_HEADER)

            for user in allUsers:
                role = "awipsUser"
                if user in admins:
                    role = "awipsAdmin"
                print "INFO: Granting", user, role, "role"
                out.write("%s = password, %s\n" % (user, role))
        os.chmod(iniPath, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP)

        # change owner to awips
        p = pwd.getpwnam("awips")
        os.chown(dirPath, p.pw_uid, p.pw_gid)
        os.chown(iniPath, p.pw_uid, p.pw_gid)
    except:
        print "ERROR: Exception writing to %s" % iniPath
        traceback.print_exc()

        # remove any partially written users.ini file
        if os.path.isfile(iniPath):
            os.remove(iniPath)
        return 1

    print "INFO: Successfully migrated awips admins"
    return 0

if __name__ == '__main__':
    sys.exit(main())
