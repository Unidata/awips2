#!/usr/bin/env python


import sys, os, glob, shutil, pwd


def main():

    REMOVE = """#==============================================================================
#
#  The following empty code is here to fool the ifpServer into
#  thinking it's a tool.  This is so that the configuration will 
#  appear right next to the primary tool.
#
#  DO NOT CHANGE THE LINES BELOW
#
ToolType = "numeric"
WeatherElementEdited = "None"
from numpy import *
HideTool = 1

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
    def execute(self):
        return
"""

    dryrun = 0
    if len(sys.argv) > 1 and sys.argv[1] == "-dry":
        dryrun = 1

    print "running %s with dryrun = %d\n\n" % (sys.argv[0], dryrun)

    pws = pwd.getpwnam("awips")


    cavestatic = '/awips2/edex/data/utility/cave_static'
    tool_subpaths = 'gfe/userPython/smartTools'
    util_subpaths = 'gfe/userPython/utilities'
    tool_list = glob.glob(cavestatic + "/*/*/" + tool_subpaths + "/SerpConfig*.py")
    util_list = glob.glob(cavestatic + "/*/*/" + util_subpaths + "/SerpConfig*.py")
    print "current tool files:"
    print tool_list
    print "\ncurrent utilities:"
    print util_list

    for f in tool_list:
        print "\nworking from    %s" % f
        dirn, filen = os.path.split(f)
        utildir = dirn.replace("smartTools", "utilities")
        newfile = os.path.join(utildir, "SerpConfig.py")
        if os.path.exists(newfile):
            print "%s already exists. No need to create." % newfile
        else:
            content = open(f).read()
            replaced = content.replace(REMOVE, "")
            if not dryrun:
                if not os.path.exists(utildir):
                    os.makedirs(utildir)
                open(newfile, 'w+').write(replaced)
            print "create new file %s" % newfile

            if not dryrun:
                if not os.path.exists(newfile):
                    print "Error: file %s is not created." % newfile
                else:
                    os.chown(newfile, pws.pw_uid, pws.pw_gid)
                    os.chmod(newfile, 644)

        if filen == "SerpConfig.py":
            print "removing override %s" % f
            if not dryrun:
                os.remove(f)

    print ""
    for f in util_list:
        dirn, filen = os.path.split(f)
        utildir = dirn
        newfile = os.path.join(utildir, "SerpConfig.py")
        if not os.path.exists(newfile):
            if not dryrun:
                shutil.copy(f, newfile)
            print "create new file %s from %s" % (newfile, filen)
            if not dryrun:
                if not os.path.exists(newfile):
                    print "Error: file %s is not created." % newfile
                else:
                    os.chown(newfile, pws.pw_uid, pws.pw_gid)
                    pass


if __name__ == "__main__":
    main()
