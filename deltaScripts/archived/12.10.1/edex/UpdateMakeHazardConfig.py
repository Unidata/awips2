#!/usr/bin/env python
# Issue #436
# Update script to move and modify MakeHazardConfig.py for local effects based hazards work assignment

import os, re, string, sys

configStartPattern = """###########################################################
##############                              ###############
##############    CONFIGURATION SECTION     ###############
##############                              ###############
"""

configEndPattern = "####################   END CONFIGURATION SECTION   #################"

localEffectsAdditions = """# Dictionary mapping Hazard Types to applicable local effect areas
#    that can be intersected with the zone edit areas.
# You should not define localEffectAreas entries for Tropical Cyclone
# or Convective Watches.
localEffectAreas = {}

#localEffectAreas = {
#  'Winter Weather' : ["Below_1000","Below_1500","Below_2000","Below_2500","Below_3000","Below_3500","Below_4000",
#                      "Above_1000","Above_1500","Above_2000","Above_2500","Above_3000","Above_3500"],
#                   }

# Dictionary associating local Effect Area names with a corresponding
#    segment number, display name, and list of zones to be auto-selected
# If you do not wish to auto-select zones you should supply an empty list
#
# The display name allows you to display a "pretty" string in the UI rather
# than the edit area name. If the display name is empty ("") the edit area
# name will be used.
localAreaData = {}

#localAreaData = {
#     "Below_1000" : ( 999, "", []),
#     "Below_1500" : (1499, "", []),
#     "Below_2000" : (1999, "", []),
#     "Below_2500" : (2499, "", []),
#     "Below_3000" : (2999, "", []),
#     "Below_3500" : (3499, "", []),
#     "Below_4000" : (3999, "", []),
#     "Above_1000" : (1000, "", []),
#     "Above_1500" : (1500, "", []),
#     "Above_2000" : (2000, "", []),
#     "Above_2500" : (2500, "", []),
#     "Above_3000" : (3000, "", []),
#     "Above_3500" : (3500, "", []),
#     }
"""

hazardDictPattern = re.compile("hazardDict\s*=\s*\{(.*?)}", re.DOTALL)
entryPattern = re.compile("('.*?')\s*:\s*(\[.*?\])", re.DOTALL)
gumPattern = re.compile('\nsiteID = DataManager.getCurrentInstance\(\).getSiteID\(\)\nif siteID == "GUM":\n(.*?)\nelse:\n(.*?\n)\n', re.DOTALL)
sitePattern = re.compile("/awips2/edex/data/utility/cave_static/site/(.*?)/")
leadingSpacePattern = re.compile("(\s*?)\S")
commentPattern = re.compile("^", re.MULTILINE)

orderdDictImport = "\n\nfrom collections import OrderedDict\n"
gumComment = "\n# for GUM use comment out the above definition and uncomment the one below\n\n"

fixed = []
status = 0

def fixEntry(m):
    newEntry = "(" + m.string[m.start(1):m.end(1)] + ", " + m.string[m.start(2):m.end(2)] + ")"
    return newEntry

def fixHazardDict(m):
    newDict = "hazardDict =  OrderedDict([" + m.string[m.start(1):m.end(1)] + "])"
    newDict = re.sub(entryPattern, fixEntry, newDict)
    return newDict

def unindent(s):
    m = re.match(leadingSpacePattern, s)
    if m is not None:
        p = re.compile("^"+m.string[m.start(1):m.end(1)], re.MULTILINE)
        s = re.sub(p, "", s)
    return s

def comment(s):
    s = re.sub(commentPattern, "#", s)
    return s

def processDir(arg, srcDir, names):
    global fixed, status
    if arg in names:
        src = os.path.join(srcDir, arg)

        # skip if already fixed
        if src in fixed:
            return

        print
        if "userPython/procedures" in srcDir:
            destDir = string.replace(srcDir, "userPython/procedures", "userPython/utilities")
            if not os.path.exists(destDir):
                os.makedirs(destDir)
            dest = os.path.join(destDir, arg)
            if os.path.exists(dest):
                print "MakeHazardConfig.py exists in both\n\t"+srcDir+" and\n\t"+destDir+"\nPlease update this file manually"
                status = 1
                return
            else:
                print "moving", src, "to utilities"
                try:
                    os.rename(src, dest)
                    src = dest
                except:
                    print "Error moving file\nPlease update this file manually"
                    status = 1
                    return

        try:
            f = open(src, 'r')
            contents = f.read()
        except:
            print "Error reading file: ",src
            print sys.exc_info()[0]
            print "Please update this file manually"
            status = 1
            return
        finally:
            try:
                f.close()
            except:
                pass  # if we read the file successfully don't care about close

        # skip if doesn't contain the old hazardDict pattern
        if re.search(hazardDictPattern, contents) is None:
            print src, "cannot be automatically updated.\nPlease manually update this file if required"
            status = 1
            return

        print "updating", src
        m = re.search(gumPattern, contents)
        if m is not None:
            # determine site ID
            m1 = re.match(sitePattern, srcDir)
            siteId = srcDir[m1.start(1):m1.end(1)]

            gumBlock = unindent(contents[m.start(1):m.end(1)])
            regBlock = unindent(contents[m.start(2):m.end(2)])
            if siteId == "GUM":
                contents = contents[:m.start(0)] + orderdDictImport + comment(regBlock) + gumComment + gumBlock + "\n" + contents[m.end(0):]
            else:
                contents = contents[:m.start(0)] + orderdDictImport + regBlock + gumComment + comment(gumBlock) + "\n" + contents[m.end(0):]

        newContents = re.sub(hazardDictPattern, fixHazardDict, contents)
        newContents = re.sub(configStartPattern, "", newContents)
        newContents = re.sub(configEndPattern, "", newContents)
        newContents += localEffectsAdditions

        try:
           back = src+".BAK"
           os.rename(src,back)
        except:
            print "Unable to create backup file, Please update this file manually"
            status = 1
            return

        try:
            f = open(src, 'w')
            f.write(newContents)
        except:
            print "Error writing updated file:"
            print sys.exc_info[0]
            print "Please restore from", back, "and update manually"
            status = 1
        finally:
            try:
                f.close()
                fixed.append(src)
            except:
                print "Error closing updated file:"
                print sys.exc_info[0]
                print "Please restore from", back, "and update manually"
                status = 1

def main():
    os.path.walk("/awips2/edex/data/utility/cave_static/site", processDir, "MakeHazardConfig.py")
    if status != 0:
        print "1 or more errors were encountered. Please review the log carefully."
    return status

if __name__ == "__main__":
    sys.exit(main())
