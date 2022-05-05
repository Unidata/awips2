##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

#
#  Creates combination files specific to a site.  Somewhat ported from AWIPS-I.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/08/08                      njensen        Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

import glob
import os
import pprint
import stat
import tempfile
import traceback

import LogStream


# Creation of combination files
def createComboFiles(definitionDir, outputDir, mapDict):
    #list of definition files    
    LogStream.logEvent("definitionDir", definitionDir)    
    files = glob.glob(definitionDir + '/*Definition.py')
    for f in files:
        LogStream.logEvent("File", f)
        
        try:
            buf = None
            with open(f, 'rb') as fd:
                buf = fd.read()
        except:
            LogStream.logProblem("Failure to read Definition: ", f, traceback.format_exc())
            continue

        # attempt to read in the Definition dictionary
        try:
            execLocals = {}
            exec(buf, execLocals)
            Definition = execLocals['Definition']
        except:
            LogStream.logProblem("Failure on Definition: ", f, traceback.format_exc())
            continue

        if "mapNameForCombinations" in Definition and \
          "defaultEditAreas" in Definition and \
          type(Definition['defaultEditAreas']) is str:

            srcDict = {}   #keep track of what zones from what map

            #determine if a single map or multiple maps
            if type(Definition["mapNameForCombinations"]) is str:
                maps = [Definition["mapNameForCombinations"]]
            else:
                maps = []
                for m in Definition["mapNameForCombinations"]:
                    maps.append(m)

            LogStream.logVerbose("mapNameForCombinations=", maps)

            outName = Definition["defaultEditAreas"]
            LogStream.logVerbose("Generating Combo File: ", outName)

            #See if the definition limits the zones to subdomains
            if "subDomainUGCs" in Definition and \
              Definition["subDomainUGCs"] is not None:
                limitZones = Definition["subDomainUGCs"]
            else:
                limitZones = None


            #pull out the EDITAREA attribute from all of the maps
            eans = []
            for m in maps:
                names = mapDict.get(m)
                if names is not None:
                    size = names.size()                    
                    LogStream.logVerbose("processing: ", m, "#recs=",
                      size)
                    
                    for n in range(size):
                        ean = str(names.get(n))
                        if limitZones is None or ean in limitZones:
                            #tracking source map
                            if len(ean):
                                slist = srcDict.get(m, [])
                                if ean not in slist:
                                    slist.append(ean)
                                    srcDict[m] = slist
                                    
                            #combo file
                            if ean not in eans and len(ean):
                                eans.append(ean)
            eans.sort()
            LogStream.logVerbose("eans=", eans)


            s = """
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Combinations
#   <comboName>
#
# Author: GFESuite Installation Script
# ----------------------------------------------------------------------------

# Format:
# Combinations = [
#    ([ list of edit areas as named in the GFE ], label),
#    ...
#    ]

Combinations = [
"""
            s = s.replace("<comboName>", Definition['defaultEditAreas'])
            for (count, ean) in enumerate(eans, 1):
                s = s + '       (["' + ean + '"],  "Region' + repr(count) + \
                  '"),\n'
            s = s + '       ]\n\n'
            
            # output 2nd half of combinations file (srcDict)
            s = s + "#Source Maps for edit areas\nEASourceMap = \\\n"
            pp = pprint.PrettyPrinter()
            s = s + pp.pformat(srcDict)

            if not os.path.isdir(outputDir):
                os.makedirs(outputDir)

            outName = os.path.join(outputDir, Definition["defaultEditAreas"] + ".py")
            
            try:
                with tempfile.NamedTemporaryFile('w', suffix='.py', dir=outputDir, delete=False) as fh:
                    fpath = fh.name
                    fh.write(s)
        
                os.chmod(fpath, stat.S_IRUSR | stat.S_IWUSR |
                                stat.S_IRGRP | stat.S_IWGRP | 
                                stat.S_IROTH)
                os.rename(fpath, outName)
            except:
                LogStream.logProblem("Error writing combo files", LogStream.exc())
