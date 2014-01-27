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
# System Diagnostics
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/27/09                      njensen       Initial Creation.
#    
#

import sys, os

def cstringIOTest():
    try:
        import cStringIO
        return cStringIO.__file__
    except ImportError:
        return "WARNING - Failed to import cStringIO"

def numpyTest():
    try:
        import numpy        
        return numpy.version.version
    except ImportError:
        return "WARNING - Failed to import numpy"

def pathEdexHome():
    try:
        return os.environ['EDEX_HOME']
    except KeyError:
        return "WARNING - No EDEX_HOME environment variable detected"

def pathJavaHome():
    try:
        return os.environ['JAVA_HOME']
    except KeyError:
        return "WARNING - No JAVA_HOME environment variable detected"

def pathLdLibraryPath():
    try:
        val = os.environ['LD_LIBRARY_PATH']
        if val.find('awips/lib') < 0:
            val = "WARNING: awips/lib not found\n" + val
        return val
    except KeyError:
        return "WARNING - No LD_LIBRARY_PATH environment variable detected"

def pathLdPreload():
    try:
        val = os.environ['LD_PRELOAD']
        if val.find('libpython2.5.so') < 0:
            val = "WARNING: libpython2.5.so not found\n" + val
        return val
    except KeyError:
        return "WARNING - No LD_PRELOAD environment variable detected"

def pathPath():
    try:
        val = os.environ['PATH']
        if val.find('/usr/bin') < val.find('awips/bin'):
            val = "WARNING: /usr/bin on PATH is before awips/bin\n" + val
        return val 
    except KeyError:
        return "WARNING - No PATH environment variable detected"

def platform():        
    return sys.platform

def pythonExecutable():    
    return sys.executable    

def pythonPath():    
    return sys.path    

def pythonVersion():    
    return sys.version
    
def zipTest():
    try:
        import zipimport        
        return "Successfully imported zipimport"
    except ImportError:
        return "Failed to import zipimport"

def runDiagnostics():
    results = []
    results.append(('Platform', platform()))
    results.append(('Python Executable', pythonExecutable()))
    results.append(('Python Version', pythonVersion()))
    results.append(('Python Path', pythonPath()))
    results.append(('cStringIO library', cstringIOTest()))
    results.append(('zipimport import', zipTest()))
    results.append(('numpy version', numpyTest()))    
    results.append(('JAVA_HOME', pathJavaHome()))
    results.append(('PATH', pathPath()))
    results.append(('EDEX_HOME', pathEdexHome()))    
    results.append(('LD_LIBRARY_PATH', pathLdLibraryPath()))
    results.append(('LD_PRELOAD', pathLdPreload()))
    return results

def runFromEngine():
    from java.util import ArrayList
    try:
        from com.raytheon.uf.common.message.response import ResponseMessageGeneric
    except:
        # older EDEX versions support before the class was moved
        from com.raytheon.edex.msg import ResponseMessageGeneric
    jlist = ArrayList()
    vals = runDiagnostics()
    for v in vals:
        r = ResponseMessageGeneric(v[0] + ": " + str(v[1]))
        jlist.add(r)
    return jlist
    
    
    