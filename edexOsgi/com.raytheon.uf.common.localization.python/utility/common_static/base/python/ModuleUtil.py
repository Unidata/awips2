# #
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
# #

#
# Provides a method to dynamically load a python module into memory. The method
# was relocated to this module from LocalizationUtil. 
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/11/13                      bkowal         Initial Creation.
#    01/20/14        2712          bkowal         Always add the directory path for
#                                                 the desired module to the beginning
#                                                 of the system path.
#
import os, sys, imp
    
def loadModule(filename):
    '''
    @param filename: the fully qualified name of the file
    @return: the module that was loaded
    @summary: This function takes a filename and find the module,
    loads it and returns that module
    '''
    
    path = os.path.splitext(filename)[0]
    directory = os.path.dirname(filename)
    # ensure the module containing directory is on the python path.
    sys.path.insert(0, directory)
    try:
        filename = os.path.split(path)[1]
        fp, pathname, description = imp.find_module(filename)
        module = imp.load_module(filename, fp, pathname, description)
    finally:
        sys.path.pop(0)
    
    return module