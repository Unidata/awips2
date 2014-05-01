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
# Utility method to unpickle python objects to a java map
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/21/09        1995          bphillip       Initial Creation.
#    
# 
#

def unPickle(str):
    import pickle,tempfile,os,JUtil
    tempfile.tempdir = "/tmp/" 
    fname = tempfile.mktemp(".bin")
    FILE = open(fname,"w")
    FILE.write(str)
    FILE.close()
    
    FILE = open(fname,"r")
    retVal = pickle.load(FILE)
    FILE.close()
    return JUtil.pyDictToJavaMap(retVal)