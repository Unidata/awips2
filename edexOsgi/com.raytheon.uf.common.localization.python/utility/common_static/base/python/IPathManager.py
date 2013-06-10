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
# Class with abstract methods for use with localization files.  
# One implementation of this will be a pure python implementation 
# for use outside of the AWIPS II architecture, and one will follow 
# the AWIPS II architecture.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash        Initial Creation.
#    
# 
#

import abc

class IPathManager(object):
    
    __metaclass__ = abc.ABCMeta
    
    def __init__(self):
        return
    
    @abc.abstractmethod
    def getLocalizationFile(self, name, context=None):
        pass
    
    @abc.abstractmethod
    def getTieredLocalizationFile(self, loctype, name):
        pass
    
    @abc.abstractmethod
    def listFiles(self, name, extensions, recursive, filesOnly, loctype=None, loclevel=None, locname=None):
        pass
    
    @abc.abstractmethod
    def getAvailableLevels(self):
        pass