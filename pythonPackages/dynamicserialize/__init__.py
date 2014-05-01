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
# TODO
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/20/10                      njensen       Initial Creation.
#    
# 
#

__all__ = [
           ]

import dstypes, adapters
import DynamicSerializationManager

class SerializationException(Exception):
    
    def __init__(self, message=None):
        self.message = message
    
    def __str__(self):
        if self.message:
            return self.message 
        else:
            return ""
        
def serialize(obj):
    dsm = DynamicSerializationManager.DynamicSerializationManager()
    return dsm.serializeObject(obj)

def deserialize(bytes):
    dsm = DynamicSerializationManager.DynamicSerializationManager()
    return dsm.deserializeBytes(bytes)