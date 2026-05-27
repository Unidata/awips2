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
# Python wrapper for PointDataView
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/20/09                      njensen       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

class PointDataView:

    def __init__(self, javaPointDataView):
        self.__javaPdv = javaPointDataView
        self.__keys = []
        keyset = self.__javaPdv.getContainer().getParameters()
        itr = keyset.iterator()
        while itr.hasNext():
            self.__keys.append(str(next(itr)))

    def __getitem__(self, key):
        result = None
        strValType = self.getType(key)
        if strValType == 'FLOAT':
            result = self.__javaPdv.getFloat(key)
        elif strValType == 'STRING':
            result = self.__javaPdv.getString(key)
            if isinstance(result, bytes):
                result = result.decode()
        elif strValType == 'INT':
            result = self.__javaPdv.getInt(key)
        elif strValType == 'LONG':
            result = self.__javaPdv.getLong(key)

        return result

    def getType(self, key):
        val = self.__javaPdv.getType(key)
        if val:
            val = str(val)
        return val

    def has_key(self, key):
        return self.__keys.__contains__(key)

    def keys(self):
        return self.__keys

    def __iter__(self):
        return iter(self.keys())

    def __contains__(self, key):
        return key in self.__keys

    def getFillValue(self, key):
        # TODO if we get fill value support in pointdata, hook that up
        return -9999.0

    def getNumberAllLevels(self, key):
        strValType = self.getType(key)
        jlevels = self.__javaPdv.getNumberAllLevels(key)
        levels = []
        for level in jlevels:
            level = str(level)
            if strValType == 'FLOAT':
                levels.append(float(level))
            elif strValType == 'STRING':
                if isinstance(level, bytes):
                    levels.append(level.decode())
                else:
                    levels.append(str(level))
            elif strValType == 'INT':
                levels.append(int(level))
            elif strValType == 'LONG':
                levels.append(int(level))
        return levels
