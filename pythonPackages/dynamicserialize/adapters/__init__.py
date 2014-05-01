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
# __init__.py for Dynamic Serialize adapters.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/31/10                      njensen       Initial Creation.
#    03/20/13           #1774      randerso      Added TimeConstraintsAdapter
#    04/22/13           #1949      rjpeter       Added LockTableAdapter
# 
#

__all__ = [
           'PointAdapter',
           'StackTraceElementAdapter',
           'WsIdAdapter',
           'CalendarAdapter',
           'GregorianCalendarAdapter',
           'ActiveTableModeAdapter',
           'DateAdapter',
           'LocalizationLevelSerializationAdapter',
           'LocalizationTypeSerializationAdapter',
           'GeometryTypeAdapter',
           'CoordAdapter',
           'TimeRangeTypeAdapter',
           'ParmIDAdapter',
           'DatabaseIDAdapter',
           'TimestampAdapter',
           'EnumSetAdapter',
           'FloatBufferAdapter',
           'ByteBufferAdapter',
           'TimeConstraintsAdapter',
           'LockTableAdapter'
#           'GridDataHistoryAdapter',
           ]
 
classAdapterRegistry = {}

 
def getAdapterRegistry():
    import sys    
    for x in __all__:
        exec 'import ' + x
        m = sys.modules['dynamicserialize.adapters.' + x]
        d = m.__dict__
        if d.has_key('ClassAdapter'):
            if isinstance(m.ClassAdapter, list):
                for clz in m.ClassAdapter:
                    classAdapterRegistry[clz] = m
            else:
                clzName = m.ClassAdapter
                classAdapterRegistry[clzName] = m
        else:
            raise LookupError('Adapter class ' + x + ' has no ClassAdapter field ' + \
                              'and cannot be registered.')
                               

getAdapterRegistry()

