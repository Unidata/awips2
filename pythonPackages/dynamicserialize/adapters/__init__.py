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
# Plugins can contribute to dynamicserialize.adapters by either including their
# classes directly in pythonPackages/dynamicserialize/adapters/ within their
# plugin. The plugin's adapter will automatically be added to __all__ at runtime
# and registered.
# Plugins should not include a custom __init__.py in 
# pythonPackages/dynamicserialize/adapters/ because it will overwrite this file. 
# If custom package initialization is needed, a subpackage should be created
# with an __init__.py that includes the following:
#
# __all__ = ['CustomAdapter1', 'CustomAdapter2']
# from dynamicserialize.adapters import registerAdapters
# registerAdapters(__name__, __all__)
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/31/10                      njensen        Initial Creation.
#    03/20/13           #1774      randerso       Added TimeConstraintsAdapter
#    04/22/13           #1949      rjpeter        Added LockTableAdapter
#    02/06/14           #2672      bsteffen       Added JTSEnvelopeAdapter
#    06/22/2015         #4573      randerso       Added JobProgressAdapter
#    09/21/2015         #4486      rjpeter        Added FormattedDateAdapter
#    06/23/2016         #5696      rjpeter        Added CommutativeTimestampAdapter
#    10/17/2016         #5919      njensen        Added GeomDataRespAdapter
#    01/09/2017         #5997      nabowle        Allow contribution from plugins.
#

__all__ = [
    'PointAdapter',
    'StackTraceElementAdapter',
    'CalendarAdapter',
    'GregorianCalendarAdapter',
    'DateAdapter',
    'GeometryTypeAdapter',
    'CoordAdapter',
    'TimestampAdapter',
    'EnumSetAdapter',
    'FloatBufferAdapter',
    'ByteBufferAdapter',
    'JTSEnvelopeAdapter'
]

classAdapterRegistry = {}


def getAdapterRegistry():
    import pkgutil
    
    discoveredPackages = []
    # allow other plugins to contribute to adapters by dropping their adapter or
    # package into the dynamicserialize.adapters package
    for _, modname, ispkg in pkgutil.iter_modules(__path__):
        if ispkg:
            discoveredPackages.append(modname)
        else:
            if modname not in __all__:
                __all__.append(modname)
    
    registerAdapters(__name__, __all__)
    
    for pkg in discoveredPackages:
        __import__(__name__ + '.' + pkg)
    
            
def registerAdapters(package, modules):
    import sys
    if not package.endswith('.'):
        package += '.'
    for x in modules:
        exec 'import ' + package + x
        m = sys.modules[package + x]
        d = m.__dict__
        if d.has_key('ClassAdapter'):
            if isinstance(m.ClassAdapter, list):
                for clz in m.ClassAdapter:
                    classAdapterRegistry[clz] = m
            else:
                clzName = m.ClassAdapter
                classAdapterRegistry[clzName] = m
        else:
            raise LookupError('Adapter class ' + x + ' has no ClassAdapter field ' +
                              'and cannot be registered.')


getAdapterRegistry()
