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


from java.lang import Integer, Float, Long, Boolean, String
from java.util import HashMap, LinkedHashMap, ArrayList
from java.util import Collections
from collections import OrderedDict

#
# Provides convenience methods for Java-Python bridging
#
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/01/08                      njensen       Initial Creation.
#    03/12/13         1759         dgilling      Extend Java List types handled
#                                                by javaObjToPyVal().
#
#
#


def javaStringListToPylist(jlist):
    pylist = []
    size = jlist.size()
    for i in range(size):
        pylist.append(str(jlist.get(i)))
    return pylist

def pylistToJavaStringList(pylist):
    jlist = ArrayList();
    for i in pylist:
        jlist.add(String(i))
    return jlist

def javaStringMapToPyDict(javaMap):
    keys = javaMap.keySet()
    itr = keys.iterator()
    pyDict = {}
    while itr.hasNext():
        key = itr.next()
        val = javaMap.get(key)
        fval = str(val)
        if fval.find('[') > -1:
            exec "fval = " + fval
        else:
            try:
                fval = float(fval)
            except:
                pass
        pyDict[str(key)] = fval
    return pyDict

def javaMapToPyDict(javaMap, customConverter=None):
    keys = javaMap.keySet()
    itr = keys.iterator()
    if javaMap.jclassname == "java.util.LinkedHashMap":
        pyDict = OrderedDict()
    else:
        pyDict = {}
    while itr.hasNext():
        key = itr.next()
        obj = javaMap.get(key)
        pyDict[javaObjToPyVal(key)] = javaObjToPyVal(obj, customConverter)
    return pyDict

def pyDictToJavaMap(pyDict):
    if pyDict is None :
        return None

    if isinstance(pyDict, OrderedDict):
        jmap = LinkedHashMap()
    else:
        jmap = HashMap()

    for key in pyDict:
        jmap.put(pyValToJavaObj(key), pyValToJavaObj(pyDict[key]))
    return jmap

def pyValToJavaObj(val):
    retObj = val
    valtype = type(val)
    if valtype is int:
        retObj = Integer(val)
    elif valtype is float:
        retObj = Float(val)
    elif valtype is long:
        retObj = Long(val)
    elif valtype is bool:
        retObj = Boolean(val)
    elif valtype is list:
        retObj = ArrayList()
        for i in val:
            retObj.add(pyValToJavaObj(i))
    elif valtype is tuple:
        tempList = ArrayList()
        for i in val:
            tempList.add(pyValToJavaObj(i))
        retObj = Collections.unmodifiableList(tempList)
    elif issubclass(valtype, dict):
        retObj = pyDictToJavaMap(val)
    elif issubclass(valtype, JavaWrapperClass):
        retObj = val.toJavaObj()
    return retObj

def javaObjToPyVal(obj, customConverter=None):
    retVal = None
    if obj is None:
        return retVal

    objtype = obj.jclassname
    if objtype == "java.lang.Integer":
        retVal = obj.intValue()
    elif objtype == "java.lang.Float":
        retVal = obj.floatValue()
    elif objtype == "java.lang.Long":
        retVal = obj.longValue()
    elif objtype == "java.lang.Boolean":
        retVal = bool(obj.booleanValue())
    elif objtype in ["java.util.ArrayList", "java.util.Arrays$ArrayList"]:
        retVal = []
        size = obj.size()
        for i in range(size):
            retVal.append(javaObjToPyVal(obj.get(i), customConverter))
    elif objtype == "java.util.Collections$UnmodifiableRandomAccessList":
        tempList = []
        size = obj.size()
        for i in range(size):
            tempList.append(javaObjToPyVal(obj.get(i), customConverter))
        retVal = tuple(tempList)
    elif objtype == "java.util.HashMap":
        retVal = javaMapToPyDict(obj, customConverter)
    elif customConverter is not None:
        retVal = customConverter(obj)

    if retVal is None:
        retVal = str(obj)
    return retVal

class JavaWrapperClass(object):
    def toJavaObj(self):
        raise NotImplementedError, "Subclasses must override this method."

