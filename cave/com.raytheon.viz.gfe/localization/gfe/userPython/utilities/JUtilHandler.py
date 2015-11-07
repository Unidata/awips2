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

import JUtil

#
# Basic handler for Python to Java and back (this handles all the main things
# that JUtil needs to do, but none of the special ones that should and can be
# added if the code wants to be more powerful.
#
# Note : All methods that convert from Java to Python should take a custom converter, even if it is not used.
#
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/14/13         2250         mnash          Initial creation of JUtil handler
#    02/06/14                      mnash          Fixed fallbacks by using OrderedDict,
#                                                 fixed exception by declaring a size
#
#

from collections import OrderedDict

from java.lang import Integer, Float, Long, Boolean, String, Double, Number
from java.util import Date
from com.raytheon.uf.common.python import PyJavaUtil
import datetime

# Java -> Python conversion

def javaBasicsToPyBasics(obj, customConverter=None):
    '''
    Determines the correct method to call out of the dict to convert Java basic
    objects to Python
    '''
    if hasattr(obj, 'jclassname'):
        classname = obj.jclassname
        if classname in javaBasics :
            return True, javaBasics[classname](obj)
        else :
            for javaClass in fallbackBasics :
                if PyJavaUtil.isSubclass(obj, javaClass) :
                    return True, fallbackBasics[javaClass](obj, customConverter)
    return False, obj

def _toPythonInt(obj, customConverter=None):
    '''
    Turns a Java Integer to a Python int
    '''
    return obj.intValue()

def _toPythonLong(obj, customConverter=None):
    '''
    Turns a Java Long to a Python long, or int, 
    depending on the architecture
    '''
    return obj.longValue()

def _toPythonFloat(obj, customConverter=None):
    '''
    Turns a Java Float to a Python float
    '''
    return obj.floatValue()

def _toPythonDouble(obj, customConverter=None):
    '''
    Turns a Java Double to a Python float
    '''
    return obj.doubleValue()

def _toPythonBool(obj, customConverter=None):
    '''
    Turns a Java Boolean to a Python bool
    '''
    return bool(obj.booleanValue())

def _toPythonString(obj, customConverter=None):
    '''
    Turns a Java String to a Python str
    '''
    return obj.toString()

def _toPythonDatetime(obj, customConverter=None):
    '''
    Turns a Java Date to a Python datetime
    '''
    return datetime.datetime.fromtimestamp(obj.getTime() / 1000)

# Python -> Java conversion

def pyBasicsToJavaBasics(val):
    '''
    Method registered with JUtil to figure out any conversion of Python to Java.
    Returns a default of String of that value if nothing else can be found.
    '''
    valtype = type(val)
    if valtype in pythonBasics :
        return True, pythonBasics[valtype](val)
    return False, str(val)

def _toJavaInt(val):
    '''
    Turns a Python int to a Java Integer, or a Long, depending the range
    of the value.
    '''
    # since Python on 64 bit has larger ints, we need to do a check
    # for compatibility with Java Integers
    if val <= Integer.MAX_VALUE and val >= Integer.MIN_VALUE :
        return Integer(val)
    # outside the Java bounds for Integer, so make it a Java Long
    return Long(val)

def _toJavaFloat(val):
    '''
    Turns a Python float to a Java Float
    '''
    return Float(val)

def _toJavaLong(val):
    '''
    Turns a Python long to a Java Long
    '''
    return Long(val)

def _toJavaBoolean(val):
    '''
    Turns a Python bool to a Java Boolean
    '''
    return Boolean(val)

def _toJavaString(val):
    '''
    Turns a Python str to a Java String
    '''
    return String(str(val))

def _toJavaDate(val):
    '''
    Turns a Python datetime to a Java Date
    '''
    epoch = datetime.datetime.utcfromtimestamp(0)
    delta = val - epoch
    return Date(long(delta.total_seconds()) * 1000)

# the dict that registers the Python data type to the method for conversion
pythonBasics = OrderedDict({int:_toJavaInt, float:_toJavaFloat, long:_toJavaLong, bool:_toJavaBoolean, str:_toJavaString, unicode:_toJavaString, datetime.datetime:_toJavaDate})
# the dict that registers the Java String of type to the method for conversion
javaBasics = OrderedDict({'java.lang.Integer':_toPythonInt, 'java.lang.Float':_toPythonFloat, 'java.lang.Double':_toPythonDouble, 'java.lang.Long':_toPythonLong, 'java.lang.Boolean':_toPythonBool, 'java.lang.String':_toPythonString, 'java.util.Date':_toPythonDatetime})
fallbackBasics = OrderedDict({Number:_toPythonFloat})
'''
The following methods will handle Python and Java collection conversion.
'''
from java.lang import Object
from java.util import Collections, HashMap, LinkedHashMap, ArrayList, HashSet
from java.util import Date
from java.lang.reflect import Array
from java.util import List, Set, Map

import jep

# make a jarray to find out if we have that
JEP_ARRAY_TYPE = type(jep.jarray(0, Object))

# Java -> Python conversion

def javaCollectionToPyCollection(obj, customConverter=None):
    '''
    Main method to register with JUtil for conversion of Java
    collections to Python collections.
    '''
    if hasattr(obj, 'jclassname'):
        classname = obj.jclassname
        if classname in javaCollections :
            return True, javaCollections[classname](obj, customConverter)
        elif PyJavaUtil.isArray(obj):
            return True, _fromJavaArray(obj, customConverter)
        else :
            # we have some fallback capability, if we don't specifically handle a class, we
            # want to try some of the more common types and see if those are available for 
            # conversion
            for javaClass in fallbackCollections :
                if PyJavaUtil.isSubclass(obj, javaClass):
                    return True, fallbackCollections[javaClass](obj, customConverter)
    elif isinstance(obj, JEP_ARRAY_TYPE):
        return True, _fromJepArray(obj, customConverter)   
    return False, obj


def _toPythonList(obj, customConverter=None): 
    '''
    Converts to a Python list.
    '''           
    retVal = []
    size = obj.size()
    for i in range(size):
        retVal.append(JUtil.javaObjToPyVal(obj.get(i), customConverter))
    return retVal

def _toPythonTuple(obj, customConverter=None):
    '''
    Converts to a Python tuple.
    '''
    return tuple(_toPythonList(obj, customConverter))
    
def _toPythonDict(obj, customConverter=None):
    '''
    Converts to a Python dict.
    '''
    return __toPythonDictInternal(obj, {}, customConverter)

def _toPythonSet(obj, customConverter=None):
    '''
    Converts to a Python set.
    '''
    retVal = set()
    itr = obj.iterator()
    while itr.hasNext():
        val = itr.next() 
        retVal.add(JUtil.javaObjToPyVal(val, customConverter))
    return retVal

def _toPythonOrderedDict(obj, customConverter=None):
    '''
    Converts to a Python OrderedDict.
    '''
    return __toPythonDictInternal(obj, OrderedDict(), customConverter)

def _fromJavaArray(obj, customConverter=None):
    '''
    Converts from a Java array to a Python list.
    '''
    retVal = []
    size = Array.getLength(obj)
    for i in range(size):
        retVal.append(JUtil.javaObjToPyVal(Array.get(obj, i), customConverter))
    return retVal

def _fromJepArray(obj, customConverter=None):  
    '''
    Converts from a Jep array to a Python list.
    '''  
    retVal = []
    size = len(obj)
    for i in range(size):
        retVal.append(JUtil.javaObjToPyVal(obj[i], customConverter))    
    return retVal

def __toPythonDictInternal(javaMap, pyDict, customConverter=None):
    '''
    Converts to a Python dict.  Passed in the dict type, and then handles the key conversion.
    '''
    keys = javaMap.keySet()
    itr = keys.iterator()
    while itr.hasNext() :
        key = itr.next()
        obj = javaMap.get(key)
        pyDict[JUtil.javaObjToPyVal(key)] = JUtil.javaObjToPyVal(obj, customConverter)
    return pyDict

# Python -> Java conversion
    
def pyCollectionToJavaCollection(val):
    '''
    Main method registered with JUtil for conversion of collections in Python
    to Java collections.
    '''
    valtype = type(val)
    if valtype in pythonCollections :
        return True, pythonCollections[valtype](val)
    # not directly in the dict, so lets check whether they are subclasses
    for pytype in pythonCollections :
        if issubclass(pytype, valtype):
            return True, pythonCollections[valtype](val)
    return False, str(val)

def _toJavaList(val):
    '''
    Turns a Python list to a Java List
    '''
    retObj = ArrayList()
    for i in val :
        retObj.add(JUtil.pyValToJavaObj(i))
    return retObj

def _toJavaUnmodifiableList(val):
    '''
    Turns a Python tuple to a Java UnmodifiableList
    '''
    return Collections.unmodifiableList(_toJavaList(val))

def _toJavaLinkedMap(val):
    '''
    Turns a Python OrderedDict to a Java LinkedHashMap
    '''
    return __toJavaMapInternal(val, LinkedHashMap())

def _toJavaMap(val):
    '''
    Turns a Python dict to a Java HashMap
    '''
    return __toJavaMapInternal(val, HashMap())

def _toJavaSet(val):
    '''
    Turns a Python set to a Java set
    '''
    return __toJavaSetInternal(val)

def _toJavaUnmodifiableSet(val):
    '''
    Turns a Python frozenset to a Java unmodifiableset
    '''
    return Collections.unmodifiableSet(__toJavaSetInternal(val))

def __toJavaSetInternal(val):
    '''
    Does the actual conversion of the elements inside of the set or frozenset to Set
    '''
    retObj = HashSet()
    for v in val :
        retObj.add(JUtil.pyValToJavaObj(v))
    return retObj


def __toJavaMapInternal(pyDict, jmap):
    '''
    Does the actual conversion of the elements inside of the dict to Map
    '''
    for key in pyDict:
        jmap.put(JUtil.pyValToJavaObj(key), JUtil.pyValToJavaObj(pyDict[key]))
    return jmap

javaCollections = OrderedDict({'java.util.ArrayList':_toPythonList, 'java.util.Arrays$ArrayList':_toPythonList, 'java.util.Collections$UnmodifiableRandomAccessList':_toPythonTuple, 'java.util.HashMap':_toPythonDict, 'java.util.LinkedHashMap':_toPythonOrderedDict})
pythonCollections = OrderedDict({ list:_toJavaList, tuple:_toJavaUnmodifiableList, OrderedDict:_toJavaLinkedMap, dict:_toJavaMap, set:_toJavaSet, frozenset:_toJavaUnmodifiableSet })
fallbackCollections = OrderedDict({ List:_toPythonList, Map:_toPythonDict, Set:_toPythonSet })

'''
Handles other types of Java to Python conversion and back.
'''

def javaClassToPyClass(obj, customConverter=None):
    '''
    Main method to convert Java classes to Python classes that aren't already defined above.
    Registers with JUtil
    '''
    if customConverter is not None :
        return True, customConverter(obj)
    return False, obj

def pyClassToJavaClass(val):
    '''
    Main method that registers with JUtil to convert Python classes to Java classes.
    '''
    valtype = type(val)
    for pyType in pythonClasses :
        if issubclass(valtype, pyType):
            return True, pythonClasses[pyType](val)
    return False, str(val)

def _toJavaClass(val):
    '''
    Utilizes the JUtil.JavaWrapperClass object and its corresponding
    toJavaObj() method that returns a Java object.
    '''
    return val.toJavaObj()

# registers the data type for conversion to a Java class.
pythonClasses = OrderedDict({JUtil.JavaWrapperClass:_toJavaClass})
