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
# A port of the Java ThriftSerializationContext, used for reading/writing
# DynamicSerialize objects to/from thrift.
#
# For serialization, it has no knowledge of the expected types in other
# languages, it is instead all based on inspecting the types of the objects
# passed to it.  Therefore, ensure the types of python objects and primitives
# match what they should be in the destination language.
#
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/09/10                      njensen       Initial Creation.
#    06/12/13         #2099        dgilling      Implement readObject() and
#                                                writeObject().
#    Apr 24, 2015    4425          nabowle       Add Double support
#    Oct 17, 2016    5919          njensen       Optimized for speed
#    Oct 11, 2018    7306          dgilling      Fix handling of ndarrays in 
#                                                _serializeArray.
#    Apr 30, 2019    7814          dgilling      Serialize python unicode strings.
#    Jun 26, 2019    7888          tgurney       Python 3 fixes
#    Jul 23, 2019    7888          tgurney       Fix handling of bytes as strings
#    Sep 13, 2019    7888          tgurney       Fix serialization of bytes
#
#

from thrift.Thrift import TType
import inspect
import sys
import dynamicserialize
from dynamicserialize import dstypes, adapters
from . import SelfDescribingBinaryProtocol
import numpy

DS_LEN = len('dynamicserialize.dstypes.')

dsObjTypes = {}


def buildObjMap(module):
    if '__all__' in module.__dict__:
        for i in module.__all__:
            name = module.__name__ + '.' + i
            __import__(name)
            buildObjMap(sys.modules[name])
    else:
        clzName = module.__name__[module.__name__.rfind('.') + 1:]
        clz = module.__dict__[clzName]
        tname = module.__name__
        tname = tname[DS_LEN:]
        dsObjTypes[tname] = clz

buildObjMap(dstypes)

pythonToThriftMap = {
    bytes: TType.LIST,
    str: TType.STRING,
    int: TType.I64,
    list: TType.LIST,
    dict: TType.MAP,
    set: TType.SET,
    float: SelfDescribingBinaryProtocol.FLOAT,
    # types.FloatType: TType.DOUBLE,
    bool: TType.BOOL,
    object: TType.STRUCT,
    type(None): TType.VOID,
    numpy.float32: SelfDescribingBinaryProtocol.FLOAT,
    numpy.int32: TType.I32,
    numpy.ndarray: TType.LIST,
    numpy.object_: TType.STRING,  # making an assumption here
    numpy.string_: TType.STRING,
    # numpy.bytes_ is the same as numpy.string_
    numpy.unicode_: TType.STRING,
    # numpy.str_ is the same as numpy.unicode_
    numpy.float64: TType.DOUBLE,
    numpy.int16: TType.I16,
    numpy.int8: TType.BYTE,
    numpy.int64: TType.I64
}

primitiveSupport = (TType.BYTE, TType.I16, TType.I32, TType.I64,
                    SelfDescribingBinaryProtocol.FLOAT, TType.DOUBLE)

BYTE_STRING_TYPES = {numpy.string_, numpy.bytes_, bytes}


class ThriftSerializationContext(object):

    def __init__(self, serializationManager, selfDescribingBinaryProtocol):
        self.serializationManager = serializationManager
        self.protocol = selfDescribingBinaryProtocol
        self.typeDeserializationMethod = {
            TType.STRING: self.protocol.readString,
            TType.I16: self.protocol.readI16,
            TType.I32: self.protocol.readI32,
            TType.LIST: self._deserializeArray,
            TType.MAP: self._deserializeMap,
            TType.SET: self._deserializeSet,
            SelfDescribingBinaryProtocol.FLOAT: self.protocol.readFloat,
            TType.BYTE: self.protocol.readByte,
            TType.I64: self.protocol.readI64,
            TType.DOUBLE: self.protocol.readDouble,
            TType.BOOL: self.protocol.readBool,
            TType.STRUCT: self.deserializeMessage,
            TType.VOID: lambda: None
        }
        self.typeSerializationMethod = {
            TType.STRING: self.writeString,
            TType.I16: self.protocol.writeI16,
            TType.I32: self.protocol.writeI32,
            TType.LIST: self._serializeArray,
            TType.MAP: self._serializeMap,
            TType.SET: self._serializeSet,
            SelfDescribingBinaryProtocol.FLOAT: self.protocol.writeFloat,
            TType.BYTE: self.protocol.writeByte,
            TType.I64: self.protocol.writeI64,
            TType.DOUBLE: self.protocol.writeDouble,
            TType.BOOL: self.protocol.writeBool,
            TType.STRUCT: self.serializeMessage,
            TType.VOID: lambda x: None
        }
        self.listDeserializationMethod = {
            TType.BYTE: self.protocol.readI8List,
            TType.I16: self.protocol.readI16List,
            TType.I32: self.protocol.readI32List,
            TType.I64: self.protocol.readI64List,
            SelfDescribingBinaryProtocol.FLOAT: self.protocol.readF32List,
            TType.DOUBLE: self.protocol.readF64List
        }
        self.listSerializationMethod = {
            TType.BYTE: self.protocol.writeI8List,
            TType.I16: self.protocol.writeI16List,
            TType.I32: self.protocol.writeI32List,
            TType.I64: self.protocol.writeI64List,
            SelfDescribingBinaryProtocol.FLOAT: self.protocol.writeF32List,
            TType.DOUBLE: self.protocol.writeF64List
        }

    def readMessageStart(self):
        msg = self.protocol.readMessageBegin()
        return msg[0]

    def readMessageEnd(self):
        self.protocol.readMessageEnd()

    def deserializeMessage(self):
        name = self.protocol.readStructBegin()
        if name.isdigit():
            obj = self._deserializeType(int(name))
            return obj
        name = name.replace('_', '.')
        if name in adapters.classAdapterRegistry:
            return adapters.classAdapterRegistry[name].deserialize(self)
        elif '$' in name:
            # it's an inner class, we're going to hope it's an enum, treat it
            # special
            fieldName, fieldType, fieldId = self.protocol.readFieldBegin()
            if fieldName != '__enumValue__':
                raise dynamiceserialize.SerializationException(
                    "Expected to find enum payload.  Found: " + fieldName)
            obj = self.protocol.readString()
            self.protocol.readFieldEnd()
            return obj
        else:
            clz = dsObjTypes[name]
            obj = clz()

        while self._deserializeField(name, obj):
            pass

        self.protocol.readStructEnd()
        return obj

    def _deserializeType(self, b):
        try:
            return self.typeDeserializationMethod[b]()
        except KeyError:
            raise dynamicserialize.SerializationException(
                "Unsupported type value " + str(b))

    def _deserializeField(self, structname, obj):
        fieldName, fieldType, fieldId = self.protocol.readFieldBegin()
        if fieldType == TType.STOP:
            return False
        elif fieldType != TType.VOID:
            result = self._deserializeType(fieldType)
            lookingFor = "set" + fieldName[0].upper() + fieldName[1:]

            try:
                setMethod = getattr(obj, lookingFor)
                setMethod(result)                
            except:
                raise dynamicserialize.SerializationException(
                    "Couldn't find setter method " + lookingFor)

        self.protocol.readFieldEnd()
        return True

    def _deserializeArray(self):
        listType, size = self.protocol.readListBegin()
        result = []
        if size:
            if listType not in primitiveSupport:
                m = self.typeDeserializationMethod[listType]
                result = [m() for n in range(size)]
            else:
                result = self.listDeserializationMethod[listType](size)
        self.protocol.readListEnd()
        return result

    def _deserializeMap(self):
        keyType, valueType, size = self.protocol.readMapBegin()
        result = {}
        for n in range(size):
            # can't go off the type, due to java generics limitations dynamic serialize is
            # serializing keys and values as void
            key = self.typeDeserializationMethod[TType.STRUCT]()
            value = self.typeDeserializationMethod[TType.STRUCT]()
            result[key] = value
        self.protocol.readMapEnd()
        return result

    def _deserializeSet(self):
        setType, setSize = self.protocol.readSetBegin()
        result = set([])
        for n in range(setSize):
            result.add(self.typeDeserializationMethod[TType.STRUCT]())
        self.protocol.readSetEnd()
        return result

    def _lookupType(self, obj):
        pyt = type(obj)
        if pyt in pythonToThriftMap:
            return pythonToThriftMap[pyt]
        elif pyt.__module__[:DS_LEN - 1] == ('dynamicserialize.dstypes'):
            return pythonToThriftMap[object]
        else:
            raise dynamicserialize.SerializationException(
                "Don't know how to serialize object of type: " + str(pyt))

    def serializeMessage(self, obj):
        tt = self._lookupType(obj)

        if tt == TType.STRUCT:
            fqn = obj.__module__[DS_LEN:]
            if fqn in adapters.classAdapterRegistry:
                # get proper class name when writing class name to serialization stream
                # in case we have a special inner-class case
                m = sys.modules[adapters.classAdapterRegistry[fqn].__name__]
                if isinstance(m.ClassAdapter, list):
                    fqn = m.ClassAdapter[0]
                self.protocol.writeStructBegin(fqn)
                adapters.classAdapterRegistry[fqn].serialize(self, obj)
                return
            else:
                self.protocol.writeStructBegin(fqn)
                methods = inspect.getmembers(obj, inspect.ismethod)
                fid = 1
                for m in methods:
                    methodName = m[0]
                    if methodName.startswith('get'):
                        fieldname = methodName[3].lower() + methodName[4:]
                        val = m[1]()
                        ft = self._lookupType(val)
                        if ft == TType.STRUCT:
                            self._serializeField(fieldname, ft, fid, val)
                        else:
                            self._serializeField(fieldname, ft, fid, val)
                        fid += 1
                self.protocol.writeFieldStop()

                self.protocol.writeStructEnd()
        else:
            # basic types
            self.protocol.writeStructBegin(str(tt))
            self._serializeType(obj, tt)
            self.protocol.writeStructEnd()

    def _serializeField(self, fieldName, fieldType, fieldId, fieldValue):
        self.protocol.writeFieldBegin(fieldName, fieldType, fieldId)
        self._serializeType(fieldValue, fieldType)
        self.protocol.writeFieldEnd()

    def _serializeType(self, fieldValue, fieldType):
        if fieldType in self.typeSerializationMethod:
            return self.typeSerializationMethod[fieldType](fieldValue)
        else:
            raise dynamicserialize.SerializationException(
                "Unsupported type value " + str(fieldType))

    def _serializeArray(self, obj):
        size = len(obj)
        objType = type(obj)
        if objType is numpy.ndarray:
            t = pythonToThriftMap[obj.dtype.type]
            size = obj.size
        elif objType is bytes:
            t = TType.BYTE
            obj = list(obj)
        elif size:
            t = self._lookupType(obj[0])
        else:
            t = TType.STRUCT
        self.protocol.writeListBegin(t, size)
        if t == TType.STRING:
            # For TType.STRING we always assume that if the objType is bytes we
            # want to decode it into a str
            if objType is numpy.ndarray:
                if len(obj.shape) == 1:
                    for x in obj:
                        s = self._decodeString(x).strip()
                        self.typeSerializationMethod[t](s)
                else:
                    for x in obj:
                        for y in x:
                            s = self._decodeString(y).strip()
                            self.typeSerializationMethod[t](s)
            else:
                for x in obj:
                    s = self._decodeString(x)
                    self.typeSerializationMethod[t](s)
        elif t not in primitiveSupport:
            for x in obj:
                self.typeSerializationMethod[t](x)
        else:
            self.listSerializationMethod[t](obj)
        self.protocol.writeListEnd()

    def _serializeMap(self, obj):
        size = len(obj)
        self.protocol.writeMapBegin(TType.VOID, TType.VOID, size)
        for (key, value) in obj.items():
            self.typeSerializationMethod[TType.STRUCT](key)
            self.typeSerializationMethod[TType.STRUCT](value)
        self.protocol.writeMapEnd()

    def _serializeSet(self, obj):
        size = len(obj)
        self.protocol.writeSetBegin(TType.VOID, size)
        for x in obj:
            self.typeSerializationMethod[TType.STRUCT](x)
        self.protocol.writeSetEnd()

    def _decodeString(self, s):
        """If s is a byte string, return s.decode(). Otherwise return str(s)"""
        if type(s) in BYTE_STRING_TYPES:
            return s.decode()
        else:
            return str(s)

    def writeMessageStart(self, name):
        self.protocol.writeMessageBegin(name, TType.VOID, 0)

    def writeMessageEnd(self):
        self.protocol.writeMessageEnd()

    def readBool(self):
        return self.protocol.readBool()

    def writeBool(self, b):
        self.protocol.writeBool(b)

    def readByte(self):
        return self.protocol.readByte()

    def writeByte(self, b):
        self.protocol.writeByte(b)

    def readDouble(self):
        return self.protocol.readDouble()

    def writeDouble(self, d):
        self.protocol.writeDouble(d)

    def readFloat(self):
        return self.protocol.readFloat()

    def writeFloat(self, f):
        self.protocol.writeFloat(f)

    def readI16(self):
        return self.protocol.readI16()

    def writeI16(self, i):
        self.protocol.writeI16(i)

    def readI32(self):
        return self.protocol.readI32()

    def writeI32(self, i):
        self.protocol.writeI32(i)

    def readI64(self):
        return self.protocol.readI64()

    def writeI64(self, i):
        self.protocol.writeI64(i)

    def readString(self):
        return self.protocol.readString()

    def writeString(self, s):
        s = self._decodeString(s)
        self.protocol.writeString(s)

    def readBinary(self):
        numBytes = self.protocol.readI32()
        return self.protocol.readI8List(numBytes)

    def readFloatArray(self):
        size = self.protocol.readI32()
        return self.protocol.readF32List(size)

    def writeFloatArray(self, floats):
        self.protocol.writeI32(len(floats))
        self.protocol.writeF32List(floats)

    def readObject(self):
        return self.deserializeMessage()

    def writeObject(self, obj):
        self.serializeMessage(obj)
