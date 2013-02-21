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
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/09/10                      njensen       Initial Creation.
#    
# 
#

from thrift.Thrift import TType
import inspect, sys, types
import dynamicserialize
from dynamicserialize import dstypes, adapters
import SelfDescribingBinaryProtocol
import numpy

dsObjTypes = {}
                
def buildObjMap(module):
    if module.__dict__.has_key('__all__'):        
        for i in module.__all__:
            name = module.__name__ + '.' + i
            __import__(name)
            buildObjMap(sys.modules[name])
    else:         
        clzName = module.__name__[module.__name__.rfind('.') + 1:]
        clz = module.__dict__[clzName]
        tname = module.__name__
        tname = tname.replace('dynamicserialize.dstypes.', '')        
        dsObjTypes[tname] = clz

buildObjMap(dstypes)

pythonToThriftMap = {
    types.StringType: TType.STRING,
    types.IntType: TType.I32,
    types.LongType: TType.I64,
    types.ListType: TType.LIST,
    types.DictionaryType: TType.MAP,
    type(set([])): TType.SET,
    types.FloatType: SelfDescribingBinaryProtocol.FLOAT,
    #types.FloatType: TType.DOUBLE,
    types.BooleanType: TType.BOOL,
    types.InstanceType: TType.STRUCT,
    types.NoneType: TType.VOID,
    numpy.float32: SelfDescribingBinaryProtocol.FLOAT,
    numpy.int32: TType.I32,
    numpy.ndarray: TType.LIST,
    numpy.object_: TType.STRING,  # making an assumption here
    numpy.string_: TType.STRING,
    numpy.float64: TType.DOUBLE,
    numpy.int16: TType.I16,
    numpy.int8: TType.BYTE,
    numpy.int64: TType.I64
}

primitiveSupport = (TType.BYTE, TType.I16, TType.I32, TType.I64, SelfDescribingBinaryProtocol.FLOAT)

class ThriftSerializationContext:
    
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
                                    TType.STRING: self.protocol.writeString,
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
                                    SelfDescribingBinaryProtocol.FLOAT: self.protocol.readF32List
                                    }
        self.listSerializationMethod = {
                                    TType.BYTE: self.protocol.writeI8List,
                                    TType.I16: self.protocol.writeI16List,
                                    TType.I32: self.protocol.writeI32List,
                                    TType.I64: self.protocol.writeI64List,
                                    SelfDescribingBinaryProtocol.FLOAT: self.protocol.writeF32List
                                    }
    
    
    def readMessageStart(self):
        msg = self.protocol.readMessageBegin()        
        return msg[0]
    
    def readMessageEnd(self):
        self.protocol.readMessageEnd()
    
    def deserializeMessage(self):
        name = self.protocol.readStructBegin()
        name = name.replace('_', '.')
        if name.isdigit():
            obj = self._deserializeType(int(name))
            return obj
        elif adapters.classAdapterRegistry.has_key(name):
            return adapters.classAdapterRegistry[name].deserialize(self)
        elif name.find('$') > -1:
            # it's an inner class, we're going to hope it's an enum, treat it special
            fieldName, fieldType, fieldId = self.protocol.readFieldBegin()            
            if fieldName != '__enumValue__':
                raise dynamiceserialize.SerializationException("Expected to find enum payload.  Found: " + fieldName)
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
        if self.typeDeserializationMethod.has_key(b):
            return self.typeDeserializationMethod[b]()
        else:
            raise dynamicserialize.SerializationException("Unsupported type value " + str(b))
    
    
    def _deserializeField(self, structname, obj):
        fieldName, fieldType, fieldId = self.protocol.readFieldBegin()
        if fieldType == TType.STOP:
            return False
        elif fieldType != TType.VOID:
#            if adapters.fieldAdapterRegistry.has_key(structname) and adapters.fieldAdapterRegistry[structname].has_key(fieldName):
#                result = adapters.fieldAdapterRegistry[structname][fieldName].deserialize(self)
#            else:
            result = self._deserializeType(fieldType)
            lookingFor = "set" + fieldName[0].upper() + fieldName[1:]

            try:
                setMethod = getattr(obj, lookingFor)

                if callable(setMethod):
                    setMethod(result)
                else:
                    raise dynamicserialize.SerializationException("Couldn't find setter method " + lookingFor)
            except:
                raise dynamicserialize.SerializationException("Couldn't find setter method " + lookingFor)
        
        self.protocol.readFieldEnd()
        return True
    
    
    def _deserializeArray(self):
        listType, size = self.protocol.readListBegin()
        result = []
        if size:
            if listType not in primitiveSupport:
                m = self.typeDeserializationMethod[listType]
                result = [m() for n in xrange(size)]
            else:
                result = self.listDeserializationMethod[listType](size)
        self.protocol.readListEnd()
        return result
    
    def _deserializeMap(self):
        keyType, valueType, size = self.protocol.readMapBegin()
        result = {}        
        for n in xrange(size):
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
        for n in xrange(setSize):
            result.add(self.typeDeserializationMethod[TType.STRUCT]())
        self.protocol.readSetEnd()
        return result
        
    def _lookupType(self, obj):        
        pyt = type(obj)
        if pythonToThriftMap.has_key(pyt):
            return pythonToThriftMap[pyt]
        elif pyt.__module__.startswith('dynamicserialize.dstypes'):
            return pythonToThriftMap[types.InstanceType]
        else:             
            raise dynamicserialize.SerializationException("Don't know how to serialize object of type: " + str(pyt))    
                
    def serializeMessage(self, obj):
        tt = self._lookupType(obj)
                
        if tt == TType.STRUCT:
            fqn = obj.__module__.replace('dynamicserialize.dstypes.', '')
            if adapters.classAdapterRegistry.has_key(fqn):
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
                            fc = val.__module__.replace('dynamicserialize.dstypes.', '')
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
        if self.typeSerializationMethod.has_key(fieldType):
            return self.typeSerializationMethod[fieldType](fieldValue)
        else:
            raise dynamicserialize.SerializationException("Unsupported type value " + str(fieldType))
                    
    def _serializeArray(self, obj):
        size = len(obj)
        if size:
            if type(obj) is numpy.ndarray:
               t = pythonToThriftMap[obj.dtype.type]
               size = obj.size
            else:
                t = self._lookupType(obj[0])
        else:
            t = TType.STRUCT        
        self.protocol.writeListBegin(t, size)
        if t == TType.STRING:
            if type(obj) is numpy.ndarray:
                if len(obj.shape) == 1:
                    for x in obj:                    
                        s = str(x).strip()
                        self.typeSerializationMethod[t](s)
                else:                    
                    for x in obj:
                        for y in x:
                            s = str(y).strip()
                            self.typeSerializationMethod[t](s)
            else:
                for x in obj:                    
                    s = str(x)
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
        for k in obj.keys():
            self.typeSerializationMethod[TType.STRUCT](k)
            self.typeSerializationMethod[TType.STRUCT](obj[k])
        self.protocol.writeMapEnd()
    
    def _serializeSet(self, obj):
        size = len(obj)
        self.protocol.writeSetBegin(TType.VOID, size)
        for x in obj:
            self.typeSerializationMethod[TType.STRUCT](x)
        self.protocol.writeSetEnd()
    
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
        
    
    