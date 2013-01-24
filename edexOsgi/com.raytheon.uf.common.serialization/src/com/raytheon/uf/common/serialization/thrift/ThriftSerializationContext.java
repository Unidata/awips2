/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.serialization.thrift;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.sf.cglib.beans.BeanMap;
import net.sf.cglib.reflect.FastClass;

import com.facebook.thrift.TException;
import com.facebook.thrift.protocol.TField;
import com.facebook.thrift.protocol.TList;
import com.facebook.thrift.protocol.TMap;
import com.facebook.thrift.protocol.TMessage;
import com.facebook.thrift.protocol.TSet;
import com.facebook.thrift.protocol.TStruct;
import com.facebook.thrift.protocol.TType;
import com.raytheon.uf.common.serialization.BaseSerializationContext;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.EnclosureType;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationMetadata;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationCache;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Provides a serialization capability based on the Thrift Binary Serialization
 * Format
 * 
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 12, 2008 #1448       chammack    Initial creation
 * Jun 17, 2010 #5091       njensen     Optimized primitive arrays
 * Mar 01, 2011             njensen     Restructured deserializeArray()
 * Sep 14, 2012 #1169       djohnson    Add ability to write another object into the stream directly.
 * Sep 28, 2012 #1195       djohnson    Add ability to specify adapter at field level.
 * Nov 02, 2012 1302        djohnson    No more field level adapters.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
// Warnings are suppressed in this class because generics cause issues with the
// extensive use of reflection. The erased objects are used instead.
@SuppressWarnings({ "unchecked", "rawtypes" })
public class ThriftSerializationContext extends BaseSerializationContext {

    /** The tag that is used to indicate the value of an enumeration */
    private static final String ENUM_VALUE_TAG = "__enumValue__";

    private final SelfDescribingBinaryProtocol protocol;

    private static Map<Class<?>, Byte> types;

    private static Map<String, Class<?>> fieldClass = new ConcurrentHashMap<String, Class<?>>();

    /** Mapping of built in java types to thift types */
    static {
        types = new HashMap<Class<?>, Byte>();
        types.put(String.class, TType.STRING);
        types.put(Integer.class, TType.I32);
        types.put(Integer.TYPE, TType.I32);
        types.put(Long.class, TType.I64);
        types.put(Long.TYPE, TType.I64);
        types.put(Short.class, TType.I16);
        types.put(Short.TYPE, TType.I16);
        types.put(Byte.class, TType.BYTE);
        types.put(Byte.TYPE, TType.BYTE);
        types.put(Float.class, SelfDescribingBinaryProtocol.FLOAT);
        types.put(Float.TYPE, SelfDescribingBinaryProtocol.FLOAT);
        types.put(Double.class, TType.DOUBLE);
        types.put(Double.TYPE, TType.DOUBLE);
        types.put(Boolean.class, TType.BOOL);
        types.put(Boolean.TYPE, TType.BOOL);
    }

    /**
     * Constructor
     * 
     * @param protocol
     * @param serializationManager
     */
    public ThriftSerializationContext(SelfDescribingBinaryProtocol protocol,
            DynamicSerializationManager serializationManager) {
        super(serializationManager);

        this.protocol = protocol;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readBinary()
     */
    @Override
    public byte[] readBinary() throws SerializationException {
        try {
            return this.protocol.readBinary();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readBool()
     */
    @Override
    public boolean readBool() throws SerializationException {
        try {
            return this.protocol.readBool();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readByte()
     */
    @Override
    public byte readByte() throws SerializationException {
        try {
            return this.protocol.readByte();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readDouble()
     */
    @Override
    public double readDouble() throws SerializationException {
        try {
            return this.protocol.readDouble();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readFloat()
     */
    @Override
    public float readFloat() throws SerializationException {
        try {
            return this.protocol.readFloat();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readI16()
     */
    @Override
    public short readI16() throws SerializationException {
        try {
            return this.protocol.readI16();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readI32()
     */
    @Override
    public int readI32() throws SerializationException {
        try {
            return this.protocol.readI32();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readI64()
     */
    @Override
    public long readI64() throws SerializationException {
        try {
            return this.protocol.readI64();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readString()
     */
    @Override
    public String readString() throws SerializationException {
        try {
            return this.protocol.readString();
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.serialize.ISerializationContext#writeBinary(byte[])
     */
    @Override
    public void writeBinary(byte[] arg0) throws SerializationException {
        try {
            this.protocol.writeBinary(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContext#writeBool(boolean)
     */
    @Override
    public void writeBool(boolean arg0) throws SerializationException {
        try {
            this.protocol.writeBool(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContext#writeByte(byte)
     */
    @Override
    public void writeByte(byte arg0) throws SerializationException {
        try {
            this.protocol.writeByte(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.serialize.ISerializationContext#writeDouble(double)
     */
    @Override
    public void writeDouble(double arg0) throws SerializationException {
        try {
            this.protocol.writeDouble(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContext#writeFloat(float)
     */
    @Override
    public void writeFloat(float arg0) throws SerializationException {
        try {
            this.protocol.writeFloat(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContext#writeI16(short)
     */
    @Override
    public void writeI16(short arg0) throws SerializationException {
        try {
            this.protocol.writeI16(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContext#writeI32(int)
     */
    @Override
    public void writeI32(int arg0) throws SerializationException {
        try {
            this.protocol.writeI32(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContext#writeI64(long)
     */
    @Override
    public void writeI64(long arg0) throws SerializationException {
        try {
            this.protocol.writeI64(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.serialize.ISerializationContext#writeString(java.lang
     * .String)
     */
    @Override
    public void writeString(String arg0) throws SerializationException {
        try {
            this.protocol.writeString(arg0);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /**
     * Serializes a specific type
     * 
     * @param val
     *            the value to serialize
     * @param valClass
     *            the class object
     * @param type
     *            the type
     * @throws SerializationException
     */
    private void serializeType(Object val, Class<?> valClass, byte type)
            throws SerializationException {
        try {
            switch (type) {
            case TType.BYTE:
                protocol.writeByte((Byte) val);
                return;
            case TType.STRING:
                protocol.writeString((String) val);
                return;
            case TType.I32:
                protocol.writeI32((Integer) val);
                return;
            case TType.I16:
                protocol.writeI16((Short) val);
                return;
            case TType.I64:
                protocol.writeI64((Long) val);
                return;
            case TType.BOOL:
                protocol.writeBool((Boolean) val);
                return;
            case SelfDescribingBinaryProtocol.FLOAT:
                protocol.writeFloat(((Number) val).floatValue());
                return;
            case TType.DOUBLE:
                protocol.writeDouble(((Number) val).doubleValue());
                return;
            case TType.STRUCT:
                this.serializationManager.serialize(this, val);
                return;
            case TType.SET:
                Set<?> set = ((Set<?>) val);
                Iterator<?> iterator = set.iterator();
                TSet setObj = new TSet(TType.VOID, set.size());
                protocol.writeSetBegin(setObj);
                while (iterator.hasNext()) {
                    Object v = iterator.next();
                    this.serializationManager.serialize(this, v);
                }
                protocol.writeSetEnd();
                return;
            case TType.MAP:
                Map<?, ?> map = ((Map<?, ?>) val);
                TMap tmap = new TMap(TType.VOID, TType.VOID, map.size());
                protocol.writeMapBegin(tmap);
                for (Entry<?, ?> entry : map.entrySet()) {
                    this.serializationManager.serialize(this, entry.getKey());
                    this.serializationManager.serialize(this, entry.getValue());
                }
                protocol.writeMapEnd();
                return;
            case TType.LIST:
                serializeArray(val, valClass);
                return;
            case TType.VOID:
                return;
            default:
                throw new SerializationException("Unknown type! " + val);
            }
        } catch (TException e) {
            throw new SerializationException(
                    "Error occurred during serialization of base type", e);
        }

    }

    /**
     * Serialize an array/List
     * 
     * 
     * @param val
     *            the object
     * @param valClass
     *            the class of the object
     * @throws TException
     *             if writing error occurs
     * @throws SerializationException
     */
    private void serializeArray(Object val, Class<?> valClass)
            throws TException, SerializationException {
        Iterator<?> iterator;
        int i;
        if (valClass.isArray()) {
            Class<?> c = valClass.getComponentType();
            Byte b = lookupType(c);
            int arrayLength = Array.getLength(val);

            if (b == null) {
                // Assume they are objects... throw exceptions if one of
                // the components isn't serializable when it comes time
                // to serialize it
                b = TType.STRUCT;
            }

            TList list = new TList(b, arrayLength);
            protocol.writeListBegin(list);

            // For speed, handle all the primitive types and Strings in the most
            // optimized way
            if (c.equals(Float.TYPE)) {
                float[] d = (float[]) val;
                protocol.writeF32List(d);
            } else if (c.equals(Double.TYPE)) {
                double[] d = (double[]) val;
                protocol.writeD64List(d);
            } else if (c.equals(Byte.TYPE)) {
                byte[] d = (byte[]) val;
                protocol.writeI8List(d);
            } else if (c.equals(Integer.TYPE)) {
                int[] d = (int[]) val;
                protocol.writeI32List(d);
            } else if (c.equals(Short.TYPE)) {
                short[] d = (short[]) val;
                protocol.writeI16List(d);
            } else if (c.equals(Long.TYPE)) {
                long[] d = (long[]) val;
                protocol.writeI64List(d);
            } else if (c.equals(String.class)) {
                String[] d = (String[]) val;
                for (int z = 0; z < arrayLength; z++) {
                    protocol.writeString(d[z]);
                }
            } else {
                // Do it using reflection for objects
                for (int k = 0; k < arrayLength; k++) {
                    serializeType(Array.get(val, k), c, b);
                }
            }

            protocol.writeListEnd();
        } else {
            iterator = ((List<?>) val).iterator();
            TList list = new TList(TType.STRUCT, ((List<?>) val).size());
            protocol.writeListBegin(list);
            i = 0;
            while (iterator.hasNext()) {
                Object v = iterator.next();
                this.serializationManager.serialize(this, v);
                i++;
            }
            protocol.writeListEnd();
        }
    }

    public Byte lookupType(Class<?> clazz) {
        Byte b = types.get(clazz);
        if (b == null) {
            if (clazz.isArray()) {
                b = TType.LIST;
            } else {
                SerializationMetadata md = this.serializationManager
                        .getSerializationMetadata(clazz.getName());
                if (md != null || clazz.isEnum()) {
                    b = TType.STRUCT;
                } else {
                    Class<?> superClazz = clazz.getSuperclass();
                    while (superClazz != null && md == null) {
                        md = this.serializationManager
                                .getSerializationMetadata(superClazz.getName());
                        if (md == null) {
                            superClazz = superClazz.getSuperclass();
                        }
                    }
                    if (md != null) {
                        b = TType.STRUCT;
                    } else if (Set.class.isAssignableFrom(clazz)) {
                        b = TType.SET;
                    } else if (List.class.isAssignableFrom(clazz)) {
                        b = TType.LIST;
                    } else if (Map.class.isAssignableFrom(clazz)) {
                        b = TType.MAP;
                    }
                }
            }
        }

        return b;
    }

    /**
     * Serialize a message
     * 
     * @param obj
     *            the object
     * @param beanMap
     *            the beanmap of the object
     * @param metadata
     *            the object's metadata
     * @throws SerializationException
     */
    public void serializeMessage(Object obj, BeanMap beanMap,
            SerializationMetadata metadata) throws SerializationException {
        try {
            // Determine the type of the message

            Byte b = null;
            if (obj == null) {
                b = TType.VOID;
            } else {
                b = lookupType(obj.getClass());
            }
            if (b == null) {
                throw new SerializationException(
                        "Don't know how to serialize class: " + obj.getClass());
            }

            // If it is a struct, determine if we know enough about the class
            // (it is properly annotated or it has a factory) for it to be
            // serialized

            if (b == TType.STRUCT) {
                if (metadata != null && metadata.serializationFactory != null) {
                    // we need to encode the struct name to something
                    // deserialization can recognize, for instance
                    // java.nio.FloatBuffer instead of
                    // java.nio.DirectFloatBufferS
                    String structName = metadata.adapterStructName.replace('.',
                            '_');
                    TStruct struct = new TStruct(structName);
                    protocol.writeStructBegin(struct);

                    Object o = obj;
                    ISerializationTypeAdapter fact = metadata.serializationFactory;
                    fact.serialize(this, o);
                    return;
                }

                // Must remove "." to be cross platform
                String structName = obj.getClass().getName().replace('.', '_');

                TStruct struct = new TStruct(structName);
                protocol.writeStructBegin(struct);

                // Determine if the class is really an enum, if so, serialize it
                // in a simple way
                if (obj.getClass().isEnum()) {

                    TField enumValueField = new TField(ENUM_VALUE_TAG,
                            TType.STRING, (short) 0);
                    protocol.writeFieldBegin(enumValueField);
                    protocol.writeString(((Enum) obj).name());
                    protocol.writeFieldEnd();

                } else {
                    // Otherwise it is a class

                    // Look at all the fields available
                    // Serialize all of the remaining fields
                    short id = 1;
                    for (String keyStr : metadata.serializedAttributes) {

                        Object val = beanMap.get(keyStr);

                        Class<?> valClass = null;
                        Byte type = null;
                        ISerializationTypeAdapter attributeFactory = null;
                        // Determine if we know how to serialize this field

                        if (val != null) {
                            valClass = val.getClass();

                            type = lookupType(valClass);
                            attributeFactory = metadata.attributesWithFactories
                                    .get(keyStr);
                            if (type == null && attributeFactory == null) {

                                throw new SerializationException(
                                        "Unable to find serialization for "
                                                + valClass.getName());
                            }

                            // If it's not a first class type or has a
                            // serialization factory, assume struct for now, if
                            // there is no tags we'll find out soon
                            if (type == null) {
                                type = TType.STRUCT;
                            }
                        } else {
                            // Data is null
                            type = TType.VOID;
                        }

                        // Perform actual serialization
                        serializeField(val, valClass, type, keyStr,
                                attributeFactory, id);
                        id++;

                    }

                    protocol.writeFieldStop();
                }
                protocol.writeStructEnd();
            } else {
                // Wrap basic types with a struct with the typeId as the name
                // This guarantees you know what you're getting even if they
                // declare their list as List<Object>
                TStruct tstruct = new TStruct("" + b);
                protocol.writeStructBegin(tstruct);
                Class<?> clazz = null;
                if (obj != null) {
                    clazz = obj.getClass();
                }
                serializeType(obj, clazz, b);
                protocol.writeStructEnd();
            }
        } catch (TException e) {
            throw new SerializationException("Serialization failed", e);
        }
    }

    /**
     * Serialize a field
     * 
     * @param val
     * @param valClass
     * @param type
     * @param keyStr
     * @param adapter
     * @return
     * @throws TException
     * @throws SerializationException
     */
    private boolean serializeField(Object val, Class<?> valClass, byte type,
            String keyStr, ISerializationTypeAdapter adapter, short id)
            throws TException, SerializationException {
        TField field = new TField();
        field.type = type;
        field.id = id;
        field.name = keyStr;
        protocol.writeFieldBegin(field);

        if (type != TType.VOID) {
            // Otherwise, as long as it's not void, use basic type serialization
            serializeType(val, valClass, type);
        }
        protocol.writeFieldEnd();

        return false;
    }

    /**
     * Deserialize a message (with headers)
     * 
     * @return the deserialized object
     * @throws SerializationException
     */
    public Object deserializeMessage() throws SerializationException {
        Object retObj = null;
        TStruct struct = protocol.readStructBegin();

        String structName = struct.name.replace('_', '.');

        char c0 = structName.charAt(0);

        if (Character.isDigit(c0)) {
            // since fields/methods in java can't start with numeric, this means
            // that this struct contains a numerical value indicating the type
            byte b = Byte.parseByte(structName);
            Object obj = deserializeType(b, null, null, "",
                    EnclosureType.COLLECTION);
            return obj;
        }
        SerializationMetadata md = this.serializationManager
                .getSerializationMetadata(structName);

        FastClass fc;
        try {
            fc = SerializationCache.getFastClass(structName);
        } catch (Exception e) {
            throw new SerializationException("Unable to load class: "
                    + structName, e);
        }

        if (md == null) {
            // check to see if superclass has an adapter
            Class<?> superClazz = fc.getJavaClass().getSuperclass();
            while (superClazz != null && md == null) {
                md = this.serializationManager
                        .getSerializationMetadata(superClazz.getName());
                superClazz = superClazz.getSuperclass();
            }
        }

        if (md != null) {
            if (md.serializationFactory != null) {
                ISerializationTypeAdapter factory = md.serializationFactory;
                return factory.deserialize(this);
            }
        } else if (!fc.getJavaClass().isEnum()) {
            throw new SerializationException("metadata is null for "
                    + structName);
        }

        Object o = null;
        BeanMap bm = null;

        try {

            if (fc.getJavaClass().isEnum()) {
                // an enum
                try {
                    TField enumField = protocol.readFieldBegin();
                    if (!enumField.name.equals(ENUM_VALUE_TAG)) {
                        throw new SerializationException(
                                "Expected to find enum payload.  Found: "
                                        + enumField.name);
                    }
                    Object retVal = Enum.valueOf(fc.getJavaClass(),
                            protocol.readString());
                    protocol.readFieldEnd();
                    return retVal;
                } catch (Exception e) {
                    throw new SerializationException(
                            "Error constructing enum enum", e);
                }

            } else {
                // a "regular" class
                try {
                    o = fc.newInstance();
                    bm = SerializationCache.getBeanMap(o);
                } catch (Exception e) {
                    throw new SerializationException(
                            "Error instantiating class: " + struct.name, e);
                }

                while (deserializeField(md, o, fc, bm)) {
                    ;
                }

            }

            protocol.readStructEnd();
        } catch (TException e) {
            throw new SerializationException("Error deserializing class "
                    + structName, e);
        } finally {
            if (bm != null && o != null) {
                retObj = bm.getBean();
                SerializationCache.returnBeanMap(bm, o);
            }
        }

        return retObj;
    }

    /**
     * Deserialize a field
     * 
     * @param md
     * @param o
     * @param fc
     * @param bm
     * @throws TException
     * @throws SerializationException
     */
    private boolean deserializeField(SerializationMetadata md, Object o,
            FastClass fc, BeanMap bm) throws TException, SerializationException {

        TField field = protocol.readFieldBegin();
        Object obj = null;

        if (field.type == TType.STOP) {
            return false;
        }

        if (field.type != TType.VOID) {
            obj = deserializeType(field.type, o.getClass(), fc, field.name,
                    EnclosureType.FIELD);
            if (field.type == TType.STRING) {
                Class<?> fieldClass = findFieldClass(o.getClass(), field.name);
                if (fieldClass != null && fieldClass.isEnum()) {
                    // special case to handle Strings sent from python and
                    // transform them into enums, since python had no
                    // knowledge of whether a string should translate
                    // to a string or enum in java
                    obj = Enum.valueOf((Class<Enum>) fieldClass, (String) obj);
                }
            }
            bm.put(field.name, obj);
        }
        protocol.readFieldEnd();

        return true;
    }

    /**
     * Deserialize a type
     * 
     * @param type
     * @param clazz
     * @param fclazz
     * @param fieldName
     * @param enclosureType
     * @return
     * @throws SerializationException
     */
    private Object deserializeType(byte type, Class clazz, FastClass fclazz,
            String fieldName, EnclosureType enclosureType)
            throws SerializationException {
        switch (type) {
        case TType.STRING: {
            try {
                return protocol.readString();
            } catch (TException e) {
                throw new SerializationException("Error reading string", e);
            }
        }
        case TType.I16: {
            try {
                return protocol.readI16();
            } catch (TException e) {
                throw new SerializationException("Error reading short", e);
            }
        }
        case TType.I32: {
            try {
                return protocol.readI32();
            } catch (TException e) {
                throw new SerializationException("Error reading int", e);
            }
        }
        case TType.LIST: {
            return deserializeArray(fclazz, fieldName);
        }
        case TType.MAP: {
            // Since Java 1.5+ did not expose generics information at runtime,
            // we must assume an erased type, and do reflection on every
            // component of the key and value pair.
            Map map = null;
            try {
                TMap tmap = protocol.readMapBegin();
                if (fclazz != null) {
                    Class<?> clazzToTry = fclazz.getJavaClass();
                    boolean fieldFound = false;
                    while (!fieldFound && clazzToTry != null) {
                        Field listField = null;
                        try {
                            listField = clazzToTry.getDeclaredField(fieldName);
                        } catch (NoSuchFieldException e) {
                            // try super class
                            clazzToTry = clazzToTry.getSuperclass();
                            continue;
                        }
                        Class<?> fieldClazz = listField.getType();
                        if (!fieldClazz.isInterface()
                                && Map.class.isAssignableFrom(fieldClazz)) {
                            map = (Map) fieldClazz.newInstance();
                        }
                        fieldFound = true;
                    }

                    if (!fieldFound) {
                        throw new NoSuchFieldException(fieldName);
                    }
                }

                if (map == null) {
                    // assume hashmap if nothing else available
                    map = new HashMap(tmap.size);
                }

                for (int i = 0; i < tmap.size; i++) {
                    Object key = this.serializationManager.deserialize(this);
                    Object val = this.serializationManager.deserialize(this);
                    map.put(key, val);
                }
            } catch (Exception e) {
                throw new SerializationException("Error deserializing map", e);
            }
            return map;
        }
        case TType.SET: {
            Set set = null;
            try {
                TSet tset = protocol.readSetBegin();

                // Since Java 1.5+ did not expose generics information at
                // runtime,
                // we must assume an erased type, and do reflection on every
                // component of the set.
                if (fclazz != null) {
                    Class<?> clazzToTry = fclazz.getJavaClass();
                    boolean fieldFound = false;
                    while (!fieldFound && clazzToTry != null) {
                        Field listField = null;
                        try {
                            listField = clazzToTry.getDeclaredField(fieldName);
                        } catch (NoSuchFieldException e) {
                            // try super class
                            clazzToTry = clazzToTry.getSuperclass();
                            continue;
                        }
                        Class<?> fieldClazz = listField.getType();
                        if (!fieldClazz.isInterface()
                                && Set.class.isAssignableFrom(fieldClazz)) {
                            set = (Set) fieldClazz.newInstance();
                        }
                        fieldFound = true;
                    }

                    if (!fieldFound) {
                        throw new NoSuchFieldException(fieldName);
                    }
                }

                if (set == null) {
                    // assume hashset if nothing else available
                    set = new HashSet(tset.size);
                }

                for (int i = 0; i < tset.size; i++) {
                    set.add(this.serializationManager.deserialize(this));
                }

                protocol.readSetEnd();
                return set;
            } catch (Exception e) {
                throw new SerializationException("Error deserializing set", e);
            }
        }
        case SelfDescribingBinaryProtocol.FLOAT: {
            try {
                return protocol.readFloat();
            } catch (TException e) {
                throw new SerializationException("Error reading double", e);
            }
        }
        case TType.BYTE: {
            try {
                return protocol.readByte();
            } catch (TException e) {
                throw new SerializationException("Error reading double", e);
            }
        }
        case TType.I64: {
            try {
                return protocol.readI64();
            } catch (TException e) {
                throw new SerializationException("Error reading double", e);
            }
        }
        case TType.DOUBLE: {
            try {
                return protocol.readDouble();
            } catch (TException e) {
                throw new SerializationException("Error reading double", e);
            }
        }
        case TType.BOOL: {
            try {
                return protocol.readBool();
            } catch (TException e) {
                throw new SerializationException("Error reading boolean", e);
            }
        }
        case TType.STRUCT: {
            return this.serializationManager.deserialize(this);
        }
        case TType.VOID: {
            return null;
        }
        default:
            throw new SerializationException("Unhandled type: " + type);
        }

    }

    private Class<?> findFieldClass(Class<?> clazz, String fieldName) {
        String key = clazz.getName() + "." + fieldName;
        Class<?> rval = fieldClass.get(key);

        if (rval != null) {
            return rval;
        }

        rval = clazz;

        while (rval != null) {
            Field listField = null;
            try {
                listField = rval.getDeclaredField(fieldName);
            } catch (NoSuchFieldException e) {
                // try super class
                rval = rval.getSuperclass();
                continue;
            }
            rval = listField.getType();
            fieldClass.put(key, rval);
            return rval;
        }

        return null;
    }

    /**
     * Deserialize an array
     * 
     * @param fclazz
     * @param fieldName
     * @return
     * @throws SerializationException
     */
    private Object deserializeArray(FastClass fclazz, String fieldName)
            throws SerializationException {
        try {
            TList innerList = protocol.readListBegin();
            // System.out.println("List sz: " + innerList.size);

            // Determine whether the list is really an array or if it is
            // a list.
            Class<?> listFieldClazz = null;
            Field listField = null;

            if (fclazz != null) {
                Class c = fclazz.getJavaClass();
                do {
                    try {
                        listField = c.getDeclaredField(fieldName);
                    } catch (NoSuchFieldException e) {
                        // ignore
                    }
                    c = c.getSuperclass();
                } while (c != null && listField == null);

                if (listField == null) {
                    throw new SerializationException("Cannot find field "
                            + fieldName);
                }

                listFieldClazz = listField.getType();
            }

            // The type is an array or list. If the inner type matches a
            // primitive or String, it's guaranteed be homogeneous in type,
            // we can just read the data quickly as a block one right after
            // another without a header.
            switch (innerList.elemType) {
            case SelfDescribingBinaryProtocol.FLOAT:
                float[] fa = protocol.readF32List(innerList.size);
                protocol.readListEnd();
                return fa;
            case TType.DOUBLE:
                double[] doubleArray = protocol.readD64List(innerList.size);
                protocol.readListEnd();
                return doubleArray;
            case TType.I32:
                int[] intArray = protocol.readI32List(innerList.size);
                protocol.readListEnd();
                return intArray;
            case TType.BYTE:
                byte[] byteArray = protocol.readI8List(innerList.size);
                protocol.readListEnd();
                return byteArray;
            case TType.BOOL:
                boolean[] boolArray = new boolean[innerList.size];
                for (int i = 0; i < boolArray.length; i++) {
                    boolArray[i] = readBool();
                }
                protocol.readListEnd();
                return boolArray;
            case TType.I64:
                long[] longArray = protocol.readI64List(innerList.size);
                protocol.readListEnd();
                return longArray;
            case TType.I16:
                short[] shortArray = protocol.readI16List(innerList.size);
                protocol.readListEnd();
                return shortArray;
            case TType.STRING:
                if (listFieldClazz == null || listFieldClazz.isArray()) {
                    String[] stringArray = new String[innerList.size];
                    for (int i = 0; i < stringArray.length; i++) {
                        stringArray[i] = readString();
                    }
                    protocol.readListEnd();
                    return stringArray;
                } else {
                    // this is a List but due to the encoded element type
                    // we can safely assume it's all Strings
                    List<String> list = null;
                    if (listFieldClazz != null) {
                        if (!listFieldClazz.isInterface()
                                && List.class.isAssignableFrom(listFieldClazz)) {
                            list = (List<String>) listFieldClazz.newInstance();
                        }
                    }
                    if (list == null) {
                        list = new ArrayList<String>();
                    }
                    for (int i = 0; i < innerList.size; i++) {
                        list.add(readString());
                    }
                    return list;
                }
            default:
                if (listFieldClazz != null && listFieldClazz.isArray()) {
                    // Slower catch-all implementation
                    Class<?> arrayComponent = listFieldClazz.getComponentType();
                    Byte serializedType = innerList.elemType;
                    Object array = Array.newInstance(arrayComponent,
                            innerList.size);

                    for (int i = 0; i < innerList.size; i++) {
                        Array.set(
                                array,
                                i,
                                deserializeType(serializedType, null, null, "",
                                        EnclosureType.COLLECTION));
                    }
                    protocol.readListEnd();
                    return array;
                } else {
                    // This type is an actual List. Since Java 1.5+ did not
                    // expose generics information at runtime, we must assume an
                    // erased type, and do reflection on every component of the
                    // list.
                    List list = null;
                    if (listFieldClazz != null) {
                        if (!listFieldClazz.isInterface()
                                && List.class.isAssignableFrom(listFieldClazz)) {
                            list = (List) listFieldClazz.newInstance();
                        }
                    }

                    if (list == null) {
                        list = new ArrayList();
                    }
                    for (int i = 0; i < innerList.size; i++) {
                        list.add(this.serializationManager.deserialize(this));
                    }
                    protocol.readListEnd();
                    return list;
                }
            }
        } catch (Exception e) {
            throw new SerializationException("Error deserializing list/array",
                    e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.ISerializationContext#writeMessageEnd()
     */
    @Override
    public void writeMessageEnd() throws SerializationException {
        this.protocol.writeMessageEnd();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.serialize.ISerializationContext#writeMessageStart(java
     * .lang.String)
     */
    @Override
    public void writeMessageStart(String messageName)
            throws SerializationException {
        try {
            this.protocol.writeMessageBegin(new TMessage(messageName,
                    TType.VOID, 0));
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.serialize.IDeserializationContext#readMessageEnd()
     */
    @Override
    public void readMessageEnd() throws SerializationException {
        this.protocol.readMessageEnd();

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.serialize.IDeserializationContext#readMessageStart()
     */
    @Override
    public String readMessageStart() throws SerializationException {
        try {
            TMessage msg = this.protocol.readMessageBegin();
            return msg.name;
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.IDeserializationContext#readFloatArray
     * ()
     */
    @Override
    public float[] readFloatArray() throws SerializationException {
        try {
            int sz = this.protocol.readI32();
            return this.protocol.readF32List(sz);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.IDeserializationContext#readDoubleArray
     * ()
     */
    @Override
    public double[] readDoubleArray() throws SerializationException {
        try {
            int sz = this.protocol.readI32();
            return this.protocol.readD64List(sz);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationContext#writeFloatArray
     * (float[])
     */
    @Override
    public void writeFloatArray(float[] floats) throws SerializationException {
        try {
            this.protocol.writeI32(floats.length);
            this.protocol.writeF32List(floats);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationContext#writeDoubleArray
     * (double[])
     */
    @Override
    public void writeDoubleArray(double[] dubs) throws SerializationException {
        try {
            this.protocol.writeI32(dubs.length);
            this.protocol.writeD64List(dubs);
        } catch (TException e) {
            throw new SerializationException(e);
        }
    }
}
