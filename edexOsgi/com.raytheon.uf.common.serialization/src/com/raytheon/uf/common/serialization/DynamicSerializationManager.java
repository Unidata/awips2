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
package com.raytheon.uf.common.serialization;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.datatype.Duration;
import javax.xml.namespace.QName;

import net.sf.cglib.beans.BeanMap;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.serialization.BuiltInTypeSupport.CalendarSerializer;
import com.raytheon.uf.common.serialization.BuiltInTypeSupport.DateSerializer;
import com.raytheon.uf.common.serialization.BuiltInTypeSupport.TimestampSerializer;
import com.raytheon.uf.common.serialization.adapters.BufferAdapter;
import com.raytheon.uf.common.serialization.adapters.ByteBufferAdapter;
import com.raytheon.uf.common.serialization.adapters.CoordAdapter;
import com.raytheon.uf.common.serialization.adapters.EnumSetAdapter;
import com.raytheon.uf.common.serialization.adapters.FloatBufferAdapter;
import com.raytheon.uf.common.serialization.adapters.GeometryTypeAdapter;
import com.raytheon.uf.common.serialization.adapters.GridGeometry2DAdapter;
import com.raytheon.uf.common.serialization.adapters.GridGeometryAdapter;
import com.raytheon.uf.common.serialization.adapters.JTSEnvelopeAdapter;
import com.raytheon.uf.common.serialization.adapters.PointAdapter;
import com.raytheon.uf.common.serialization.adapters.StackTraceElementAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.serialization.thrift.ThriftSerializationContext;
import com.raytheon.uf.common.serialization.thrift.ThriftSerializationContextBuilder;
import com.raytheon.uf.common.util.ByteArrayOutputStreamPool;
import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Dynamic Serialization Manager provides a serialization capability that runs
 * purely at runtime based on annotations.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 13, 2008 #1448       chammack    Initial creation
 * Mar 27, 2012 #428        dgilling    Add support for built-in
 *                                      classes used by data delivery's
 *                                      registry service.
 * Sep 28, 2012 #1195       djohnson    Add ability to specify adapter at field level.
 * Oct 08, 2012 #1251       dgilling    Ensure type registered with
 *                                      serialization adapter is encoded
 *                                      in serialization stream.
 * Nov 02, 2012 1302        djohnson    Remove field level adapters, they break python serialization.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class DynamicSerializationManager {
    private static Map<SerializationType, DynamicSerializationManager> instanceMap = new HashMap<SerializationType, DynamicSerializationManager>();

    private ISerializationContextBuilder builder;

    public static class SerializationMetadata {
        public List<String> serializedAttributes;

        public ISerializationTypeAdapter<?> serializationFactory;

        public Map<String, ISerializationTypeAdapter<?>> attributesWithFactories;

        public String adapterStructName;

    }

    private static Map<String, SerializationMetadata> serializedAttributes = new ConcurrentHashMap<String, SerializationMetadata>();

    private static final SerializationMetadata NO_METADATA = new SerializationMetadata();

    static {
        // TODO: Can the registration of adapters that require dependencies be
        // moved to a separate plugin somehow?
        registerAdapter(GregorianCalendar.class, new CalendarSerializer());
        registerAdapter(XMLGregorianCalendarImpl.class,
                new BuiltInTypeSupport.XMLGregorianCalendarSerializer());
        registerAdapter(Date.class, new DateSerializer());
        registerAdapter(Timestamp.class, new TimestampSerializer());
        registerAdapter(java.sql.Date.class,
                new BuiltInTypeSupport.SqlDateSerializer());
        registerAdapter(java.awt.Point.class, new PointAdapter());
        registerAdapter(Coordinate.class, new CoordAdapter());
        registerAdapter(BigDecimal.class,
                new BuiltInTypeSupport.BigDecimalSerializer());
        registerAdapter(BigInteger.class,
                new BuiltInTypeSupport.BigIntegerSerializer());
        registerAdapter(Geometry.class, new GeometryTypeAdapter());
        registerAdapter(Envelope.class, new JTSEnvelopeAdapter());
        registerAdapter(GridGeometry2D.class, new GridGeometry2DAdapter());
        registerAdapter(GeneralGridGeometry.class, new GridGeometryAdapter());
        registerAdapter(EnumSet.class, new EnumSetAdapter());
        registerAdapter(StackTraceElement.class, new StackTraceElementAdapter());
        registerAdapter(Duration.class,
                new BuiltInTypeSupport.DurationSerializer());
        registerAdapter(QName.class, new BuiltInTypeSupport.QNameSerializer());
        registerAdapter(Throwable.class,
                new BuiltInTypeSupport.ThrowableSerializer());
        // These two are OBE by BufferAdapter and should be deleted sometime
        registerAdapter(ByteBuffer.class, new ByteBufferAdapter());
        registerAdapter(FloatBuffer.class, new FloatBufferAdapter());
        registerAdapter(Buffer.class, new BufferAdapter());
    }

    public enum EnclosureType {
        FIELD, COLLECTION
    };

    public static enum SerializationType {
        Thrift
    };

    /**
     * Serialize an object to a byte array
     * 
     * @param obj
     *            the object
     * @return a byte array with a serialized version of the object
     * @throws SerializationException
     */
    public byte[] serialize(Object obj) throws SerializationException {

        ByteArrayOutputStream baos = ByteArrayOutputStreamPool.getInstance()
                .getStream();

        try {
            ISerializationContext ctx = this.builder.buildSerializationContext(
                    baos, this);
            ctx.writeMessageStart("dynamicSerialize");
            serialize(ctx, obj);
            ctx.writeMessageEnd();
            return baos.toByteArray();
        } finally {
            if (baos != null) {
                try {
                    // return stream to pool
                    baos.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Serialize an object to a byte array
     * 
     * @param obj
     *            the object
     * @return a byte array with a serialized version of the object
     * @throws SerializationException
     */
    public void serialize(Object obj, OutputStream os)
            throws SerializationException {

        ISerializationContext ctx = this.builder.buildSerializationContext(os,
                this);
        ctx.writeMessageStart("dynamicSerialize");
        serialize(ctx, obj);
        ctx.writeMessageEnd();

    }

    /**
     * Serialize an object using a context
     * 
     * This method is not intended to be used by end users.
     * 
     * @param ctx
     *            the serialization context
     * @param obj
     *            the object to serialize
     * @throws SerializationException
     */
    public void serialize(ISerializationContext ctx, Object obj)
            throws SerializationException {
        BeanMap beanMap = null;

        if (obj != null && !obj.getClass().isArray()) {
            beanMap = SerializationCache.getBeanMap(obj);
        }
        try {
            SerializationMetadata metadata = null;
            if (obj != null) {
                metadata = getSerializationMetadata(obj.getClass().getName());
            }

            ((ThriftSerializationContext) ctx).serializeMessage(obj, beanMap,
                    metadata);
        } finally {
            if (beanMap != null) {
                SerializationCache.returnBeanMap(beanMap, obj);
            }
        }
    }

    /**
     * Deserialize an object from a stream
     * 
     * @param istream
     * @return
     * @throws SerializationException
     */
    public Object deserialize(InputStream istream)
            throws SerializationException {
        IDeserializationContext ctx = this.builder.buildDeserializationContext(
                istream, this);
        ctx.readMessageStart();
        Object obj = deserialize(ctx);
        ctx.readMessageEnd();
        return obj;

    }

    /**
     * Deserialize from a context
     * 
     * Not intended to be used by end users
     * 
     * @param ctx
     * @return
     * @throws SerializationException
     */
    public Object deserialize(IDeserializationContext ctx)
            throws SerializationException {
        return ((ThriftSerializationContext) ctx).deserializeMessage();
    }

    public static <T> void registerAdapter(Class<? extends T> clazz,
            ISerializationTypeAdapter<T> adapter) {
        SerializationMetadata md = new SerializationMetadata();
        md.serializationFactory = adapter;
        md.adapterStructName = clazz.getName();
        if (serializedAttributes.containsKey(md.adapterStructName)) {
            throw new RuntimeException(
                    "Could not create serialization metadata for class: "
                            + clazz + ", metadata already exists");
        }
        serializedAttributes.put(md.adapterStructName, md);
    }

    /**
     * Inspect a class and return the metadata for the object
     * 
     * If the class has not been annotated, this will return null
     * 
     * The metadata is cached for performance
     * 
     * @param c
     *            the class
     * @return the metadata
     */
    public static SerializationMetadata inspect(Class<?> c) {

        // Check for base types

        SerializationMetadata attribs = serializedAttributes.get(c.getName());
        if (attribs != null) {
            return attribs;
        }

        attribs = new SerializationMetadata();
        attribs.serializedAttributes = new ArrayList<String>();
        attribs.attributesWithFactories = new HashMap<String, ISerializationTypeAdapter<?>>();

        DynamicSerializeTypeAdapter serializeAdapterTag = c
                .getAnnotation(DynamicSerializeTypeAdapter.class);

        // Check to see if there is an adapter
        if (serializeAdapterTag != null) {
            Class<?> factoryTag = (serializeAdapterTag).factory();
            try {
                attribs.serializationFactory = (ISerializationTypeAdapter<?>) factoryTag
                        .newInstance();
                attribs.adapterStructName = c.getName();
            } catch (Exception e) {
                throw new RuntimeException("Factory could not be constructed: "
                        + factoryTag, e);
            }
        }

        // check to see if superclass has an adapter
        if (attribs.serializationFactory == null) {
            Class<?> superClazz = c.getSuperclass();
            while (superClazz != null && attribs.serializationFactory == null) {
                SerializationMetadata superMd = inspect(superClazz);
                if (superMd != null && superMd.serializationFactory != null) {
                    attribs.serializationFactory = superMd.serializationFactory;
                    attribs.adapterStructName = superMd.adapterStructName;
                }
                superClazz = superClazz.getSuperclass();
            }
        }

        // Make sure the object is annotated or has an adapter. If not, return
        // null
        DynamicSerialize serializeTag = c.getAnnotation(DynamicSerialize.class);
        if (serializeTag == null && attribs.serializationFactory == null) {
            return null;
        }

        if (attribs.serializationFactory == null) {
            // Go through the class and find the fields with annotations
            Class<?> clazz = c;
            Set<String> getters = new HashSet<String>();
            Set<String> setters = new HashSet<String>();
            while (clazz != null && clazz != Object.class) {

                // Make sure a getter and setter has been defined, and throw an
                // exception if they haven't been

                getters.clear();
                setters.clear();
                Method[] methods = c.getMethods();
                for (Method m : methods) {
                    String name = m.getName();
                    if (name.startsWith("get")) {
                        name = name.substring(3);
                        getters.add(name.toLowerCase());
                    } else if (name.startsWith("is")) {
                        name = name.substring(2);
                        getters.add(name.toLowerCase());
                    } else if (name.startsWith("set")) {
                        name = name.substring(3);
                        setters.add(name.toLowerCase());
                    }
                }

                java.lang.reflect.Field[] fields = clazz.getDeclaredFields();
                for (java.lang.reflect.Field field : fields) {

                    int modifier = field.getModifiers();
                    if (Modifier.isFinal(modifier)) {
                        continue;
                    }

                    DynamicSerializeElement annotation = field
                            .getAnnotation(DynamicSerializeElement.class);
                    if (annotation != null) {
                        String fieldName = field.getName();

                        attribs.serializedAttributes.add(field.getName());
                        if (serializeAdapterTag == null) {
                            serializeAdapterTag = field.getType()
                                    .getAnnotation(
                                            DynamicSerializeTypeAdapter.class);
                        }
                        if (serializeAdapterTag != null) {
                            try {
                                attribs.attributesWithFactories.put(fieldName,
                                        serializeAdapterTag.factory()
                                                .newInstance());
                            } catch (Exception e) {
                                throw new RuntimeException(
                                        "Factory could not be instantiated", e);
                            }
                        }
                        // Throw a validation exception if necessary
                        boolean foundGetter = false;
                        boolean foundSetter = false;
                        String lower = fieldName.toLowerCase();

                        if (getters.contains(lower)) {
                            foundGetter = true;
                        }

                        if (setters.contains(lower)) {
                            foundSetter = true;
                        }

                        if (!foundGetter || !foundSetter) {
                            String missing = "";
                            if (!foundGetter && !foundSetter) {
                                missing = "Getter and Setter";
                            } else if (!foundGetter) {
                                missing = "Getter";
                            } else if (!foundSetter) {
                                missing = "Setter";
                            }

                            throw new RuntimeException("Required " + missing
                                    + " on " + clazz.getName() + ":"
                                    + field.getName() + " is missing");
                        }

                    }
                }
                clazz = clazz.getSuperclass();
            }
        }

        // Sort to guarantee universal ordering
        Collections.sort(attribs.serializedAttributes);
        serializedAttributes.put(c.getName(), attribs);

        // inspect inner classes
        Class<?>[] innerClzs = c.getClasses();
        for (Class<?> innerClz : innerClzs) {
            inspect(innerClz);
        }

        return attribs;
    }

    public static synchronized DynamicSerializationManager getManager(
            SerializationType type) {
        DynamicSerializationManager mgr = instanceMap.get(type);
        if (mgr == null) {
            mgr = new DynamicSerializationManager(type);
            instanceMap.put(type, mgr);
        }

        return mgr;
    }

    private DynamicSerializationManager(SerializationType type) {
        if (type == SerializationType.Thrift) {
            builder = new ThriftSerializationContextBuilder();
        }
    }

    /**
     * Get the serialization metadata. Build it if not found
     * 
     * @param name
     * @return
     */
    public SerializationMetadata getSerializationMetadata(String name) {
        // we can't synchronize on this because it's possible the
        // Class.forName() will trigger code that comes back into here and
        // then deadlocks
        SerializationMetadata sm = serializedAttributes.get(name);
        if (sm == null) {
            try {
                sm = inspect(Class.forName(name, true, getClass()
                        .getClassLoader()));
                if (sm == null) {
                    serializedAttributes.put(name, NO_METADATA);
                }
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            }
        }

        if (sm == NO_METADATA) {
            return null;
        }
        return sm;

    }
}
