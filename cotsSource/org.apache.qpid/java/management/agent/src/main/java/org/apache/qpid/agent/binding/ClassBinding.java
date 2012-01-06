/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.agent.binding;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.agent.annotations.QMFEvent;
import org.apache.qpid.agent.annotations.QMFObject;
import org.apache.qpid.agent.annotations.QMFProperty;
import org.apache.qpid.agent.annotations.QMFSeeAlso;
import org.apache.qpid.agent.annotations.QMFType;
import org.apache.qpid.agent.annotations.QMFHide;
import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;

/**
 * Binding information from a custom java class to a QMF schema
 */
public class ClassBinding implements TypeBinding
{
    private static Log log = LogFactory.getLog(ClassBinding.class);

    private static enum MethodType
    {
        READ_ONLY, READ_WRITE, METHOD, IGNORE
    }

    protected boolean exposeBehaviour = true;
    protected String pkg;
    protected BindingContext bctx;
    protected String name;
    protected ArrayList<PropertyBinding> properties = new ArrayList<PropertyBinding>();
    protected ArrayList<MethodBinding> methods = new ArrayList<MethodBinding>();
    protected Map<String, MethodBinding> methodsByName = new HashMap<String, MethodBinding>();
    protected Class javaClass;
    protected short kind = 1;
    protected byte hash[] = null;
    protected ClassBinding superType = null;

    public ClassBinding(String pkg, String name, Class cls,
            boolean exposeBehaviour, BindingContext bctx)
    {
        this.pkg = pkg;
        this.name = name;
        this.bctx = bctx;
        this.javaClass = cls;
        this.exposeBehaviour = exposeBehaviour;
    }

    protected MethodType classify(Class<?> cls, Method m)
    {
        String name = m.getName();
        MethodType returnValue = MethodType.METHOD;
        String propPrefixes[] =
        { "get", "is" };
        for (String prefix : propPrefixes)
        {
            if (name.startsWith(prefix) && m.getParameterTypes().length == 0)
            {
                try
                {
                    Class<?> type = m.getReturnType();
                    Method setter = cls.getMethod("set"
                            + name.substring(prefix.length()), type);
                    returnValue = MethodType.READ_WRITE;
                } catch (NoSuchMethodException e)
                {
                    returnValue = MethodType.READ_ONLY;
                }
                break;
            }
        }
        return returnValue;
    }

    protected String property(Method m)
    {
        String name = m.getName();
        String propPrefixes[] =
        { "get", "is" };
        for (String prefix : propPrefixes)
        {
            if (name.startsWith(prefix) && m.getParameterTypes().length == 0)
            {
                String sfx = name.substring(prefix.length());
                return Character.toLowerCase(sfx.charAt(0)) + sfx.substring(1);
            }
        }
        // If we got here, it is n invalid property
        throw new IllegalArgumentException("" + m);
    }

    protected ArrayList<Method> getMethods(Class cls)
    {
        ArrayList returnValue = new ArrayList();
        ArrayList nameList = new ArrayList();
        if ((cls != null) && (!cls.equals(Object.class)))
        {
            for (Method m : cls.getDeclaredMethods())
            {
                if (m.getAnnotation(QMFHide.class) == null)
                // && (!Modifier.isAbstract(m.getModifiers())))
                {
                    returnValue.add(m);
                    nameList.add(m.getName());
                }
            }
            // Look at the superclass, if it is also a
            // QMF object then stop.
            Class superType = cls.getSuperclass();
            if (!this.hasQMFSupertype(cls))
            {
                for (Method m : this.getMethods(cls.getSuperclass()))
                {
                    if (!nameList.contains(m.getName()))
                    {
                        returnValue.add(m);
                        nameList.add(m.getName());
                    }
                }
            }
        }
        return returnValue;
    }

    protected boolean hasQMFSupertype(Class cls)
    {
        boolean returnValue = false;
        Class superType = cls.getSuperclass();
        if (superType != null)
        {
            if ((superType.getAnnotation(QMFObject.class) != null)
                    || (superType.getAnnotation(QMFType.class) != null)
                    || (superType.getAnnotation(QMFSeeAlso.class) != null)
                    || (superType.getAnnotation(QMFEvent.class) != null))
            {
                returnValue = true;
            }
        }
        return returnValue;
    }

    protected boolean isOptional(Method m, TypeBinding type)
    {
        boolean returnValue = false;
        // Look for the annotaiton first
        QMFProperty ann = m.getAnnotation(QMFProperty.class);
        if (ann != null)
        {
            returnValue = ann.optional();
        } else
        {
            returnValue = type.optionalDefault();
        }
        return returnValue;
    }

    public ClassBinding parse()
    {
        log.debug(String.format(
                "Parsing class binding '%s' for package '%s' from class %s",
                name, pkg, javaClass.getName()));
        for (Method m : this.getMethods(javaClass))
        {
            String mname = m.getName();
            Class<?> type = m.getReturnType();
            switch (classify(javaClass, m))
            {
            case READ_ONLY:
                TypeBinding tb = bctx.getTypeBinding(type);
                boolean optional = isOptional(m, tb);
                properties.add(new PropertyBinding(property(m), tb,
                        PropertyBinding.READ_ONLY, optional));
                break;
            case READ_WRITE:
                TypeBinding tbnd = bctx.getTypeBinding(type);
                boolean opt = isOptional(m, tbnd);
                properties.add(new PropertyBinding(property(m), tbnd,
                        PropertyBinding.READ_WRITE, opt));
                break;
            case METHOD:
                // Only expose methods if told to
                if (exposeBehaviour)
                {
                    List<ParameterBinding> params = new ArrayList<ParameterBinding>();
                    int arg = 0;
                    for (Class pcls : m.getParameterTypes())
                    {
                        params.add(new ParameterBinding("arg" + arg++, bctx
                                .getTypeBinding(pcls), true, false));
                    }
                    if (type != void.class)
                    {
                        params.add(new ParameterBinding("result", bctx
                                .getTypeBinding(type), false, true));
                    }
                    methods.add(new MethodBinding(mname, params));
                }
                break;
            case IGNORE:
                break;
            }
        }
        for (MethodBinding m : methods)
        {
            methodsByName.put(m.getName(), m);
        }
        QMFEvent eventAnnotation = (QMFEvent) javaClass
                .getAnnotation(QMFEvent.class);
        if (eventAnnotation != null)
        {
            kind = 2; // Event Type
        }
        // if (this.hasQMFSupertype(javaClass)) {
        if ((javaClass.getSuperclass() != Object.class)
                && (javaClass.getSuperclass() != null))
        {
            superType = bctx.register(javaClass.getSuperclass());
        }
        return this;
    }

    public String getPackage()
    {
        return pkg;
    }

    public String getName()
    {
        return name;
    }

    public List<PropertyBinding> getProperties()
    {
        return properties;
    }

    public List<PropertyBinding> getAllProperties()
    {
        if (this.superType == null)
        {
            return properties;
        } else
        {
            List<PropertyBinding> newList = new ArrayList<PropertyBinding>(
                    properties);
            for (PropertyBinding p : superType.getAllProperties())
            {
                if (!newList.contains(p))
                {
                    newList.add(p);
                }
            }
            return newList;
        }
    }

    public List<MethodBinding> getMethods()
    {
        return methods;
    }

    public MethodBinding getMethod(String name)
    {
        return methodsByName.get(name);
    }

    // Use this format
    // bytes value
    // 0-3 package name
    // 4-7 class name
    // 8-11 property signature hash
    // 12-15 method signature hash
    // FIXME: Hash codes seem to mess things up
    public byte[] getSchemaHash()
    {
        if (null == hash)
        {
            hash = new byte[16];
            StringBuilder blder = new StringBuilder();
            int packageHash = pkg.hashCode();
            int classHash = name.hashCode();
            int propertyHash = 0;
            int methodHash = 0;
            for (PropertyBinding p : properties)
            {
                blder.append(p.getName()).append(":").append(
                        p.getType().getCode()).append(":")
                        .append(p.getAccess()).append(":").append(
                                p.isOptional());
            }
            propertyHash = blder.toString().hashCode();
            blder = new StringBuilder();
            for (MethodBinding m : methods)
            {
                blder.append(m.getName());
                for (ParameterBinding p : m.getParameters())
                {
                    String direction = p.isIn() ? "in" : "out";
                    blder.append(":").append(p.getName()).append(":").append(
                            direction).append(":")
                            .append(p.getType().getCode());
                }
            }
            methodHash = blder.toString().hashCode();
            hash[0] = (byte) (packageHash >> 24);
            hash[1] = (byte) (packageHash >> 16);
            hash[2] = (byte) (packageHash >> 8);
            hash[3] = (byte) (packageHash);
            hash[4] = (byte) (classHash >> 24);
            hash[5] = (byte) (classHash >> 16);
            hash[6] = (byte) (classHash >> 8);
            hash[7] = (byte) (classHash);
            hash[8] = (byte) (propertyHash >> 24);
            hash[9] = (byte) (propertyHash >> 16);
            hash[10] = (byte) (propertyHash >> 8);
            hash[11] = (byte) (propertyHash);
            hash[12] = (byte) (methodHash >> 24);
            hash[13] = (byte) (methodHash >> 16);
            hash[14] = (byte) (methodHash >> 8);
            hash[15] = (byte) (methodHash);
        }
        return hash;
    }

    public void encode(Encoder enc)
    {
        log.debug(String.format("encoding %s %s with superclass %s", this
                .getRefClass(), this.getRefPackage(), superType));
        enc.writeUint8(kind); // kind
        enc.writeStr8(pkg);
        enc.writeStr8(name);
        enc.writeBin128(this.getSchemaHash()); // schema hash
        // Send true (1) if we have a super-type
        //if (superType == null)
        //{
        //    enc.writeUint8((short) 0);
        //} else
        //{
        //    enc.writeUint8((short) 1);
        //}
        enc.writeUint16(properties.size());
        // Events do not have the method size sent
        if (kind == 1)
        {
            enc.writeUint16(0);
            enc.writeUint16(methods.size());
        }
        // Add the super type information if we have it
        //if (superType != null)
        //{
        //    enc.writeStr8(superType.pkg);
        //    enc.writeStr8(superType.name);
        //    enc.writeBin128(superType.getSchemaHash()); // schema hash
        //}
        for (PropertyBinding p : properties)
        {
            log.trace("encoding property " + p.getName());
            p.encode(enc);
        }
        for (MethodBinding m : methods)
        {
            m.encode(enc);
        }
    }

    // Type Binding functions
    public short getCode()
    {
        return (short) 20;
    }

    public Class<?> getJavaClass()
    {
        return javaClass;
    }

    public Object decode(Decoder dec)
    {
        // FIXME This only works with POJOs
        short typeCode = dec.readUint8();
        log.trace("Type code: " + typeCode);
        if (typeCode == 20)
        {
            String packageName = dec.readStr8();
            String className = dec.readStr8();
            log
                    .debug(String
                            .format(
                                    "Decoding an object for package %s class %s with bindings for %s %s",
                                    packageName, className, this.pkg, this.name));
            byte schemaHash[] = dec.readBin128();
            // Check to see that this is me, and not a subclass
            if (packageName.equals(this.pkg) && className.equals(this.name))
            {
                return decodeWithNoHeaders(dec);
            } else
            {
                ClassBinding mcls = bctx
                        .getClassBinding(packageName, className);
                return mcls.decodeWithNoHeaders(dec);
            }
        } else
        {
            TypeBinding tb = QMFTypeBinding.getType(typeCode);
            return tb.decode(dec);
        }
    }

    protected Object decodeWithNoHeaders(Decoder dec)
    {
        Object instance = null;
        try
        {
            log.trace("Creating a new instance of " + this.javaClass.getName());
            instance = this.javaClass.newInstance();
        } catch (Exception e)
        {
            log.error("Could not instantiate object of class"
                    + this.javaClass.getName());
            throw new BindingException(e);
        }
        List<String> excludes = this.processPresenceMasks(dec);
        for (PropertyBinding p : getAllProperties())
        {
            if (!excludes.contains(p.getName()))
            {
                Object value = p.getType().decode(dec);
                BindingUtils.set(p, value, instance);
            }
        }
        return instance;
    }

    protected List<String> processPresenceMasks(Decoder dec)
    {
        List<String> excludes = new ArrayList<String>();
        short bit = 0;
        short mask = 0;
        for (PropertyBinding prop : properties)
        {
            if (prop.isOptional())
            {
                if (bit == 0)
                {
                    mask = dec.readUint8();
                    bit = 1;
                }
                if ((mask & bit) == 0)
                {
                    log.trace("Going in exclude " + prop.getName());
                    excludes.add(prop.getName());
                }
                bit *= 2;
                if (bit == 256)
                {
                    bit = 0;
                }
            }
        }
        return excludes;
    }

    public void encode(Encoder enc, Object value)
    {
        // if the object is null, assume this is the
        // correct class
        if (value == null || (value.getClass().equals(this.javaClass)))
        {
            String pkg = getPackage();
            String cls = getName();
            log.debug(String.format("Encoding class %s:%s", pkg, cls));
            enc.writeUint8(this.getCode());
            enc.writeStr8(pkg);
            enc.writeStr8(cls);
            enc.writeBin128(this.getSchemaHash());
            short bit = 0;
            short mask = 0;
            if (value != null)
            {
                // Encode the property presence masks first.
                // if this is not an event
                if (!isEvent())
                {
                    for (PropertyBinding p : getAllProperties())
                    {
                        if (p.isOptional())
                        {
                            Object pValue = BindingUtils.get(p, value);
                            if (bit == 0)
                                bit = 1;
                            if (pValue != null)
                            {
                                mask |= bit;
                            }
                            if (bit == 128)
                            {
                                enc.writeUint8(mask);
                                bit = 0;
                                mask = 0;
                            } else
                            {
                                bit = (short) (bit << 1);
                            }
                        }
                    }
                    if (bit != 0)
                    {
                        enc.writeUint8(mask);
                    }
                }
                // Now put the actual properties
                for (PropertyBinding p : getAllProperties())
                {
                    Object pValue = BindingUtils.get(p, value);
                    if (!p.isOptional() || !(pValue == null))
                    {
                        log.trace(String.format("Encoding property %s", p
                                .getName()));
                        p.getType().encode(enc, pValue);
                    }
                }
            }
            log.debug(String.format("Done with %s:%s", pkg, cls));
        } else
        {
            TypeBinding tb = bctx.getTypeBinding(value.getClass());
            if (tb == null)
            {
                throw new BindingException(String.format(
                        "No class named %s defined for this context ", value
                                .getClass()));
            } else
            {
                if (tb.isNative())
                {
                    enc.writeUint8(tb.getCode());
                }
                tb.encode(enc, value);
            }
        }
    }

    public boolean isNative()
    {
        return false;
    }

    public boolean optionalDefault()
    {
        return true;
    }

    public String getRefClass()
    {
        return this.name;
    }

    public String getRefPackage()
    {
        return this.pkg;
    }

    public short getKind()
    {
        return kind;
    }

    public boolean isEvent()
    {
        return kind == 2;
    }

    public void setKind(short kind)
    {
        this.kind = kind;
    }
}
