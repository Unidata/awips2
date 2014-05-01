/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.jaxb;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.annotation.XmlTransient;

import com.sun.xml.bind.v2.model.annotation.AbstractInlineAnnotationReaderImpl;
import com.sun.xml.bind.v2.model.annotation.Locatable;
import com.sun.xml.bind.v2.model.annotation.RuntimeAnnotationReader;
import com.sun.xml.bind.v2.model.annotation.RuntimeInlineAnnotationReader;

/**
 * JAXB utility to allow for Transient annotation to be added to fields and
 * methods of classes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

@SuppressWarnings("rawtypes")
public class TransientAnnotationReader extends
        AbstractInlineAnnotationReaderImpl<Type, Class, Field, Method>
        implements
        RuntimeAnnotationReader {

    private static class XmlTransientProxyHandler implements InvocationHandler {
        public Object invoke(Object proxy, Method method, Object[] args)
                throws Throwable {
            if (args == null || args.length == 0) {
                if (method.getName().equals("annotationType")) {
                    return XmlTransient.class;
                }
                if (method.getName().equals("toString")) {
                    return "XmlTransient";
                }
            }
            String msg = "@XmlTransient doesn't support method: "
                    + method.getName();
            throw new UnsupportedOperationException(msg);
        }

        private static XmlTransient create() {
            ClassLoader loader = XmlTransientProxyHandler.class
                    .getClassLoader();
            Class<?>[] interfaces = new Class[] { XmlTransient.class };
            XmlTransientProxyHandler handler = new XmlTransientProxyHandler();
            return (XmlTransient) Proxy.newProxyInstance(loader, interfaces,
                    handler);
        }
    }

    private static final Annotation XML_TRANSIENT_ANNOTATION = XmlTransientProxyHandler
            .create();

    private static final Annotation[] XML_TRANSIENT_ANNOTATION_ONLY = { XML_TRANSIENT_ANNOTATION };

    private final RuntimeAnnotationReader delegate = new RuntimeInlineAnnotationReader();

    private final Set<Class<?>> transientClasses = Collections
            .newSetFromMap(new ConcurrentHashMap<Class<?>, Boolean>());

    private final Set<Field> transientFields = Collections
            .newSetFromMap(new ConcurrentHashMap<Field, Boolean>());

    private final Set<Method> transientMethods = Collections
            .newSetFromMap(new ConcurrentHashMap<Method, Boolean>());

    /**
     * 
     */
    public TransientAnnotationReader() {
    }

    public void addTransientClass(Class<?> c) {
        transientClasses.add(c);
    }

    public void addTransientField(Field f) {
        transientFields.add(f);
    }

    public void addTransientMethod(Method m) {
        transientMethods.add(m);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getAllFieldAnnotations
     * (java.lang.Object, com.sun.xml.bind.v2.model.annotation.Locatable)
     */
    @Override
    public Annotation[] getAllFieldAnnotations(Field f, Locatable srcPos) {
        if (transientFields.contains(f)) {
            return XML_TRANSIENT_ANNOTATION_ONLY;
        }
        return delegate.getAllFieldAnnotations(f, srcPos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getAllMethodAnnotations
     * (java.lang.Object, com.sun.xml.bind.v2.model.annotation.Locatable)
     */
    @Override
    public Annotation[] getAllMethodAnnotations(Method m, Locatable srcPos) {
        if (transientMethods.contains(m)) {
            return XML_TRANSIENT_ANNOTATION_ONLY;
        }
        return delegate.getAllMethodAnnotations(m, srcPos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getClassAnnotation
     * (java.lang.Class, java.lang.Object,
     * com.sun.xml.bind.v2.model.annotation.Locatable)
     */
    @SuppressWarnings("unchecked")
    @Override
    public <A extends Annotation> A getClassAnnotation(Class<A> type, Class c,
            Locatable srcPos) {
        if (transientClasses.contains(c)) {
            return (A) XML_TRANSIENT_ANNOTATION;
        }
        return delegate.getClassAnnotation(type, c, srcPos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getClassArrayValue
     * (java.lang.annotation.Annotation, java.lang.String)
     */
    @Override
    public Type[] getClassArrayValue(Annotation a, String name) {
        return delegate.getClassArrayValue(a, name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getClassValue(java
     * .lang.annotation.Annotation, java.lang.String)
     */
    @Override
    public Type getClassValue(Annotation a, String name) {
        return delegate.getClassValue(a, name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getFieldAnnotation
     * (java.lang.Class, java.lang.Object,
     * com.sun.xml.bind.v2.model.annotation.Locatable)
     */
    @SuppressWarnings("unchecked")
    @Override
    public <A extends Annotation> A getFieldAnnotation(Class<A> type, Field f,
            Locatable srcPos) {
        if (XmlTransient.class.isAssignableFrom(type)
                && transientFields.contains(f)) {
            return (A) XML_TRANSIENT_ANNOTATION;
        }
        return delegate.getFieldAnnotation(type, f, srcPos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getMethodAnnotation
     * (java.lang.Class, java.lang.Object,
     * com.sun.xml.bind.v2.model.annotation.Locatable)
     */
    @SuppressWarnings("unchecked")
    @Override
    public <A extends Annotation> A getMethodAnnotation(Class<A> type,
            Method m, Locatable srcPos) {
        if (XmlTransient.class.isAssignableFrom(type)
                && transientMethods.contains(m)) {
            return (A) XML_TRANSIENT_ANNOTATION;
        }
        return delegate.getMethodAnnotation(type, m, srcPos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.xml.bind.v2.model.annotation.AnnotationReader#
     * getMethodParameterAnnotation(java.lang.Class, java.lang.Object, int,
     * com.sun.xml.bind.v2.model.annotation.Locatable)
     */
    @Override
    public <A extends Annotation> A getMethodParameterAnnotation(Class<A> type,
            Method m, int index, Locatable srcPos) {
        return delegate.getMethodParameterAnnotation(type, m, index, srcPos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#getPackageAnnotation
     * (java.lang.Class, java.lang.Object,
     * com.sun.xml.bind.v2.model.annotation.Locatable)
     */
    @Override
    public <A extends Annotation> A getPackageAnnotation(Class<A> type,
            Class c, Locatable srcPos) {
        return delegate.getPackageAnnotation(type, c, srcPos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#hasClassAnnotation
     * (java.lang.Object, java.lang.Class)
     */
    @Override
    public boolean hasClassAnnotation(Class c, Class<? extends Annotation> type) {
        if (transientClasses.contains(c)) {
            return true;
        }
        return delegate.hasClassAnnotation(c, type);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#hasFieldAnnotation
     * (java.lang.Class, java.lang.Object)
     */
    @Override
    public boolean hasFieldAnnotation(Class<? extends Annotation> type, Field f) {
        if (XmlTransient.class.isAssignableFrom(type)
                && transientFields.contains(f)) {
            return true;
        }
        return delegate.hasFieldAnnotation(type, f);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AnnotationReader#hasMethodAnnotation
     * (java.lang.Class, java.lang.Object)
     */
    @Override
    public boolean hasMethodAnnotation(Class<? extends Annotation> type,
            Method m) {
        if (XmlTransient.class.isAssignableFrom(type)
                && transientMethods.contains(m)) {
            return true;
        }
        return delegate.hasMethodAnnotation(type, m);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.sun.xml.bind.v2.model.annotation.AbstractInlineAnnotationReaderImpl
     * #fullName(java.lang.Object)
     */
    @Override
    protected String fullName(Method m) {
        return m.getDeclaringClass().getName() + '#' + m.getName();
    }

}
