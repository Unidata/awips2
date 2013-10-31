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
package com.raytheon.uf.common.serialization.jaxb;

import java.io.File;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.reflect.ISubClassLocator;

/**
 * Locate all classes that can be used in a JAXB contexts. This class uses an
 * {@link ISubClassLocator} to dynamically find all possible subtypes of every
 * class used in a JAXB context. For most xml types this would be overkill but
 * for some advanced types that allow dynamic class types with the xsi:type
 * field this can be useful for building a complete context.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 22, 2013  2491     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class JAXBClassLocator {

    private ISubClassLocator locator;

    private Set<Class<?>> processed = new HashSet<Class<?>>(512);

    private List<Class<?>> included = new ArrayList<Class<?>>(512);

    private JAXBClassLocator(ISubClassLocator locator, Class<?>... rootClasses) {
        this.locator = locator;
        processed.addAll(getBuiltinTypes());
        for (Class<?> rootClass : rootClasses) {
            processClass(rootClass);
        }
    }

    private void processClass(Class<?> clazz) {
        if (!processed.add(clazz)) {
            return;
        }
        if (clazz.isPrimitive() || clazz.isEnum() || clazz.isInterface()) {
            return;
        }
        if (clazz.isArray()) {
            processClass(clazz.getComponentType());
            return;
        }
        if (clazz.isAnnotationPresent(XmlTransient.class)) {
            return;
        }
        if (clazz.getName().contains("$")
                && !clazz.isAnnotationPresent(XmlType.class)) {
            return;
        }
        included.add(clazz);
        Class<?> zuper = clazz.getSuperclass();
        if (zuper == Object.class) {
            /* Found a "base" class, search for subclasses */
            if (!Modifier.isFinal(clazz.getModifiers())) {
                for (Class<?> sub : locator.locateSubClasses(clazz)) {
                    processClass(sub);
                }
            }
        } else {
            processClass(zuper);
        }

        for (Field field : clazz.getDeclaredFields()) {
            processField(field);
        }
        for (Method method : clazz.getDeclaredMethods()) {
            processMethod(method);
        }
    }

    private void processField(Field field) {
        if (isXmlElement(field)) {
            Class<?> type = field.getType();
            if (Collection.class.isAssignableFrom(type)) {
                handleCollectionType(field.getGenericType());
            } else {
                processClass(field.getType());
            }
        }
    }

    private void processMethod(Method method) {
        if (isXmlElement(method)) {
            Class<?> returnType = method.getReturnType();
            if (returnType != void.class) {
                /* Getter */
                processClass(returnType);
            } else if (Collection.class.isAssignableFrom(returnType)) {
                handleCollectionType(method.getGenericReturnType());
            } else {
                Class<?>[] tp = method.getParameterTypes();
                if (tp.length == 1) {
                    /* Setter */
                    if (Collection.class.isAssignableFrom(tp[0])) {
                        handleCollectionType(method.getGenericParameterTypes()[0]);
                    } else {
                        processClass(tp[0]);
                    }
                }
            }
        }
    }

    private void handleCollectionType(Type type) {
        if (type instanceof ParameterizedType) {
            ParameterizedType ptype = (ParameterizedType) type;
            Type[] args = ptype.getActualTypeArguments();
            if (args.length == 1 && args[0] instanceof Class) {
                processClass((Class<?>) args[0]);
            }
        } else if (type instanceof Class) {
            handleCollectionType(((Class<?>) type).getGenericSuperclass());
        }
    }

    private Collection<Class<?>> getResults() {
        return included;
    }

    private static boolean isXmlElement(AnnotatedElement element) {
        return element.isAnnotationPresent(XmlElement.class)
                && !element.isAnnotationPresent(XmlJavaTypeAdapter.class);
    }

    /**
     * A base set of classes for which we don't need to find subclasses or
     * reflect on the fields.
     * 
     * @return
     */
    private static Collection<Class<?>> getBuiltinTypes() {
        return Arrays.<Class<?>> asList(Boolean.class, Byte.class, Short.class,
                Integer.class, Long.class, Float.class, Double.class,
                String.class, Object.class, Class.class, File.class);
    }

    /**
     * Dynamically find all classes that can possibly appear in an xml document
     * rooted in baseClass.
     * 
     * @param locator
     *            used for finding subclasses
     * @param rootClass
     *            the root cml object
     * @return all classes that are can be in the xml.
     */
    public static Collection<Class<?>> getJAXBClasses(ISubClassLocator locator,
            Class<?>... baseClasses) {
        return new JAXBClassLocator(locator, baseClasses).getResults();
    }
}
