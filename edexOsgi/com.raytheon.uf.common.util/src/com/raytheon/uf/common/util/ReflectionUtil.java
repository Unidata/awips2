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
package com.raytheon.uf.common.util;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.beanutils.PropertyUtils;

/**
 * Reflection utilities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2012 634        djohnson     Initial creation
 * Jul 10, 2012 455        djohnson     Move in methods from RegistryUtil, 
 *                                      fix setter method to use parameter types.
 * Sep 28, 2012 1195       djohnson     Add {@link #forName(String)}.
 * Jan 23, 2014 2584       dhladky      Versions for JAXB objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public final class ReflectionUtil {

    private static ConcurrentMap<Class<?>, List<Field>> classFields = new ConcurrentHashMap<Class<?>, List<Field>>();

    private ReflectionUtil() {
        // Utility class
    }

    public static <T> T newInstanceOfAssignableType(Class<T> assignableClass,
            String className) {
        try {
            @SuppressWarnings("unchecked")
            Class<? extends T> clazz = (Class<? extends T>) Class
                    .forName(className);
            return newInstanceOfAssignableType(assignableClass, clazz);
        } catch (ClassCastException cce) {
            throw new ReflectionException(String.format(
                    "%s is not assignable to a field of type %s", className,
                    assignableClass.getName()), cce);
        } catch (ClassNotFoundException e) {
            throw new ReflectionException(e);
        }
    }

    public static <T> T newInstanceOfAssignableType(Class<T> assignableClass,
            Class<? extends T> clazz) {
        try {
            return assignableClass.cast(newInstance(clazz));
        } catch (ClassCastException cce) {
            throw new ReflectionException(String.format(
                    "%s is not assignable to a field of type %s",
                    clazz.getName(), assignableClass.getName()), cce);
        }
    }

    public static Method getGetterMethod(Class<?> clazz, String name)
            throws ReflectionException {
        // Assume camel cased names - capitalize first letter...
        String method = Character.toUpperCase(name.charAt(0))
                + name.substring(1);
        try {
            Method m;
            try {
                // Try common 'get' first...
                m = clazz.getMethod("get" + method);
            } catch (NoSuchMethodException e) {
                // Try 'is' as a prefix
                m = clazz.getMethod("is" + method);
            }

            return m;
        } catch (Exception e) {
            throw new ReflectionException(e);
        }
    }

    public static Object getter(Object object, String name)
            throws ReflectionException {
        try {
            return getGetterMethod(object.getClass(), name).invoke(object,
                    (Object[]) null);
        } catch (Exception e) {
            throw new ReflectionException(e);
        }
    }

    public static <T> T getter(Class<T> resultType, Object obj, String name) {
        Object result = null;
        try {
            result = getter(obj, name);
            return resultType.cast(result);
        } catch (ClassCastException cce) {
            throw new ReflectionException(String.format(
                    "%s is not assignable to a field of type %s", result
                            .getClass().getName(), resultType.getName()), cce);
        }
    }

    public static void setter(Object object, String name, Object... parameters)
            throws ReflectionException {
        try {
            String method = "set" + Character.toUpperCase(name.charAt(0))
                    + name.substring(1);

            Class<?>[] parameterTypes = new Class<?>[parameters.length];
            for (int i = 0; i < parameters.length; i++) {
                parameterTypes[i] = parameters[i].getClass();
            }

            Method m = object.getClass().getMethod(method, parameterTypes);
            m.invoke(object, parameters);
        } catch (Exception e) {
            throw new ReflectionException(e);
        }
    }

    /**
     * Constructs a new instance of the specified class.
     * 
     * @param clazz
     *            the class to construct
     * @return the constructed instance
     * @throws ReflectionException
     *             on error constructing the instance
     */
    public static Object newInstance(Class<?> clazz) throws ReflectionException {
        try {
            return clazz.newInstance();
        } catch (Exception e) {
            throw new ReflectionException(String.format(
                    "Unable to construct an object of type %s!",
                    clazz.getName()), e);
        }
    }

    /**
     * Gets all fields for a given class. Fields from the superclass are also
     * included
     * 
     * @param fields
     *            The list used to hold the fields
     * @param type
     *            The class type to look at
     * @return The complete list of all fields including inherited fields
     */
    public static List<Field> getAllFields(Class<?> type) {
        List<Field> storedFields = classFields.get(type);
        if (storedFields != null) {
            return storedFields;
        }
        List<Field> fields = new ArrayList<Field>();
        for (Field field : type.getDeclaredFields()) {
            fields.add(field);
        }
        if (type.getSuperclass() != null) {
            fields.addAll(getAllFields(type.getSuperclass()));
        }
        classFields.putIfAbsent(type, fields);
        return fields;
    }

    /**
     * Checks the fields of a class to see if the desired annotation is present
     * on any fields. If the annotation is found, the value of the field is
     * returned
     * 
     * @param obj
     *            The object to check
     * @param annotation
     *            The annotation class to look for
     * @return The value of the annotated field
     * @throws ReflectionException
     *             If reflection errors occur
     */
    public static String getAnnotatedField(Object obj,
            Class<? extends Annotation> annotation) throws ReflectionException {
        List<Field> fields = getAllFields(obj.getClass());

        for (Field field : fields) {
            Annotation ann = field.getAnnotation(annotation);
            if (ann != null) {
                try {
                    return PropertyUtils.getProperty(obj, field.getName())
                            .toString();
                } catch (Exception e) {
                    throw new ReflectionException(
                            "Error getting annotated field value", e);
                }
            }
        }
        return null;
    }
 
    /**
     * Get this annotation from this class if it exists
     * 
     * @param clazz
     *            The class to check
     * @param annotation
     *            The annotation class to look for
     * @return Annotation
     */
    public static <T extends Annotation>T getAnnotationFromClass(Class<?> clazz,
            Class<T> annotation) throws ReflectionException {

        T ann = clazz.getAnnotation(annotation);
        if (ann != null) {
            return ann;
        }
        
        return null;
    }

    /**
     * Create a class instance from its name.
     * 
     * @param className
     *            the class name
     * @return the class
     */
    public static Class<?> forName(String className) {
        try {
            return Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new ReflectionException(e);
        }
    }
}
