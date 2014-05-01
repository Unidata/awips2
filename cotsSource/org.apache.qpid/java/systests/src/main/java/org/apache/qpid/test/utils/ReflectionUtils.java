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
package org.apache.qpid.test.utils;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Provides helper methods for operating on classes and methods using reflection. Reflection methods tend to return
 * a lot of checked exception so writing code to use them can be tedious and harder to read, especially when such errors
 * are not expected to occur. This class always works with {@link ReflectionUtilsException}, which is a runtime exception,
 * to wrap the checked exceptions raised by the standard Java reflection methods. Code using it does not normally
 * expect these errors to occur, usually does not have a recovery mechanism for them when they do, but is cleaner,
 * quicker to write and easier to read in the majority of cases.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Look up Classes by name.
 * <tr><td> Instantiate Classes by no-arg constructor.
 * </table>
 */
public class ReflectionUtils
{
    /**
    * Gets the Class object for a named class.
    *
    * @param className The class to get the Class object for.
    *
    * @return The Class object for the named class.
    */
    public static Class<?> forName(String className)
    {
        try
        {
            return Class.forName(className);
        }
        catch (ClassNotFoundException e)
        {
            throw new ReflectionUtilsException("ClassNotFoundException whilst finding class.", e);
        }
    }

    /**
     * Creates an instance of a Class, instantiated through its no-args constructor.
     *
     * @param cls The Class to instantiate.
     * @param <T> The Class type.
     *
     * @return An instance of the class.
     */
    public static <T> T newInstance(Class<? extends T> cls)
    {
        try
        {
            return cls.newInstance();
        }
        catch (InstantiationException e)
        {
            throw new ReflectionUtilsException("InstantiationException whilst instantiating class.", e);
        }
        catch (IllegalAccessException e)
        {
            throw new ReflectionUtilsException("IllegalAccessException whilst instantiating class.", e);
        }
    }

    /**
     * Calls a named method on an object with a specified set of parameters, any Java access modifier are overridden.
     *
     * @param o            The object to call.
     * @param method       The method name to call.
     * @param params       The parameters to pass.
     * @param paramClasses The argument types.
     *
     * @return The return value from the method call.
     */
    public static Object callMethodOverridingIllegalAccess(Object o, String method, Object[] params, Class[] paramClasses)
    {
        // Get the objects class.
        Class cls = o.getClass();

        // Get the classes of the parameters.
        /*Class[] paramClasses = new Class[params.length];

        for (int i = 0; i < params.length; i++)
        {
            paramClasses[i] = params[i].getClass();
        }*/

        try
        {
            // Try to find the matching method on the class.
            Method m = cls.getDeclaredMethod(method, paramClasses);

            // Make it accessible.
            m.setAccessible(true);

            // Invoke it with the parameters.
            return m.invoke(o, params);
        }
        catch (NoSuchMethodException e)
        {
            throw new ReflectionUtilsException("NoSuchMethodException.", e);
        }
        catch (IllegalAccessException e)
        {
            throw new ReflectionUtilsException("IllegalAccessException.", e);
        }
        catch (InvocationTargetException e)
        {
            throw new ReflectionUtilsException("InvocationTargetException", e);
        }
    }

    /**
     * Calls a named method on an object with a specified set of parameters.
     *
     * @param o      The object to call.
     * @param method The method name to call.
     * @param params The parameters to pass.
     *
     * @return The return value from the method call.
     */
    public static Object callMethod(Object o, String method, Object[] params)
    {
        // Get the objects class.
        Class cls = o.getClass();

        // Get the classes of the parameters.
        Class[] paramClasses = new Class[params.length];

        for (int i = 0; i < params.length; i++)
        {
            paramClasses[i] = params[i].getClass();
        }

        try
        {
            // Try to find the matching method on the class.
            Method m = cls.getMethod(method, paramClasses);

            // Invoke it with the parameters.
            return m.invoke(o, params);
        }
        catch (NoSuchMethodException e)
        {
            throw new ReflectionUtilsException("NoSuchMethodException.", e);
        }
        catch (IllegalAccessException e)
        {
            throw new ReflectionUtilsException("IllegalAccessException", e);
        }
        catch (InvocationTargetException e)
        {
            throw new ReflectionUtilsException("InvocationTargetException", e);
        }
    }

    /**
     * Calls a constuctor witht the specified arguments.
     *
     * @param constructor The constructor.
     * @param args        The arguments.
     * @param <T>         The Class type.
     *
     * @return An instance of the class that the constructor is for.
     */
    public static <T> T newInstance(Constructor<T> constructor, Object[] args)
    {
        try
        {
            return constructor.newInstance(args);
        }
        catch (InstantiationException e)
        {
            throw new ReflectionUtilsException("InstantiationException", e);
        }
        catch (IllegalAccessException e)
        {
            throw new ReflectionUtilsException("IllegalAccessException", e);
        }
        catch (InvocationTargetException e)
        {
            throw new ReflectionUtilsException("InvocationTargetException", e);
        }
    }

    /**
     * Gets the constructor of a class that takes the specified set of arguments if any matches. If no matching
     * constructor is found then a runtime exception is raised.
     *
     * @param cls  The class to get a constructor from.
     * @param args The arguments to match.
     * @param <T>  The class type.
     *
     * @return The constructor.
     */
    public static <T> Constructor<T> getConstructor(Class<T> cls, Class[] args)
    {
        try
        {
            return cls.getConstructor(args);
        }
        catch (NoSuchMethodException e)
        {
            throw new ReflectionUtilsException("NoSuchMethodException", e);
        }
    }
}
