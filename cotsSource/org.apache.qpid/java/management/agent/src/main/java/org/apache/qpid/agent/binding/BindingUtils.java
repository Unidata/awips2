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

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class BindingUtils
{
    private static Log log = LogFactory.getLog(BindingUtils.class);

    public static Object get(PropertyBinding property, Object managed)
    {
        String name = property.getName();
        return get(name, managed);
    }

    public static void set(PropertyBinding property, Object value,
            Object managed)
    {
        String name = property.getName();
        TypeBinding type = property.getType();
        try
        {
            Method meth = managed.getClass().getMethod(accessor("set", name),
                    type.getJavaClass());
            meth.invoke(managed, value);
        } catch (NoSuchMethodException e)
        {
            throw new BindingException(e);
        } catch (IllegalAccessException e)
        {
            throw new BindingException(e);
        } catch (InvocationTargetException e)
        {
            throw new BindingException(e.getTargetException());
        }
    }

    public static Object[] invoke(MethodBinding method, Object managed,
            Object... args)
    {
        log.debug(String.format("Invoking %s on %s", method.getName(), managed
                .getClass()));
        List<ParameterBinding> in = method.getInParameters();
        List<ParameterBinding> out = method.getOutParameters();
        Class<?>[] classes = new Class<?>[in.size()];
        int idx = 0;
        for (ParameterBinding p : in)
        {
            classes[idx++] = p.getType().getJavaClass();
        }
        Object result;
        try
        {
            Method meth = managed.getClass().getMethod(method.getName(),
                    classes);
            result = meth.invoke(managed, args);
        } catch (NoSuchMethodException e)
        {
            throw new BindingException(e);
        } catch (IllegalAccessException e)
        {
            throw new BindingException(e);
        } catch (InvocationTargetException e)
        {
            throw new BindingException(e.getTargetException());
        }
        Object[] results = new Object[out.size()];
        // XXX: need better way to distinguish this case
        if (out.size() == 1 && out.get(0).getName().equals("result"))
        {
            results[0] = result;
        } else
        {
            for (int i = 0; i < results.length; i++)
            {
                results[i] = get(out.get(i).getName(), result);
            }
        }
        return results;
    }

    public static String accessor(String pfx, String property)
    {
        return pfx + Character.toUpperCase(property.charAt(0))
                + property.substring(1);
    }

    public static Object get(String name, Object obj)
    {
        Object returnValue = null;
        try
        {
            BeanInfo info = Introspector.getBeanInfo(obj.getClass());
            PropertyDescriptor[] pds = info.getPropertyDescriptors();
            for (PropertyDescriptor pd : pds)
            {
                if (pd.getName().equals(name))
                {
                    Method getMethod = pd.getReadMethod();
                    returnValue = getMethod.invoke(obj);
                    break;
                }
            }
        } catch (Exception e)
        {
            throw new BindingException(e);
        }
        return returnValue;
    }
}
