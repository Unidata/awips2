/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Utility to create dimensions of type {@link NcDimension} and variables of
 * type {@link NcVariable}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 6, 2013  ?          bclement    Initial creation
 * Oct 26, 2016 5631       bkowal      It is now possible to create a dimension without
 *                                     automatically creating an associated variable.
 *
 * </pre>
 *
 * @author bclement
 */
public class NcFactory {

    private static final ConcurrentHashMap<Class<?>, Constructor<?>> cmap = new ConcurrentHashMap<Class<?>, Constructor<?>>();

    public static <T extends NcDimension> T createDim(int fileId, String name,
            int len, Class<T> dimClass)
                    throws SecurityException, NoSuchMethodException,
                    IllegalArgumentException, InstantiationException,
                    IllegalAccessException, InvocationTargetException {
        return createDim(fileId, name, len, dimClass, true);
    }

    @SuppressWarnings("unchecked")
    public static <T extends NcDimension> T createDim(int fileId, String name,
            int len, Class<T> dimClass, boolean createVar)
                    throws SecurityException, NoSuchMethodException,
                    IllegalArgumentException, InstantiationException,
                    IllegalAccessException, InvocationTargetException {
        Constructor<T> constructor = (Constructor<T>) cmap.get(dimClass);
        if (constructor == null) {
            synchronized (cmap) {
                constructor = (Constructor<T>) cmap.get(dimClass);
                if (constructor == null) {
                    constructor = dimClass.getConstructor(int.class,
                            String.class, int.class, boolean.class);
                    cmap.put(dimClass, constructor);
                }
            }
        }
        return constructor.newInstance(fileId, name, len, createVar);
    }

    @SuppressWarnings("unchecked")
    public static <T extends NcVariable> T createVar(int fileId, String name,
            NcDimension[] dims, Class<T> varClass)
                    throws SecurityException, NoSuchMethodException,
                    IllegalArgumentException, InstantiationException,
                    IllegalAccessException, InvocationTargetException {
        Constructor<T> constructor = (Constructor<T>) cmap.get(varClass);
        if (constructor == null) {
            synchronized (cmap) {
                constructor = (Constructor<T>) cmap.get(varClass);
                if (constructor == null) {
                    constructor = varClass.getConstructor(int.class,
                            String.class, NcDimension[].class);
                    cmap.put(varClass, constructor);
                }
            }
        }
        return constructor.newInstance(fileId, name, dims);
    }

}
