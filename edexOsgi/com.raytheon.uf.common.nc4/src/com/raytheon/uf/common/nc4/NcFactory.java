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
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 6, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class NcFactory {

    private static final ConcurrentHashMap<Class<?>, Constructor<?>> cmap = new ConcurrentHashMap<Class<?>, Constructor<?>>();

    @SuppressWarnings("unchecked")
    public static <T extends NcDimension> T createDim(int fileId, String name,
            int len, Class<T> dimClass) throws SecurityException,
            NoSuchMethodException, IllegalArgumentException,
            InstantiationException, IllegalAccessException,
            InvocationTargetException {
        Constructor<T> constructor = (Constructor<T>) cmap.get(dimClass);
        if (constructor == null) {
            synchronized (cmap) {
                constructor = (Constructor<T>) cmap.get(dimClass);
                if (constructor == null) {
                    constructor = dimClass.getConstructor(int.class,
                            String.class, int.class);
                    cmap.put(dimClass, constructor);
                }
            }
        }
        return constructor.newInstance(fileId, name, len);
    }

    @SuppressWarnings("unchecked")
    public static <T extends NcVariable> T createVar(int fileId, String name,
            NcDimension[] dims, Class<T> varClass) throws SecurityException,
            NoSuchMethodException, IllegalArgumentException,
            InstantiationException, IllegalAccessException,
            InvocationTargetException {
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
