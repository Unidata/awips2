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

import java.util.HashMap;
import java.util.Map;

import net.sf.cglib.beans.BeanMap;
import net.sf.cglib.beans.BeanMap.Generator;
import net.sf.cglib.reflect.FastClass;

/**
 * Provides a cache of cglib/reflection objects
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2008  #1448      chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SerializationCache {

    /** The beanmap generator cache */
    private static Map<Class<?>, BeanMap.Generator> generators = new HashMap<Class<?>, BeanMap.Generator>();;

    /** The beanmap cache */
    private static Map<Class<?>, BeanMap> beanMaps = new HashMap<Class<?>, BeanMap>();

    /** The fastclass cache */
    private static Map<String, FastClass> classCache = new HashMap<String, FastClass>();

    /**
     * protected constructor
     */
    private SerializationCache() {

    }

    /**
     * Return the beanmap
     * 
     * @param beanMap
     * @param obj
     */
    public static void returnBeanMap(BeanMap beanMap, Object obj) {
        synchronized (beanMaps) {
            beanMap.setBean(null);
            beanMaps.put(obj.getClass(), beanMap);
        }
    }

    /**
     * Get a beanmap
     * 
     * NOTE: the user must return the beanmap when done
     * 
     * @param obj
     *            the object to get a beanmap for
     * @return a beanmap representing an object
     */
    public static BeanMap getBeanMap(Object obj) {
        BeanMap bm = null;
        synchronized (beanMaps) {
            bm = beanMaps.remove(obj.getClass());
        }
        if (bm != null) {
            bm.setBean(obj);
            return bm;
        }

        Generator generator = null;
        synchronized (generators) {
            generator = generators.remove(obj.getClass());
        }
        if (generator == null) {
            generator = new BeanMap.Generator();
        }

        generator.setClassLoader(SerializationCache.class.getClassLoader());
        generator.setBean(obj);
        bm = generator.create();
        generator.setBean(null);

        synchronized (generators) {
            generators.put(obj.getClass(), generator);
        }

        return bm;
    }

    /**
     * Get the fastclass that represents a class name
     * 
     * @param name
     *            the name
     * @return the fastclass
     * @throws Exception
     */
    public static FastClass getFastClass(String name) throws Exception {
        synchronized (classCache) {
            FastClass fc = classCache.get(name);
            if (fc == null) {
                fc = FastClass.create(
                        SerializationCache.class.getClassLoader(),
                        Class.forName(name));
                classCache.put(name, fc);
            }
            return fc;
        }

    }

}
