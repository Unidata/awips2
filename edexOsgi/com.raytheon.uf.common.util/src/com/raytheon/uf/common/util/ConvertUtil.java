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

import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.Date;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.beanutils.Converter;

/**
 * Utilities for converting objects, originally taken from
 * com.raytheon.edex.common.Util. Converters should be registered through
 * Spring.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ConvertUtil {

    private static ConvertUtil instance = new ConvertUtil();

    private ConvertUtil() {

    }

    /**
     * Used to register converters from Spring xml
     * 
     * @param converter
     * @param clazz
     * @return
     */
    public static Object registerConverter(Converter converter, Class<?> clazz) {
        ConvertUtils.register(converter, clazz);
        return instance;
    }

    /**
     * Converts a string to the desired object type
     * 
     * @param value
     *            String representation of an object
     * @param desiredClass
     *            The class to convert to
     * @return The converted object
     * @throws Exception
     *             If the string value cannot be converted to the desired class
     *             type
     */
    @SuppressWarnings("unchecked")
    public static Object convertObject(String value, Class<?> desiredClass) {
        if (value == null || value.equals("null")) {
            return null;
        }
        if (desiredClass.equals(String.class)) {
            return value;
        }
        if (desiredClass.isEnum()) {
            return Enum.valueOf((Class<? extends Enum>) desiredClass, value);
        }
        if (desiredClass.equals(Calendar.class)) {
            try {
                // see if string is in ISO 8601
                return DatatypeConverter.parseDateTime(value);
            } catch (Exception e) {
                // let convertUtils try
            }
        }
        if (desiredClass.equals(Date.class)) {
            try {
                // see if string is in ISO 8601
                return DatatypeConverter.parseDateTime(value).getTime();
            } catch (Exception e) {
                // let convertUtils try
            }
        }
        return ConvertUtils.convert(value, desiredClass);
    }

    public static String toString(Object obj) {
        return ConvertUtils.convert(obj);
    }

    public static Object convertAsType(String value, Class<?> entity,
            String fieldName) throws SecurityException, NoSuchFieldException {
        Field field = getField(fieldName, entity);
        return convertObject(value, field.getType());
    }

    public static Object convertAsType(String value, Class<?> entity,
            String[] fieldPath) throws SecurityException, NoSuchFieldException {
        Field f = getField(fieldPath[0], entity);
        for (int i = 1; i < fieldPath.length; ++i) {
            f = getField(fieldPath[i], f.getType());
        }
        return convertObject(value, f.getType());
    }

    protected static Field getField(String fieldName, Class<?> c)
            throws SecurityException, NoSuchFieldException {
        Field rval;
        try {
            rval = c.getDeclaredField(fieldName);
        } catch (NoSuchFieldException e) {
            Class<?> parent = c.getSuperclass();
            if (parent.isInstance(Object.class)) {
                throw e;
            } else {
                rval = getField(fieldName, c.getSuperclass());
            }
        }
        return rval;
    }

}
