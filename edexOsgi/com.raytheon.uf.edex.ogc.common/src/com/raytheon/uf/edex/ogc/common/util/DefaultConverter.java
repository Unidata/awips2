/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.util;

import java.lang.reflect.Field;

import com.raytheon.uf.common.convert.ConvertUtil;


/**
 * Converter that uses convertUtil
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DefaultConverter implements Converter {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.util.Converter#convertObject(java.lang
     * .String, java.lang.Class)
     */
    @Override
    public Object convertObject(String value, Class<?> desiredClass) {
        return ConvertUtil.convertObject(value, desiredClass);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.util.Converter#toString(java.lang.Object)
     */
    @Override
    public String toString(Object obj) {
        return ConvertUtil.toString(obj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.util.Converter#convertAsType(java.lang
     * .String, java.lang.Class, java.lang.String)
     */
    @Override
    public Object convertAsType(String value, Class<?> entity, String fieldName)
            throws SecurityException, NoSuchFieldException {
        return ConvertUtil.convertAsType(value, entity, fieldName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.util.Converter#convertAsType(java.lang
     * .String, java.lang.Class, java.lang.String[])
     */
    @Override
    public Object convertAsType(String value, Class<?> entity,
            String[] fieldPath) throws SecurityException, NoSuchFieldException {
        return ConvertUtil.convertAsType(value, entity, fieldPath);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.util.Converter#getFields(java.lang.Class,
     * java.lang.String[])
     */
    @Override
    public Field[] getFields(Class<?> entity, String[] fieldPath)
            throws SecurityException, NoSuchFieldException {
        return ConvertUtil.getFields(entity, fieldPath);
    }

}
