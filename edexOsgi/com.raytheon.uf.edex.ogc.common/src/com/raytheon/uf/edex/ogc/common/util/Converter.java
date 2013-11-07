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

/**
 * Generic object converter interface for OGC
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
public interface Converter {

    public Object convertObject(String value, Class<?> desiredClass);

    public String toString(Object obj);

    public Object convertAsType(String value, Class<?> entity, String fieldName)
            throws SecurityException, NoSuchFieldException;

    public Object convertAsType(String value, Class<?> entity,
            String[] fieldPath) throws SecurityException, NoSuchFieldException;

    public Field[] getFields(Class<?> entity, String[] fieldPath)
            throws SecurityException, NoSuchFieldException;

}
