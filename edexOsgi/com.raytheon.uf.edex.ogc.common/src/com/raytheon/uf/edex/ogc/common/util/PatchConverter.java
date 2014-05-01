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

import java.util.Calendar;
import java.util.Date;

import javax.xml.bind.DatatypeConverter;

/**
 * Workaround for race condition for multiple versions of bean utils. Sometimes
 * the custom data and calendar converters don't get registered correctly
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
public class PatchConverter extends DefaultConverter {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.util.DefaultConverter#convertObject(java
     * .lang.String, java.lang.Class)
     */
    @Override
    public Object convertObject(String value, Class<?> desiredClass) {
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
        return super.convertObject(value, desiredClass);
    }


}
