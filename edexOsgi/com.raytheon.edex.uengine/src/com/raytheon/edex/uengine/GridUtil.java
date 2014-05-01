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

package com.raytheon.edex.uengine;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

/**
 * Util class for grids. Extracted from original uEngine's TermQueryIndex task.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class GridUtil {

    /**
     * Creates a Meta-Data map from the meta-data query result object.
     * 
     * @param result
     *            the query result object
     * @return the Meta-Data map
     * 
     * @throws Exception
     *             in the event a serious error occurs
     */
    public static Map<String, Object> createMetaDataMap(Object result)
            throws Exception {
        HashMap<String, Object> retVal = new HashMap<String, Object>();
        Class<?> aClass = Class.forName(result.getClass().getName());

        while (aClass != null) {
            for (Field aField : aClass.getDeclaredFields()) {
                if (!Modifier.isStatic(aField.getModifiers())) {
                    String name = aField.getName();
                    String getter = "get" + name.substring(0, 1).toUpperCase()
                            + name.substring(1);
                    try {
                        Method aMethod = aClass.getMethod(getter,
                                (Class[]) null);
                        Object value = aMethod.invoke(result, (Object[]) null);
                        if (value != null) {
                            // System.out.println(name + " : " + value);
                            retVal.put(name, value);
                        }
                    } catch (NoSuchMethodException e) {
                        // just skip it!
                    }
                }
            }
            aClass = aClass.getSuperclass();
        }
        return retVal;
    }

}
