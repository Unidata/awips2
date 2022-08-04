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
package com.raytheon.uf.common.hydro.util;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Hydro Database Utilities
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#      Engineer   Description
 * -----------------------------------------------------------
 * Jul 9, 2008  1194          mpduff    Initial creation.
 * Mar 7, 2014  16692                   lbousaidi Any Forecast source other than
 *                                      H*,P*,Q*,T* should be handled by fcstother.
 * Oct 10, 2015 17935                   special char (e.g apostrophe) can not be saved/updated in Hyrobase
 * Jan 15, 2016 DCS18180     JingtaoD   code improvement based on code review for DR17935
 * Mar 10, 2017 29276       gvalenzuela Fix for unable to create/update Location QC records for DR19573
 * Mar 13, 2017 29276       gvalenzuela Fix null handling for clazz field collector
 * Mar 27, 2018 DR20497     JLinahan    DbUtils should return arealobs as tablename when querying PP PM data
 * Sep 07, 2018 6979        mpduff      SonarQube cleanup.
 * </pre>
 * 
 * @author mpduff
 */

public class DbUtils {
    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbUtils.class);

    private static final Field[] NO_FIELDS = {};

    /**
     * replace string fields in table class which contains apostrophe
     * 
     * @param curData
     *            , retData
     */
    public static <T extends Object> void escapeSpecialCharforData(T curData,
            T retData) {

        copyFields(curData, retData);

        Class<?> c = retData.getClass();

        Field fields[] = getAllDeclaredFields(c);

        for (Field f : fields) {
            try {
                if (f.getType().isAssignableFrom(String.class)) {

                    f.setAccessible(true);
                    if (f.get(retData) != null) {
                        String value = f.get(retData).toString();

                        if (value != null) {
                            if (value.contains("'")) {
                                value = value.replace("'", "''");
                                f.set(retData, value);
                            }
                        }
                    }
                }
            } catch (IllegalAccessException e) {
                statusHandler.handle(Priority.ERROR,
                        "Error to escape special characters for object "
                                + curData.toString(),
                        e);
            }
        }

    }

    /**
     * replace apostrophe for string
     * 
     * @param strValue
     * @return strValue
     */
    public static String escapeSpecialCharforStr(String strValue) {

        if (strValue != null) {
            if (strValue.contains("'")) {
                strValue = strValue.replace("'", "''");
            }
        }
        return strValue;
    }

    /**
     * Copy the fields in origData to copiedData, later replace special char in
     * copiedData object
     * 
     * @param origData
     * @param copiedData
     */
    public static <T extends Object> void copyFields(T origData, T copiedData) {

        Field[] origFields = getAllDeclaredFields(origData.getClass());
        Object value = null;
        for (Field field : origFields) {

            try {

                field.setAccessible(true);
                value = field.get(origData);
                field.set(copiedData, value);

            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error to copy object from " + origData.toString()
                                + "to" + copiedData.toString(),
                        e);
            }
        }

    }

    /**
     * Traverses the class hierarchy and picks up all declared fields. The
     * methods goes down to when the class is null or the standard Object class.
     * 
     * @param clazz
     *            the class to traverse
     * @return Field[] array
     */
    public static Field[] getAllDeclaredFields(Class<? extends Object> clazz) {

        if (clazz == null) {
            return NO_FIELDS;
        }

        List<Field> fields = new ArrayList<>();
        fields.addAll(Arrays.asList(clazz.getDeclaredFields()));

        if (Object.class != clazz) {
            fields.addAll(
                    Arrays.asList(getAllDeclaredFields(clazz.getSuperclass())));
        }

        return fields.toArray(new Field[fields.size()]);
    }

}
