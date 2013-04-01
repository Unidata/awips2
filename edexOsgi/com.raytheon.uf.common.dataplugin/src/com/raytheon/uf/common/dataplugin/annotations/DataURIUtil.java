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

package com.raytheon.uf.common.dataplugin.annotations;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Utility class for working with dataURIs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/07/2008   1533        bphillip   Initial Checkin
 * Mar 29, 2013 1638       mschenke    Added method for recursively getting all
 *                                     dataURI fields for an object
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DataURIUtil {

    /** The singleton instance */
    private static DataURIUtil instance;

    /** Map of classes and the DataURI annotated fields */
    private Map<Class<?>, Field[]> uriFieldMap;

    /**
     * Constructs the singleton instance
     */
    private DataURIUtil() {
        uriFieldMap = new HashMap<Class<?>, Field[]>();
    }

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public synchronized static DataURIUtil getInstance() {
        if (instance == null) {
            instance = new DataURIUtil();
        }
        return instance;
    }

    public Field[] getAllDataURIFields(Class<?> obj) {
        List<Field> fields = new ArrayList<Field>();
        getAllDataURIFields(obj, fields);
        return fields.toArray(new Field[0]);
    }

    private void getAllDataURIFields(Class<?> obj, List<Field> fields) {
        for (Field field : getDataURIFields(obj)) {
            if (field.getAnnotation(DataURI.class).embedded()) {
                getAllDataURIFields(field.getType(), fields);
            } else {
                fields.add(field);
            }
        }
    }

    /**
     * Retrieves an ordered listing of all fields annotated as DataURI fields.
     * 
     * @param obj
     *            The object to examine
     * @return The ordered array of all fields annotated as DataURI fields
     */
    public Field[] getDataURIFields(Class<?> obj) {

        // Check map first
        if (uriFieldMap.containsKey(obj)) {
            return uriFieldMap.get(obj);
        }

        // Iterate through the class hierarchy
        List<Field> fields = new ArrayList<Field>();
        Class<?> currentClass = obj;
        while (currentClass != null) {
            fields.addAll(Arrays.asList(currentClass.getDeclaredFields()));
            currentClass = currentClass.getSuperclass();
        }

        // Count the fields annotated with the DataURI annotation
        int fieldCount = 0;
        for (Field field : fields) {
            if (field.getAnnotation(DataURI.class) != null) {
                fieldCount++;
            }
        }
        // Sort the annotations into an array
        Field[] annotatedFields = new Field[fieldCount];
        for (Field field : fields) {
            DataURI annotation = field.getAnnotation(DataURI.class);
            if (annotation != null) {
                annotatedFields[annotation.position()] = field;
            }
        }

        // Put fields in map and return
        uriFieldMap.put(obj, annotatedFields);
        return annotatedFields;
    }
}
