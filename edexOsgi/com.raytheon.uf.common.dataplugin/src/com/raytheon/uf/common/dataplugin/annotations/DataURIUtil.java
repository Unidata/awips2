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
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import org.apache.commons.beanutils.PropertyUtils;

import com.raytheon.uf.common.dataplugin.IPluginClassMapper;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ConvertUtil;

/**
 * Utility class for working with dataURIs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 07, 2008 1533       bphillip    Initial Checkin
 * Mar 29, 2013 1638       mschenke    Added method for recursively getting all
 *                                     dataURI fields for an object
 * Apr 18, 2013 1638       mschenke    Moved dataURI map generation into here
 *                                     from PluginDataObject
 * May 15, 2013 1869       bsteffen    Move uri map creation from RecordFactory.
 * May 16, 2013 1869       bsteffen    Rewrite dataURI property mappings.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DataURIUtil {

    private static final String PLUGIN_NAME_KEY = "pluginName";

    private static final Pattern SEPARATOR_PATTERN = Pattern
            .compile(DataURI.SEPARATOR);

    private static final Pattern UNDERSCORE_PATTERN = Pattern.compile("_");

    private static final Pattern SPACE_PATTERN = Pattern.compile(" ");

    /*
     * Compares two fields with the DataURI annotations based off the position.
     */
    private static final Comparator<Field> dataURIAnnotationComparator = new Comparator<Field>() {

        @Override
        public int compare(Field f1, Field f2) {
            int i1 = f1.getAnnotation(DataURI.class).position();
            int i2 = f2.getAnnotation(DataURI.class).position();
            return (i1 < i2 ? -1 : (i1 == i2 ? 0 : 1));
        }

    };

    private static IPluginClassMapper classMapper;

    /*
     * Internal cache to avoid constant reflection
     */
    private static Map<Class<?>, DataURIFieldAccess[]> uriFieldMap = new ConcurrentHashMap<Class<?>, DataURIFieldAccess[]>();

    /**
     * The classMapper needs to be injected to allow this class to create new
     * PDOs based off the pluginName in a dataURI or map.
     * 
     * @param classMapper
     */
    public static void setClassMapper(IPluginClassMapper classMapper) {
        DataURIUtil.classMapper = classMapper;
    }

    /**
     * Build a new dataURI based off the dataURI fields in the PluginDataObject.
     * 
     * @param pdo
     * @return
     * @throws PluginException
     */
    public static String createDataURI(PluginDataObject pdo)
            throws PluginException {
        StringBuilder uri = new StringBuilder(160);
        addToDataURI(uri, pdo.getPluginName());
        for (DataURIFieldAccess access : getAccess(pdo.getClass())) {
            addToDataURI(uri, access.getFieldValue(pdo));
        }
        return SPACE_PATTERN.matcher(uri).replaceAll("_");
    }

    /**
     * Create a dataURI based off the dataURI fields in the dataMap.
     * 
     * @param dataMap
     * @return
     * @throws PluginException
     */
    public static String createDataURI(Map<String, Object> dataMap)
            throws PluginException {
        String pluginName = dataMap.get(PLUGIN_NAME_KEY).toString();
        StringBuilder uri = new StringBuilder(160);
        addToDataURI(uri, pluginName);
        for (DataURIFieldAccess access : getAccess(pluginName)) {
            addToDataURI(uri, dataMap.get(access.getFieldName()));
        }
        return SPACE_PATTERN.matcher(uri).replaceAll("_");
    }

    /*
     * Properly formats an arbitrary object into a dataURI.
     */
    private static void addToDataURI(StringBuilder uri, Object property) {
        uri.append("/");
        if (property == null) {
            uri.append("null");
        } else if (property instanceof Calendar) {
            uri.append(TimeUtil.formatCalendar((Calendar) property));
        } else {
            uri.append(SEPARATOR_PATTERN.matcher(String.valueOf(property))
                    .replaceAll("_"));
        }
    }

    /**
     * Parse a dataURI and set the dataURIFields into a dataMap
     * 
     * @param dataURI
     * @return
     * @throws PluginException
     */
    public static Map<String, Object> createDataURIMap(String dataURI)
            throws PluginException {
        List<String> tokens = tokenizeURI(dataURI);
        Map<String, Object> dataMap = new HashMap<String, Object>(
                (int) (tokens.size() / 0.75f + 1), 0.75f);
        String pluginName = tokens.get(0);
        tokens = tokens.subList(1, tokens.size());
        dataMap.put(PLUGIN_NAME_KEY, pluginName);
        DataURIFieldAccess[] access = getAccess(pluginName);
        for (int i = 0; i < access.length; i += 1) {
            dataMap.put(access[i].getFieldName(),
                    access[i].getFieldValue(tokens.get(i)));
        }
        return dataMap;
    }

    /**
     * Populate a dataMap based off the dataURI fields in the PluginDataObject.
     * 
     * @param pdo
     * @return
     * @throws PluginException
     */
    public static Map<String, Object> createDataURIMap(PluginDataObject pdo)
            throws PluginException {
        return createDataURIMap((Object) pdo);
    }

    /**
     * Create a new PluginDataObject based off the dataURI. THe class of the
     * result object is based off the pluginName in the dataURI and all fields
     * present in the DataURI are set into the PDO.
     * 
     * @param dataURI
     * @return
     * @throws PluginException
     */
    public static PluginDataObject createPluginDataObject(String dataURI)
            throws PluginException {
        PluginDataObject pdo = null;
        List<String> tokens = tokenizeURI(dataURI);
        Class<PluginDataObject> clazz = getPluginRecordClass(tokens.get(0));
        try {
            pdo = clazz.newInstance();
        } catch (Exception e) {
            throw new PluginException(e);
        }
        populatePluginDataObject(pdo, tokens);
        pdo.setDataURI(dataURI);
        return pdo;
    }

    /**
     * Create a new PluginDataObject based off the dataMap. The class of the
     * result object is based off the pluginName mapping and all dataURI fields
     * present in the map are set into the PDO.
     * 
     * @param dataMap
     * @return
     * @throws PluginException
     */
    public static PluginDataObject createPluginDataObject(
            Map<String, Object> dataMap) throws PluginException {
        PluginDataObject pdo = null;
        Class<PluginDataObject> clazz = getPluginRecordClass(dataMap.get(
                PLUGIN_NAME_KEY).toString());
        try {
            pdo = clazz.newInstance();
        } catch (Exception e) {
            throw new PluginException(e);
        }
        populatePluginDataObject(pdo, dataMap);
        return pdo;
    }

    /**
     * Populate an existing PluginDataObjects with the DataURI fields from the
     * dataMap.
     * 
     * @param pdo
     * @param dataMap
     * @throws PluginException
     */
    public static void populatePluginDataObject(PluginDataObject pdo,
            Map<String, Object> dataMap) throws PluginException {
        pdo.setPluginName(dataMap.get(PLUGIN_NAME_KEY).toString());
        populateObject(pdo, dataMap);
    }

    /**
     * Populate an existing PluginDataObject using fields parsed from the
     * dataURI.
     * 
     * @param pdo
     * @param dataURI
     * @throws PluginException
     */
    public static void populatePluginDataObject(PluginDataObject pdo,
            String dataURI) throws PluginException {
        populatePluginDataObject(pdo, tokenizeURI(dataURI));
        pdo.setDataURI(dataURI);
    }

    /**
     * Create a dataURIMap from any object with dataURI annotations.
     * 
     * @param object
     * @return
     * @throws PluginException
     */
    public static Map<String, Object> createDataURIMap(Object object)
            throws PluginException {
        DataURIFieldAccess[] accessArray = getAccess(object.getClass());
        Map<String, Object> dataMap = new HashMap<String, Object>(
                (int) (accessArray.length / 0.75f + 2), 0.75f);
        if (object instanceof PluginDataObject) {
            dataMap.put(PLUGIN_NAME_KEY,
                    ((PluginDataObject) object).getPluginName());
        }
        for (DataURIFieldAccess access : accessArray) {
            dataMap.put(access.getFieldName(), access.getFieldValue(object));
        }
        return dataMap;
    }

    /**
     * Populate an existing object from the dataURI fields in dataMap.
     * 
     * @param object
     * @param dataMap
     * @throws PluginException
     */
    public static void populateObject(Object object, Map<String, Object> dataMap)
            throws PluginException {
        for (DataURIFieldAccess access : getAccess(object.getClass())) {
            access.setFieldValue(object, dataMap.get(access.getFieldName()));
        }
    }

    /*
     * Populate a PDO with the tokens from a parsed DataURI
     */
    private static void populatePluginDataObject(PluginDataObject pdo,
            List<String> uriTokens) throws PluginException {
        pdo.setPluginName(uriTokens.get(0));
        uriTokens = uriTokens.subList(1, uriTokens.size());
        DataURIFieldAccess[] access = getAccess(pdo.getClass());
        for (int i = 0; i < access.length; i += 1) {
            access[i].setFieldValue(pdo,
                    access[i].getFieldValue(uriTokens.get(i)));
        }
    }

    /*
     * Split a URI on the seperator and remove empty first element.
     */
    private static List<String> tokenizeURI(String dataURI) {
        dataURI = UNDERSCORE_PATTERN.matcher(dataURI).replaceAll(" ");
        String[] tokens = SEPARATOR_PATTERN.split(dataURI);
        return Arrays.asList(tokens).subList(1, tokens.length);
    }

    private static DataURIFieldAccess[] getAccess(String pluginName)
            throws PluginException {
        return getAccess(getPluginRecordClass(pluginName));
    }

    private static DataURIFieldAccess[] getAccess(Class<?> clazz) {
        DataURIFieldAccess[] result = uriFieldMap.get(clazz);
        if (result == null) {
            result = getAccess(clazz, Collections.<String> emptyList())
                    .toArray(new DataURIFieldAccess[0]);
            uriFieldMap.put(clazz, result);
        }
        return result;
    }

    private static List<DataURIFieldAccess> getAccess(Class<?> clazz,
            List<String> parents) {
        List<Field> fields = getOrderedDataURIFields(clazz);
        List<DataURIFieldAccess> accessors = new ArrayList<DataURIFieldAccess>();
        for (Field field : fields) {
            List<String> names = new ArrayList<String>(parents);
            names.add(field.getName());
            Class<?> type = field.getType();
            if (field.getAnnotation(DataURI.class).embedded()) {
                accessors.addAll(getAccess(type, names));
            } else {
                accessors.add(new DataURIFieldAccess(names, type));
            }
        }
        return accessors;
    }

    private static List<Field> getOrderedDataURIFields(Class<?> clazz) {
        List<Field> fields = new ArrayList<Field>();
        Class<?> currentClass = clazz;
        while (currentClass != null) {
            for (Field field : currentClass.getDeclaredFields()) {
                if (field.getAnnotation(DataURI.class) != null) {
                    fields.add(field);
                }
            }
            currentClass = currentClass.getSuperclass();
        }
        Collections.sort(fields, dataURIAnnotationComparator);
        return fields;
    }

    public static Class<PluginDataObject> getPluginRecordClass(String pluginName)
            throws PluginException {
        if (classMapper == null) {
            throw new PluginException(
                    "No PluginClassMapper is registered with DataURIUtil.");
        }
        return classMapper.getPluginRecordClass(pluginName);
    }

    /*
     * Class which remembers the fieldNames and class of a dataURI field to make
     * parsing faster.
     */
    private static class DataURIFieldAccess {

        /**
         * List of fieldNames, used for finding objects in a PDO, for example
         * {"location", "longitude"}
         */
        private final String[] fieldNames;

        /**
         * Compiled field names, used as map keys, for example
         * "location.longitude"
         */
        private final String fieldName;

        /**
         * Class of the oibject that is in the dataURI.
         */
        private final Class<?> fieldClass;

        public DataURIFieldAccess(List<String> fieldNames, Class<?> fieldClass) {
            this.fieldNames = fieldNames.toArray(new String[0]);
            StringBuilder fieldName = new StringBuilder(this.fieldNames[0]);
            for (int i = 1; i < this.fieldNames.length; i += 1) {
                fieldName.append(".").append(this.fieldNames[i]);
            }
            this.fieldName = fieldName.toString();
            this.fieldClass = fieldClass;
        }

        public String getFieldName() {
            return fieldName;
        }

        /**
         * Extract the field value from a PDO.
         * 
         * @param pdo
         * @return
         * @throws PluginException
         */
        public Object getFieldValue(Object pdo) throws PluginException {
            try {
                Object object = pdo;
                for (String fieldName : fieldNames) {
                    object = PropertyUtils.getProperty(object, fieldName);
                    if (object == null) {
                        break;
                    }
                }
                return object;
            } catch (Exception e) {
                throw new PluginException(e);
            }
        }

        /**
         * Convert the provided string into the correct type for the field.
         * 
         * @param stringValue
         * @return
         * @throws PluginException
         */
        public Object getFieldValue(String stringValue) throws PluginException {
            return ConvertUtil.convertObject(stringValue, fieldClass);
        }

        /**
         * Set the fieldValue into the PDO.
         * 
         * @param pdo
         * @param fieldValue
         * @throws PluginException
         */
        public void setFieldValue(Object pdo, Object fieldValue)
                throws PluginException {
            Object source = pdo;
            try {
                for (int i = 0; i < fieldNames.length - 1; i += 1) {
                    Object obj = PropertyUtils.getProperty(source,
                            fieldNames[i]);
                    if (obj == null) {
                        obj = PropertyUtils.getPropertyType(source,
                                fieldNames[i]).newInstance();
                        PropertyUtils.setProperty(source, fieldNames[i], obj);
                    }
                    source = obj;
                }
                PropertyUtils.setProperty(source,
                        fieldNames[fieldNames.length - 1], fieldValue);
            } catch (Exception e) {
                throw new PluginException(e);
            }
        }
    }
}
