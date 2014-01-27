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
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract and removed setPluginName.
 * Sep 24, 2013 2081       mschenke    Removed special handling of spaces and only handle 
 *                                     {@link DataURI#SEPARATOR} specially
 * Oct  4, 2013 2081       mschenke    Refactored for custom uri field conversion
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DataURIUtil {

    private static final String PLUGIN_NAME_KEY = PluginDataObject.PLUGIN_NAME_ID;

    private static final String FIELD_SEPARATOR = ".";

    private static final Pattern FIELD_SEPARATOR_PATTERN = Pattern.compile("["
            + FIELD_SEPARATOR + "]");


    private static final String DATAURI_SEPARATOR_ENCODED = "%2F";

    private static final String DATAURI_SEPARATOR_ESCAPE_CHAR = "%";

    private static final String DATAURI_SEPARATOR_CHAR_ENCODED = "%25";

    private static final Pattern DATAURI_SEPARATOR_ENCODED_PATTERN = Pattern
            .compile(DATAURI_SEPARATOR_ENCODED);

    private static final Pattern DATAURI_SEPARATOR_PATTERN = Pattern
            .compile(DataURI.SEPARATOR);

    private static final Pattern DATAURI_SEPARATED_ESCAPE_CHAR_PATTERN = Pattern
            .compile(DATAURI_SEPARATOR_ESCAPE_CHAR);

    private static final Pattern DATAURI_SEPARATOR_CHAR_ENCODED_PATTERN = Pattern
            .compile(DATAURI_SEPARATOR_CHAR_ENCODED);

    private static IPluginClassMapper classMapper;

    /*
     * Internal cache to avoid constant reflection
     */
    private static Map<Class<?>, DataURIFieldAccessCache> uriFieldMap = new ConcurrentHashMap<Class<?>, DataURIFieldAccessCache>();

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
        DataURIFieldAccessCache cache = getAccessCache(pdo.getClass());
        StringBuilder uri = new StringBuilder(160);
        addToDataURI(uri, pdo.getPluginName());
        for (DataURIFieldAccess access : cache.getDataURIFields()) {
            addToDataURI(uri, access.getFieldString(pdo));
        }
        return uri.toString();
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
        String pluginName = (String) dataMap.get(PLUGIN_NAME_KEY);
        DataURIFieldAccessCache cache = getAccessCache(pluginName);
        StringBuilder uri = new StringBuilder(160);
        addToDataURI(uri, pluginName);
        for (DataURIFieldAccess access : cache.getDataURIFields()) {
            addToDataURI(uri,
                    access.toFieldString(dataMap.get(access.getFieldName())));
        }
        return uri.toString();
    }

    /*
     * Properly formats an arbitrary object into a dataURI.
     */
    private static void addToDataURI(StringBuilder uri, String property) {
        // This is done so if the property actually contained '%2F' that
        // wouldn't get converted to '/' when tokenized. %2F becomes %252F
        // because the '%' is replaced with '%25'
        String escapeCharEscaped = DATAURI_SEPARATED_ESCAPE_CHAR_PATTERN
                .matcher(property).replaceAll(DATAURI_SEPARATOR_CHAR_ENCODED);
        // Now replace any '/' with %2F to escape slashes in the property
        String fullyEscapedProperty = DATAURI_SEPARATOR_PATTERN.matcher(
                escapeCharEscaped).replaceAll(DATAURI_SEPARATOR_ENCODED);

        uri.append(DataURI.SEPARATOR).append(fullyEscapedProperty);
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
        String[] tokens = tokenizeURI(dataURI);
        Map<String, Object> dataMap = new HashMap<String, Object>(
                tokens.length, 1.0f);
        String pluginName = tokens[0];
        dataMap.put(PLUGIN_NAME_KEY, pluginName);

        DataURIFieldAccessCache cache = getAccessCache(pluginName);
        DataURIFieldAccess[] access = cache.getDataURIFields();
        for (int i = 0; i < access.length; i += 1) {
            // Offset tokens by 1 as [0] is plugin name
            dataMap.put(access[i].getFieldName(),
                    access[i].getFieldValue(tokens[i + 1]));
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
     * Create a dataURIMap from any object with dataURI annotations.
     * 
     * @param object
     * @return
     * @throws PluginException
     */
    public static Map<String, Object> createDataURIMap(Object object)
            throws PluginException {
        DataURIFieldAccessCache cache = getAccessCache(object.getClass());
        DataURIFieldAccess[] accessArray = cache.getDataURIFields();
        Map<String, Object> dataMap = new HashMap<String, Object>(
                accessArray.length, 1.0f);
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
        String[] tokens = tokenizeURI(dataURI);
        Class<PluginDataObject> clazz = getPluginRecordClass(tokens[0]);
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
     * Populate an existing PluginDataObjects with the DataURI fields from the
     * dataMap.
     * 
     * @param pdo
     * @param dataMap
     * @throws PluginException
     */
    public static void populatePluginDataObject(PluginDataObject pdo,
            Map<String, Object> dataMap) throws PluginException {
        populateObject(pdo, dataMap);
    }

    /*
     * Populate a PDO with the tokens from a parsed DataURI
     */
    private static void populatePluginDataObject(PluginDataObject pdo,
            String[] uriTokens) throws PluginException {
        DataURIFieldAccessCache cache = getAccessCache(pdo.getClass());
        DataURIFieldAccess[] access = cache.getDataURIFields();
        for (int i = 0; i < access.length; i += 1) {
            access[i].setFieldValue(pdo,
                    access[i].getFieldValue(uriTokens[i + 1]));
        }
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
        DataURIFieldAccessCache cache = getAccessCache(object.getClass());
        for (String dataKey : dataMap.keySet()) {
            if (!PLUGIN_NAME_KEY.equals(dataKey)) {
                Object data = dataMap.get(dataKey);
                DataURIFieldAccess access = cache.getFieldAccess(dataKey, data);
                if (access != null) {

                    access.setFieldValue(object, data);
                }
            }
        }
    }

    /*
     * Split a URI on the seperator and remove empty first element.
     */
    private static String[] tokenizeURI(String dataURI) {
        String[] tokens = DATAURI_SEPARATOR_PATTERN.split(dataURI);
        for (int i = 0; i < tokens.length; ++i) {
            // Replace %2F with '/'
            tokens[i] = DATAURI_SEPARATOR_ENCODED_PATTERN.matcher(tokens[i])
                    .replaceAll(DataURI.SEPARATOR);
            // Convert %25 to %
            tokens[i] = DATAURI_SEPARATOR_CHAR_ENCODED_PATTERN.matcher(
                    tokens[i]).replaceAll(DATAURI_SEPARATOR_ESCAPE_CHAR);
        }
        // Removes empty string in [0] due to starting with '/'
        return Arrays.copyOfRange(tokens, 1, tokens.length);
    }

    private static DataURIFieldAccessCache getAccessCache(String pluginName)
            throws PluginException {
        return getAccessCache(getPluginRecordClass(pluginName));
    }

    private static DataURIFieldAccessCache getAccessCache(Class<?> clazz)
            throws PluginException {
        if (clazz == null) {
            throw new PluginException(
                    "Cannot retrieve field access for null class");
        }
        synchronized (clazz) {
            DataURIFieldAccessCache cache = uriFieldMap.get(clazz);
            if (cache == null) {
                cache = new DataURIFieldAccessCache(clazz);
                uriFieldMap.put(clazz, cache);
            }
            return cache;
        }
    }

    public static Class<PluginDataObject> getPluginRecordClass(String pluginName)
            throws PluginException {
        if (classMapper == null) {
            throw new PluginException(
                    "No PluginClassMapper is registered with DataURIUtil.");
        }
        return classMapper.getPluginRecordClass(pluginName);
    }

    private static class DataURIFieldAccessCache {

        /*
         * Compares two fields with the DataURI annotations based off the
         * position.
         */
        private static final Comparator<Field> dataURIAnnotationComparator = new Comparator<Field>() {

            @Override
            public int compare(Field f1, Field f2) {
                int i1 = f1.getAnnotation(DataURI.class).position();
                int i2 = f2.getAnnotation(DataURI.class).position();
                return (i1 < i2 ? -1 : (i1 == i2 ? 0 : 1));
            }

        };

        private final DataURIFieldAccess[] dataURIFields;

        private Map<String, DataURIFieldAccess> fieldMap;

        public DataURIFieldAccessCache(Class<?> type) throws PluginException {
            this.fieldMap = new HashMap<String, DataURIFieldAccess>();
            this.dataURIFields = getDataURIAccessFields(type);
            for (DataURIFieldAccess access : dataURIFields) {
                fieldMap.put(access.getFieldName(), access);
            }
        }

        public DataURIFieldAccess[] getDataURIFields() {
            return dataURIFields;
        }

        public DataURIFieldAccess getFieldAccess(String fieldName, Object object) {
            DataURIFieldAccess access = fieldMap.get(fieldName);
            if (access == null && object != null) {
                synchronized (this) {
                    Map<String, DataURIFieldAccess> newFieldMap = new HashMap<String, DataURIFieldAccess>(
                            fieldMap);

                    access = new DataURIFieldAccess(
                            Arrays.asList(FIELD_SEPARATOR_PATTERN
                                    .split(fieldName)), object.getClass(), null);
                    newFieldMap.put(fieldName, access);
                    fieldMap = newFieldMap;
                }
            }
            return access;
        }

        /**
         * @param clazz
         * @return
         * @throws PluginException
         */
        private static DataURIFieldAccess[] getDataURIAccessFields(
                Class<?> clazz) throws PluginException {
            return getAccess(clazz, Collections.<String> emptyList()).toArray(
                    new DataURIFieldAccess[0]);
        }

        private static List<DataURIFieldAccess> getAccess(Class<?> clazz,
                List<String> parents) throws PluginException {
            List<Field> fields = getOrderedDataURIFields(clazz);
            List<DataURIFieldAccess> accessors = new ArrayList<DataURIFieldAccess>();
            for (Field field : fields) {
                List<String> names = new ArrayList<String>(parents);
                names.add(field.getName());
                Class<?> type = field.getType();
                DataURI dataURI = field.getAnnotation(DataURI.class);
                if (dataURI.embedded()) {
                    accessors.addAll(getAccess(type, names));
                } else {
                    DataURIFieldConverter converter = null;
                    if (dataURI.converter() != DataURI.NO_CONVERTER) {
                        try {
                            converter = dataURI.converter().newInstance();
                        } catch (Exception e) {
                            throw new PluginException(
                                    "Error creating field convert: "
                                            + dataURI.converter(), e);
                        }
                    }
                    accessors
                            .add(new DataURIFieldAccess(names, type, converter));
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

        /** Class of the oibject that is in the dataURI. */
        private final Class<?> fieldClass;

        /** URI field converter */
        private DataURIFieldConverter fieldConverter;

        public DataURIFieldAccess(List<String> fieldNames, Class<?> fieldClass,
                DataURIFieldConverter fieldConverter) {
            this.fieldNames = fieldNames.toArray(new String[0]);
            StringBuilder fieldName = new StringBuilder(this.fieldNames[0]);
            for (int i = 1; i < this.fieldNames.length; i += 1) {
                fieldName.append(FIELD_SEPARATOR).append(this.fieldNames[i]);
            }
            this.fieldName = fieldName.toString();
            this.fieldClass = fieldClass;
            this.fieldConverter = fieldConverter;
        }

        /**
         * Returns the fully qualified name of the field
         * 
         * @return
         */
        public String getFieldName() {
            return fieldName;
        }

        /**
         * Extract the field value designated by this field access object from
         * the field container Object and converts it to a URI string
         * 
         * @param fieldContainer
         * @return
         * @throws PluginException
         */
        public String getFieldString(Object fieldContainer)
                throws PluginException {
            return toFieldString(getFieldValue(fieldContainer));
        }

        /**
         * Converts the field value represented by this field access to a uri
         * string
         * 
         * @param fieldValue
         * @return
         */
        public String toFieldString(Object fieldValue) {
            if (fieldConverter != null) {
                fieldValue = fieldConverter.toString(fieldValue);
            } else if (fieldValue instanceof Calendar) {
                fieldValue = TimeUtil.formatCalendar((Calendar) fieldValue);
            }
            return String.valueOf(fieldValue);
        }

        /**
         * Extract the field value designated by this field access object from
         * the field container Object
         * 
         * @param fieldContainer
         * @return
         * @throws PluginException
         */
        public Object getFieldValue(Object fieldContainer)
                throws PluginException {
            try {
                Object object = fieldContainer;
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
            if (fieldConverter != null) {
                return fieldConverter.fromString(stringValue);
            }
            return ConvertUtil.convertObject(stringValue, fieldClass);
        }

        /**
         * Set the fieldValue Object into the fieldContainer Object.
         * 
         * @param fieldContainer
         * @param fieldValue
         * @throws PluginException
         */
        public void setFieldValue(Object fieldContainer, Object fieldValue)
                throws PluginException {
            Object source = fieldContainer;
            try {
                for (int i = 0; i < (fieldNames.length - 1); i += 1) {
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
