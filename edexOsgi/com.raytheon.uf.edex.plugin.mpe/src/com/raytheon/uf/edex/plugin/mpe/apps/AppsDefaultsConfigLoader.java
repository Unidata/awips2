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
package com.raytheon.uf.edex.plugin.mpe.apps;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Collections;
import java.util.Set;
import java.util.HashSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Utility to automatically populate a POJO containing fields annotated with any
 * one of the following: {@link AppsDefaultsBooleanField},
 * {@link AppsDefaultsIntegerField}, {@link AppsDefaultsCustomField},
 * {@link AppsDefaultsPathField}, {@link AppsDefaultsStringField},
 * {@link AppsDefaultsDoubleField}, {@link AppsDefaultsFloatField} based on
 * key/value pairs defined within and retrieved from Apps_defaults.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2016 5631       bkowal      Initial creation
 * Aug 31, 2016 5631       bkowal      Implemented caching of annotated configurable
 *                                     fields.
 * Sep 27, 2016 5631       bkowal      Updated to support non-required {@link Path}s.
 * Oct 18, 2016 5631       bkowal      Added support for {@link AppsDefaultsDoubleField}.
 * Oct 26, 2016 5631       bkowal      Added support for {@link AppsDefaultsFloatField}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class AppsDefaultsConfigLoader {

    private static final Logger logger = LoggerFactory
            .getLogger(AppsDefaultsConfigLoader.class);

    private static final ConcurrentMap<Class<?>, Set<AppsDefaultsConfigField>> appsDefaultsConfigFieldsCache = new ConcurrentHashMap<>();

    protected AppsDefaultsConfigLoader() {
    }

    /**
     * Populates the specified {@link Object} instance using key/values read
     * from Apps_defaults.
     * 
     * @param toPopulate
     *            the specified {@link Object} instance
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    public static void populateFromAppsDefaults(final Object toPopulate)
            throws RequiredTokenMissingException, AppsDefaultsLoadException {
        Class<?> clazz = toPopulate.getClass();
        Set<AppsDefaultsConfigField> cachedFields = loadFromCache(clazz);
        if (cachedFields.isEmpty()) {
            logger.warn(
                    "Class {} does not include any Apps Defaults configurable fields.",
                    toPopulate.getClass().getName());
            return;
        }
        for (AppsDefaultsConfigField configField : cachedFields) {
            final Annotation annotation = configField.getAnnotation();
            final Field field = configField.getField();
            if (annotation instanceof AppsDefaultsStringField) {
                handleStringField(toPopulate, field);
            } else if (annotation instanceof AppsDefaultsBooleanField) {
                handleBooleanField(toPopulate, field);
            } else if (annotation instanceof AppsDefaultsIntegerField) {
                handleIntegerField(toPopulate, field);
            } else if (annotation instanceof AppsDefaultsCustomField) {
                handleCustomField(toPopulate, field);
            } else if (annotation instanceof AppsDefaultsPathField) {
                handlePathField(toPopulate, field);
            } else if (annotation instanceof AppsDefaultsDoubleField) {
                handleDoubleField(toPopulate, field);
            } else if (annotation instanceof AppsDefaultsFloatField) {
                handleFloatField(toPopulate, field);
            }
        }
    }

    private static Set<AppsDefaultsConfigField> loadFromCache(Class<?> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException(
                    "Required argument 'clazz' cannot be NULL.");
        }
        Set<AppsDefaultsConfigField> cachedFields = appsDefaultsConfigFieldsCache
                .get(clazz);
        if (cachedFields == null) {
            /*
             * Determine which fields have an Apps Defaults configurable
             * annotation.
             */
            Field[] fields = clazz.getDeclaredFields();
            if (fields.length == 0) {
                cachedFields = Collections.emptySet();
            } else {
                cachedFields = new HashSet<>();
                for (Field field : fields) {
                    AppsDefaultsConfigField configField = determineAppsDefaultsConfigurable(
                            field);
                    if (configField == null) {
                        continue;
                    }
                    cachedFields.add(configField);
                }
            }
            appsDefaultsConfigFieldsCache.put(clazz, cachedFields);
        }

        return cachedFields;
    }

    private static AppsDefaultsConfigField determineAppsDefaultsConfigurable(
            final Field field) {
        Annotation[] annotations = field.getAnnotations();
        if (annotations.length == 0) {
            // no annotations on this field.
            return null;
        }

        for (Annotation annotation : annotations) {
            if (annotation instanceof AppsDefaultsStringField
                    || annotation instanceof AppsDefaultsBooleanField
                    || annotation instanceof AppsDefaultsIntegerField
                    || annotation instanceof AppsDefaultsCustomField
                    || annotation instanceof AppsDefaultsPathField
                    || annotation instanceof AppsDefaultsDoubleField
                    || annotation instanceof AppsDefaultsFloatField) {
                return new AppsDefaultsConfigField(field, annotation);
            }
        }
        return null;
    }

    /**
     * Populates the specified {@link Field} for the specified {@link Object}
     * based on a boolean property read from Apps_defaults.
     * 
     * @param toPopulate
     *            the specified {@link Object}
     * @param field
     *            the specified {@link Field}
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    private static void handleBooleanField(final Object toPopulate,
            final Field field) throws RequiredTokenMissingException,
                    AppsDefaultsLoadException {
        AppsDefaultsBooleanField annotation = field
                .getAnnotation(AppsDefaultsBooleanField.class);
        final String token = annotation.property();
        Boolean value = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean(token);
        if (value == null) {
            /*
             * Determine if the field is required.
             */
            if (annotation.required()) {
                /*
                 * fail.
                 */
                throw new RequiredTokenMissingException(token);
            } else {
                /*
                 * Use the default value.
                 */
                value = annotation.defaultValue();
            }
        }
        try {
            boolean changeAccess = false;
            if (!field.isAccessible()) {
                field.setAccessible(true);
                changeAccess = true;
            }
            field.setBoolean(toPopulate, value);
            if (changeAccess) {
                field.setAccessible(false);
            }
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw generateLoadException(toPopulate, field, value, e);
        }
    }

    /**
     * Populates the specified {@link Field} for the specified {@link Object}
     * based on a {@link String} property read from Apps_defaults.
     * 
     * @param toPopulate
     *            the specified {@link Object}
     * @param field
     *            the specified {@link Field}
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    private static void handleStringField(final Object toPopulate,
            final Field field) throws RequiredTokenMissingException,
                    AppsDefaultsLoadException {
        AppsDefaultsStringField annotation = field
                .getAnnotation(AppsDefaultsStringField.class);
        final String token = annotation.property();
        String value = AppsDefaults.getInstance().getToken(token, null);
        if (value == null) {
            /*
             * Determine if the field is required.
             */
            if (annotation.required()) {
                throw new RequiredTokenMissingException(token);
            } else {
                /*
                 * should the default, when missing, be null?
                 */
                if (!annotation.nullDefaultValue()) {
                    value = annotation.defaultValue();
                }
            }
        }
        try {
            boolean changeAccess = false;
            if (!field.isAccessible()) {
                field.setAccessible(true);
                changeAccess = true;
            }
            field.set(toPopulate, value);
            if (changeAccess) {
                field.setAccessible(false);
            }
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw generateLoadException(toPopulate, field, value, e);
        }
    }

    /**
     * Populates the specified {@link Field} for the specified {@link Object}
     * based on an {@link Integer} property read from Apps_defaults.
     * 
     * @param toPopulate
     *            the specified {@link Object}
     * @param field
     *            the specified {@link Field}
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    private static void handleIntegerField(final Object toPopulate,
            final Field field) throws RequiredTokenMissingException,
                    AppsDefaultsLoadException {
        AppsDefaultsIntegerField annotation = field
                .getAnnotation(AppsDefaultsIntegerField.class);
        final String token = annotation.property();
        int value = Integer.MAX_VALUE;
        /*
         * Handling the case when no value is mapped to the token. But, it is
         * required.
         */
        if (AppsDefaults.getInstance().getToken(token, null) == null) {
            /*
             * No value has been defined for the token.
             */
            if (annotation.required()) {
                throw new RequiredTokenMissingException(token);
            } else {
                value = annotation.defaultValue();
            }
        } else {
            /*
             * A value has been defined for the token.
             */
            value = AppsDefaults.getInstance().getInt(token, 0);
        }
        try {
            boolean changeAccess = false;
            if (!field.isAccessible()) {
                field.setAccessible(true);
                changeAccess = true;
            }
            field.setInt(toPopulate, value);
            if (changeAccess) {
                field.setAccessible(false);
            }
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw generateLoadException(toPopulate, field, value, e);
        }
    }

    /**
     * Populates the specified {@link Field} for the specified {@link Object}
     * based on an {@link String} property read from Apps_defaults that will be
     * converted to a custom {@link Object} using a
     * {@link ICustomValueConverter}.
     * 
     * @param toPopulate
     *            the specified {@link Object}
     * @param field
     *            the specified {@link Field}
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    private static void handleCustomField(final Object toPopulate,
            final Field field) throws RequiredTokenMissingException,
                    AppsDefaultsLoadException {
        AppsDefaultsCustomField annotation = field
                .getAnnotation(AppsDefaultsCustomField.class);
        final String token = annotation.property();
        String value = AppsDefaults.getInstance().getToken(token, null);
        if (value == null) {
            if (annotation.required()) {
                throw new RequiredTokenMissingException(token);
            } else {
                value = annotation.defaultValue();
            }
        }

        /*
         * At this point, either a value has been set from Apps_defaults or a
         * default value is in use.
         */
        ICustomValueConverter<?> converter = null;
        try {
            converter = annotation.converter().newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new AppsDefaultsLoadException(
                    "Failed to instantiate Apps Defaults value converter: "
                            + annotation.converter().getName()
                            + " associated with field: " + field.getName()
                            + ".",
                    e);
        }

        Object convertedValue;
        try {
            convertedValue = converter.convertValue(value);
        } catch (ValueConverterException e) {
            throw new AppsDefaultsLoadException(
                    "Apps Defaults value converter: "
                            + annotation.converter().getName()
                            + " has failed to convert value: " + value + ".",
                    e);
        }
        /*
         * What if the property that was read cannot successfully be converted
         * to a custom value type?
         */
        if (convertedValue == null) {
            if (annotation.required()) {
                throw new RequiredTokenMissingException(token);
            } else {
                logger.warn("Apps Defaults value: " + value
                        + " associated with property: " + token
                        + " could not successfully be converted to a custom value type utilizing: "
                        + annotation.converter().getName() + ".");
            }
        }
        try {
            boolean changeAccess = false;
            if (!field.isAccessible()) {
                field.setAccessible(true);
                changeAccess = true;
            }
            field.set(toPopulate, convertedValue);
            if (changeAccess) {
                field.setAccessible(false);
            }
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw generateLoadException(toPopulate, field, value, e);
        }
    }

    /**
     * Populates the specified {@link Field} for the specified {@link Object}
     * based on an {@link String} property read from Apps_defaults that will be
     * converted to a custom {@link Object} using a
     * {@link ICustomValueConverter}.
     * 
     * @param toPopulate
     *            the specified {@link Object}
     * @param field
     *            the specified {@link Field}
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    private static void handlePathField(final Object toPopulate,
            final Field field) throws RequiredTokenMissingException,
                    AppsDefaultsLoadException {
        AppsDefaultsPathField annotation = field
                .getAnnotation(AppsDefaultsPathField.class);
        final String token = annotation.property();
        String value = AppsDefaults.getInstance().getToken(token, null);
        if (value == null) {
            if (annotation.required()) {
                throw new RequiredTokenMissingException(token);
            } else {
                /*
                 * Leave property at null.
                 */
                return;
            }
        }

        final Path path = Paths.get(value);
        /*
         * For now just readonly resources. So, no existence check will be done
         * because if the {@link Path} does not exist, it just will not be
         * possible to read from it.
         */
        try {
            boolean changeAccess = false;
            if (!field.isAccessible()) {
                field.setAccessible(true);
                changeAccess = true;
            }
            field.set(toPopulate, path);
            if (changeAccess) {
                field.setAccessible(false);
            }
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw generateLoadException(toPopulate, field, value, e);
        }
    }

    /**
     * Populates the specified {@link Field} for the specified {@link Object}
     * based on an {@link Double} property read from Apps_defaults.
     * 
     * @param toPopulate
     *            the specified {@link Object}
     * @param field
     *            the specified {@link Field}
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    private static void handleDoubleField(final Object toPopulate,
            final Field field) throws RequiredTokenMissingException,
                    AppsDefaultsLoadException {
        AppsDefaultsDoubleField annotation = field
                .getAnnotation(AppsDefaultsDoubleField.class);
        final String token = annotation.property();
        double value = Double.MAX_VALUE;
        /*
         * Handling the case when no value is mapped to the token. But, it is
         * required.
         */
        if (AppsDefaults.getInstance().getToken(token, null) == null) {
            /*
             * No value has been defined for the token.
             */
            if (annotation.required()) {
                throw new RequiredTokenMissingException(token);
            } else {
                value = annotation.defaultValue();
            }
        } else {
            value = AppsDefaults.getInstance().getDouble(token, 0);
        }
        try {
            boolean changeAccess = false;
            if (!field.isAccessible()) {
                field.setAccessible(true);
                changeAccess = true;
            }
            field.setDouble(toPopulate, value);
            if (changeAccess) {
                field.setAccessible(false);
            }
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw generateLoadException(toPopulate, field, value, e);
        }
    }

    /**
     * Populates the specified {@link Field} for the specified {@link Object}
     * based on an {@link Float} property read from Apps_defaults.
     * 
     * @param toPopulate
     *            the specified {@link Object}
     * @param field
     *            the specified {@link Field}
     * @throws RequiredTokenMissingException
     * @throws AppsDefaultsLoadException
     */
    private static void handleFloatField(final Object toPopulate,
            final Field field) throws RequiredTokenMissingException,
                    AppsDefaultsLoadException {
        AppsDefaultsFloatField annotation = field
                .getAnnotation(AppsDefaultsFloatField.class);
        final String token = annotation.property();
        float value = Float.MAX_VALUE;
        /*
         * Handling the case when no value is mapped to the token. But, it is
         * required.
         */
        if (AppsDefaults.getInstance().getToken(token, null) == null) {
            /*
             * No value has been defined for the token.
             */
            if (annotation.required()) {
                throw new RequiredTokenMissingException(token);
            } else {
                value = annotation.defaultValue();
            }
        } else {
            /*
             * A value has been defined for the token.
             */
            value = AppsDefaults.getInstance().getFloat(token, 0);
        }
        try {
            boolean changeAccess = false;
            if (!field.isAccessible()) {
                field.setAccessible(true);
                changeAccess = true;
            }
            field.setFloat(toPopulate, value);
            if (changeAccess) {
                field.setAccessible(false);
            }
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw generateLoadException(toPopulate, field, value, e);
        }
    }

    /**
     * Generates a {@link AppsDefaultsLoadException} identifying the
     * {@link Object} in which the specified {@link Field} could not be set to
     * the specified value.
     * 
     * @param toPopulate
     *            the {@link Object} that the field could not be set within
     * @param field
     *            the specified {@link Field}
     * @param value
     *            the specified value
     * @param e
     *            {@link Throwable} with additional details about the failure
     * @return
     */
    private static AppsDefaultsLoadException generateLoadException(
            final Object toPopulate, final Field field, final Object value,
            Throwable e) {
        StringBuilder sb = new StringBuilder("Failed to set field: ")
                .append(field.getName());
        sb.append(" for ").append(toPopulate.getClass().getName());
        sb.append(" to '").append(value).append("'.");

        return new AppsDefaultsLoadException(sb.toString(), e);
    }
}