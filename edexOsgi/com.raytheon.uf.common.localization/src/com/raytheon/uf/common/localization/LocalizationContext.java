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

package com.raytheon.uf.common.localization;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;

/**
 * Defines structures and methods for handling localization contexts. A
 * localization context is defined according to the following structure:
 * <p>
 * <b>type.level.name.function</b>
 * </p>
 * <ul>
 * <li><b>type</b> - required.
 * <li><b>level</b> - required.
 * <li><b>name</b> - optional. The name is often either the username or site ID
 * and appears for a localization level of site or user.
 * <li><b>function</b> - optional. The function further defines the utility
 * context of a localization type and is usually defined for the
 * edex_static.base context.
 * </ul>
 * 
 * <p>
 * Methods for interfacing between a value object and string representation of
 * contexts are provided.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack    Initial Creation.
 * Jul 14, 2008 1250       jelkins     EDEX LocalizationAdapter additions.
 * Oct 01, 2013 2361       njensen     Removed XML annotations and methods
 * Feb 06, 2014 2761       mnash       Add region localization level
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class LocalizationContext implements Cloneable {

    /**
     * NOTE: When making changes to LocalizationLevelSerializationAdapter, you
     * must ensure that these changes are ported to
     * /pythonPackages/dynamicserialize/adapters/
     * LocalizationLevelSerializationAdapter.py
     * 
     */
    public static class LocalizationLevelSerializationAdapter implements
            ISerializationTypeAdapter<LocalizationLevel> {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
         * (com.raytheon.uf.common.serialization.ISerializationContext,
         * java.lang.Object)
         */
        @Override
        public void serialize(ISerializationContext serializer,
                LocalizationLevel object) throws SerializationException {
            serializer.writeString(object.text);
            serializer.writeI32(object.order);
            serializer.writeBool(object.systemLevel);
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.common.serialization.ISerializationTypeAdapter#
         * deserialize
         * (com.raytheon.uf.common.serialization.IDeserializationContext)
         */
        @Override
        public LocalizationLevel deserialize(
                IDeserializationContext deserializer)
                throws SerializationException {
            LocalizationLevel level = LocalizationLevel.createLevel(
                    deserializer.readString(), deserializer.readI32(),
                    deserializer.readBool());
            return level;
        }

    }

    /**
     * NOTE: When making changes to LocalizationTypeSerializationAdapter, you
     * must ensure that these changes are ported to
     * /pythonPackages/dynamicserialize/adapters/
     * LocalizationTypeSerializationAdapter.py
     * 
     */
    public static class LocalizationTypeSerializationAdapter implements
            ISerializationTypeAdapter<LocalizationType> {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
         * (com.raytheon.uf.common.serialization.ISerializationContext,
         * java.lang.Object)
         */
        @Override
        public void serialize(ISerializationContext serializer,
                LocalizationType object) throws SerializationException {
            serializer.writeString(object.text);
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.common.serialization.ISerializationTypeAdapter#
         * deserialize
         * (com.raytheon.uf.common.serialization.IDeserializationContext)
         */
        @Override
        public LocalizationType deserialize(IDeserializationContext deserializer)
                throws SerializationException {
            return LocalizationType.valueOf(deserializer.readString());
        }

    }

    private static final char CONTEXT_SEPARATOR = '.';

    private static final char PATH_SEPARATOR = IPathManager.SEPARATOR.charAt(0);

    /**
     * NOTE: When making changes to LocalizationType, you must ensure that these
     * changes are ported to
     * /pythonPackages/dynamicserialize/dstypes/com/raytheon
     * /uf/common/localization/LocalizationType.py.
     * 
     */
    @DynamicSerialize
    @DynamicSerializeTypeAdapter(factory = LocalizationTypeSerializationAdapter.class)
    public static class LocalizationType {

        private static Map<String, LocalizationType> typeMap = new HashMap<String, LocalizationType>();

        // STANDARD TYPES, OTHERS SHOULD BE ADDED IN CUSTOM CODE
        public static LocalizationType UNKNOWN = valueOf("UNKNOWN");

        public static LocalizationType CAVE_STATIC = valueOf("CAVE_STATIC");

        public static LocalizationType CAVE_CONFIG = valueOf("CAVE_CONFIG");

        public static LocalizationType COMMON_STATIC = valueOf("COMMON_STATIC");

        public static LocalizationType EDEX_STATIC = valueOf("EDEX_STATIC");

        public static LocalizationType[] values() {
            return typeMap.values().toArray(
                    new LocalizationType[typeMap.size()]);
        }

        public static LocalizationType valueOf(String text) {
            text = String.valueOf(text).toUpperCase();
            LocalizationType type = typeMap.get(text);
            if (type == null) {
                type = new LocalizationType(text);
                typeMap.put(text, type);
            }
            return type;
        }

        private String text;

        private LocalizationType(String text) {
            this.text = text;
        }

        @Override
        public String toString() {
            return String.valueOf(text);
        }

        public String name() {
            return toString();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((text == null) ? 0 : text.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            LocalizationType other = (LocalizationType) obj;
            if (text == null) {
                if (other.text != null) {
                    return false;
                }
            } else if (!text.equals(other.text)) {
                return false;
            }
            return true;
        }

    }

    /**
     * NOTE: When making changes to LocalizationLevel, you must ensure that
     * these changes are ported to
     * /pythonPackages/dynamicserialize/dstypes/com/raytheon
     * /uf/common/localization/LocalizationLevel.py.
     * 
     */
    @DynamicSerialize
    @DynamicSerializeTypeAdapter(factory = LocalizationLevelSerializationAdapter.class)
    public static class LocalizationLevel implements
            Comparable<LocalizationLevel> {

        public static Comparator<LocalizationLevel> REVERSE_COMPARATOR = new Comparator<LocalizationLevel>() {
            @Override
            public int compare(LocalizationLevel o1, LocalizationLevel o2) {
                return o2.compareTo(o1);
            }
        };

        private static Map<String, LocalizationLevel> typeMap = new HashMap<String, LocalizationLevel>();

        public static LocalizationLevel UNKNOWN = new LocalizationLevel(
                "UNKNOWN");

        public static LocalizationLevel BASE = createLevel("BASE", 0, true);

        public static LocalizationLevel REGION = createLevel("REGION", 150);

        public static LocalizationLevel CONFIGURED = createLevel("CONFIGURED",
                250, true);

        public static LocalizationLevel SITE = createLevel("SITE", 500);

        public static LocalizationLevel WORKSTATION = createLevel(
                "WORKSTATION", 750);

        public static LocalizationLevel USER = createLevel("USER", 1000);

        static {
            UNKNOWN.order = -1;
        }

        /**
         * Returns a sorted array of the available LocalizationLevels. In most
         * cases, IPathManager.getAvailableLevels() should be used instead
         * 
         * @return the LocalizationLevels available
         */
        public static LocalizationLevel[] values() {
            LocalizationLevel[] levels = typeMap.values().toArray(
                    new LocalizationLevel[typeMap.size()]);
            Arrays.sort(levels);
            return levels;
        }

        public static LocalizationLevel valueOf(String text) {
            text = String.valueOf(text).toUpperCase();
            LocalizationLevel type = typeMap.get(text);
            if (type == null) {
                type = new LocalizationLevel(text);
                typeMap.put(text, type);
            }
            return type;
        }

        /**
         * Attempts to create a new localization level at the specified order,
         * new level is not system level
         * 
         * @param text
         * @param order
         * @return
         */
        public static LocalizationLevel createLevel(String text, int order) {
            return createLevel(text, order, false);
        }

        /**
         * Attempts to create a new localization level at the specified order
         * 
         * @param text
         * @param order
         * @param systemLevel
         * @return
         */
        public static LocalizationLevel createLevel(String text, int order,
                boolean systemLevel) {
            text = String.valueOf(text).toUpperCase();
            LocalizationLevel level = typeMap.get(text);
            // Only create level and set order if not already created
            if (level == null) {
                level = new LocalizationLevel(text);
                level.systemLevel = systemLevel;
                level.order = order;
                typeMap.put(text, level);
            }
            return level;
        }

        private String text;

        private boolean systemLevel = false;

        /** Default order for new levels is in between site and user */
        private int order = 750;

        private LocalizationLevel(String text) {
            this.text = text;
        }

        public boolean isSystemLevel() {
            return systemLevel;
        }

        @Override
        public String toString() {
            return String.valueOf(text);
        }

        public String name() {
            return toString();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((text == null) ? 0 : text.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            LocalizationLevel other = (LocalizationLevel) obj;
            if (text == null) {
                if (other.text != null) {
                    return false;
                }
            } else if (!text.equals(other.text)) {
                return false;
            }
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(LocalizationLevel lvl) {
            return (order - lvl.order);
        }

    }

    @DynamicSerializeElement
    private LocalizationType localizationType;

    @DynamicSerializeElement
    private LocalizationLevel localizationLevel;

    @DynamicSerializeElement
    private String contextName;

    /**
     * Creates a string representation of a given localization context.
     * 
     * @param lc
     *            the localization context to serialize
     * @return a localization context.
     */
    public static String serialize(LocalizationContext lc) {
        return lc.toString();
    }

    /**
     * Creates a localization context from a serialized localization context.
     * 
     * @param s
     *            the string representation of a localization context
     * @return the localization context represented by the given string
     * 
     * @see #serialize(LocalizationContext)
     * @see #LocalizationContext(String)
     */
    public static LocalizationContext deserialize(String s) {
        return new LocalizationContext(s);
    }

    /**
     * <b>NOT RECOMMENDED FOR GENERAL USE</b><br>
     * Please use
     * {@link IPathManager#getContext(LocalizationType, LocalizationLevel)} </p>
     * Constructs the default localization context.
     * <p>
     * Constructs a context with <code>UNKNOWN</code> localization and level.
     */
    public LocalizationContext() {
        this(LocalizationType.UNKNOWN, LocalizationLevel.UNKNOWN, null);
    }

    /**
     * <b>NOT RECOMMENDED FOR GENERAL USE</b><br>
     * Please use
     * {@link IPathManager#getContext(LocalizationType, LocalizationLevel)} </p>
     * Constructs the localization context with a given type and level.
     * 
     * @param type
     *            the localization type
     * @param level
     *            the localization level
     */
    public LocalizationContext(LocalizationType type, LocalizationLevel level) {
        this(type, level, null);
    }

    /**
     * <b>NOT RECOMMENDED FOR GENERAL USE</b><br>
     * Please use
     * {@link IPathManager#getContext(LocalizationType, LocalizationLevel)} </p>
     * Constructs the localization context with a given type, level, and name.
     * 
     * @param type
     *            the localization type
     * @param level
     *            the localization level
     * @param contextName
     *            the localization name
     *            <p>
     *            See {@link #setContextName(String)}
     *            </p>
     */
    public LocalizationContext(LocalizationType type, LocalizationLevel level,
            String contextName) {
        Validate.notNull(type, "type must not be null");
        Validate.notNull(level, "level must not be null");
        if (level != LocalizationLevel.BASE
                && level != LocalizationLevel.UNKNOWN) {
            Validate.notEmpty(contextName, "contextName for " + level
                    + " level must not be null or empty.");
        }

        this.localizationLevel = level;
        this.localizationType = type;
        this.contextName = contextName;
    }

    /**
     * Constructs the localization context with a context string as an
     * initializer.
     * 
     * @param contextString
     *            the context string
     *            <p>
     *            The string must be formatted similarly to the Strings returned
     *            from the {@link #toString()} or
     *            {@link #serialize(LocalizationContext)} methods.
     *            </p>
     * 
     * @throws IllegalArgumentException
     */
    public LocalizationContext(String contextString)
            throws IllegalArgumentException {

        String[] components = contextString.split(Pattern.quote(""
                + CONTEXT_SEPARATOR));
        if (components.length < 2) {
            throw new IllegalArgumentException("Unable to parse: "
                    + components.length);
        }

        String localizationTypeStr = components[0];
        try {
            this.localizationType = LocalizationType
                    .valueOf(localizationTypeStr.toUpperCase());
        } catch (RuntimeException e) {
            this.localizationType = LocalizationType.UNKNOWN;
        }

        String levelTypeStr = components[1];
        try {
            this.localizationLevel = LocalizationLevel.valueOf(levelTypeStr
                    .toUpperCase());
        } catch (RuntimeException e) {
            this.localizationLevel = LocalizationLevel.UNKNOWN;
        }

    }

    /**
     * Retrieves the localization type.
     * 
     * @return the localization type
     * @see #setLocalizationType(LocalizationType)
     */
    public LocalizationType getLocalizationType() {
        return localizationType;
    }

    /**
     * Updates the localization type.
     * 
     * @param localizationType
     *            the localization type indicating the localization type of the
     *            context.
     *            <p>
     *            The localization type is the initial or root level of the
     *            localization context. The localization context can be further
     *            defined through the
     *            {@link #setLocalizationLevel(LocalizationLevel)} and
     *            {@link #setContextName(String)} methods.
     *            </p>
     */
    public void setLocalizationType(LocalizationType localizationType) {
        this.localizationType = localizationType;
    }

    /**
     * Retrieves the localization level.
     * 
     * @return the localization level
     */
    public LocalizationLevel getLocalizationLevel() {
        return localizationLevel;
    }

    /**
     * Updates the localization level.
     * <p>
     * The localization level is the second level of defining a context.
     * </p>
     * 
     * @param localizationLevel
     *            the level type indicating the localization level of the
     *            context
     */
    public void setLocalizationLevel(LocalizationLevel localizationLevel) {
        this.localizationLevel = localizationLevel;
    }

    /**
     * Retrieves the context name.
     * 
     * @return the context name
     * @see #setContextName()
     */
    public String getContextName() {
        return contextName;
    }

    /**
     * Updates the context name.
     * 
     * @param contextName
     *            the string indicating the context name
     *            <P>
     *            Depending on the {@link LocalizationLevel}, the name can be
     *            the site ID such as OAX or the username.
     *            </P>
     */
    public void setContextName(String contextName) {
        if (this.localizationLevel != LocalizationLevel.BASE
                && this.localizationLevel != LocalizationLevel.UNKNOWN) {
            Validate.notEmpty(contextName, "contextName for "
                    + this.localizationLevel
                    + " level must not be null or empty.");
        }
        this.contextName = contextName;
    }

    private String convertToDelimitedString(char delimiter) {
        StringBuffer buf = new StringBuffer();
        buf.append(localizationType.toString().toLowerCase());
        buf.append(delimiter);
        buf.append(localizationLevel.toString().toLowerCase());
        if (contextName != null && !contextName.equals("")) {
            buf.append(delimiter);
            buf.append(contextName);
        }

        return buf.toString();
    }

    /**
     * Creates a directory path.
     * <p>
     * Concatenate the fields of this class together to form a directory path. A
     * {@link java.io.File#separatorChar} will be placed between each field.
     * </p>
     * 
     * @return the relative directory path to the localization items
     * 
     * @see #toString()
     */
    public String toPath() {
        return convertToDelimitedString(PATH_SEPARATOR);
    }

    /**
     * Creates a string representation.
     * <p>
     * Concatenate the fields of this class together to form a readable string
     * representation of this class. A '.' will be placed between each field.
     * </p>
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return convertToDelimitedString(CONTEXT_SEPARATOR);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((contextName == null) ? 0 : contextName.hashCode());
        result = prime
                * result
                + ((localizationLevel == null) ? 0 : localizationLevel
                        .hashCode());
        result = prime
                * result
                + ((localizationType == null) ? 0 : localizationType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LocalizationContext other = (LocalizationContext) obj;
        if (contextName == null) {
            if (other.contextName != null) {
                return false;
            }
        } else if (!contextName.equals(other.contextName)) {
            return false;
        }
        if (localizationLevel == null) {
            if (other.localizationLevel != null) {
                return false;
            }
        } else if (!localizationLevel.equals(other.localizationLevel)) {
            return false;
        }
        if (localizationType == null) {
            if (other.localizationType != null) {
                return false;
            }
        } else if (!localizationType.equals(other.localizationType)) {
            return false;
        }
        return true;
    }

    @Override
    public Object clone() {
        LocalizationContext ctx = new LocalizationContext();
        ctx.setContextName(this.contextName);
        ctx.setLocalizationLevel(this.localizationLevel);
        ctx.setLocalizationType(this.localizationType);
        return ctx;
    }

}
