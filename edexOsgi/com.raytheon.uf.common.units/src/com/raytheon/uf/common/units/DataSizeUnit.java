package com.raytheon.uf.common.units;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * Enumeration for Data Size units and conversions. Based off of TimeUnit class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2013 1420       mpduff       Initial creation.
 * Jan 14, 2013 1286       djohnson     Add lookup map via string version.
 * Jan 17, 2013 1357       mpduff       Moved to com.raytheon.uf.common.units so other plugins can use it.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlEnum
public enum DataSizeUnit {
    @XmlEnumValue("Byte")
    BYTE("Byte") {
        @Override
        public long toByte(long l) {
            return l;
        }

        @Override
        public long toKB(long l) {
            return l / CONV;
        }

        @Override
        public long toMB(long l) {
            return l / (CONV * CONV);
        }

        @Override
        public long toGB(long l) {
            return l / (CONV * CONV * CONV);
        }

        @Override
        public long convert(long l, DataSizeUnit ds) {
            return ds.toByte(l);
        }
    },
    @XmlEnumValue("KB")
    KB("KB") {
        @Override
        public long toByte(long l) {
            return l * CONV;
        }

        @Override
        public long toKB(long l) {
            return l;
        }

        @Override
        public long toMB(long l) {
            return l / CONV;
        }

        @Override
        public long toGB(long l) {
            return l / (CONV * CONV);
        }

        @Override
        public long convert(long l, DataSizeUnit ds) {
            return ds.toKB(l);
        }
    },
    @XmlEnumValue("MB")
    MB("MB") {
        @Override
        public long toByte(long l) {
            return l * (CONV * CONV);
        }

        @Override
        public long toKB(long l) {
            return l * (CONV);
        }

        @Override
        public long toMB(long l) {
            return l;
        }

        @Override
        public long toGB(long l) {
            return l / CONV;
        }

        @Override
        public long convert(long l, DataSizeUnit ds) {
            return ds.toMB(l);
        }
    },
    @XmlEnumValue("GB")
    GB("GB") {
        @Override
        public long toByte(long l) {
            return l * (CONV * CONV * CONV);
        }

        @Override
        public long toKB(long l) {
            return l * (CONV * CONV);
        }

        @Override
        public long toMB(long l) {
            return l * (CONV);
        }

        @Override
        public long toGB(long l) {
            return l;
        }

        @Override
        public long convert(long l, DataSizeUnit ds) {
            return ds.toGB(l);
        }
    };

    private static final long CONV = 1024;

    /** String unit */
    private String unit;

    private DataSizeUnit(String unit) {
        this.unit = unit;
    }

    /**
     * Get the unit's string representation.
     * 
     * @return unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * Convert to Bytes.
     * 
     * @param l
     *            value to convert
     * 
     * @return converted value
     */
    public abstract long toByte(long l);

    /**
     * Convert to KB.
     * 
     * @param l
     *            value to convert
     * 
     * @return converted value
     */
    public abstract long toKB(long l);

    /**
     * Convert to MB.
     * 
     * @param l
     *            value to convert
     * 
     * @return converted value
     */
    public abstract long toMB(long l);

    /**
     * Convert to GB.
     * 
     * @param l
     *            value to convert
     * 
     * @return converted value
     */
    public abstract long toGB(long l);

    /**
     * Convert a data size value.
     * 
     * @param l
     *            value to convert
     * @param ds
     *            DataSizeUtil unit
     * 
     * @return converted value
     */
    public abstract long convert(long l, DataSizeUnit ds);

    private static final Map<String, DataSizeUnit> LOOKUP_MAP;
    static {
        Map<String, DataSizeUnit> map = new HashMap<String, DataSizeUnit>();
        for (DataSizeUnit unit : DataSizeUnit.values()) {
            map.put(unit.getUnit(), unit);
        }
        LOOKUP_MAP = Collections.unmodifiableMap(map);
    }

    /**
     * Retrieve the {@link DataSizeUnit} for its string representation.
     * 
     * @param asString
     * @return
     */
    public static DataSizeUnit fromString(String asString) {
        return LOOKUP_MAP.get(asString);
    }
}