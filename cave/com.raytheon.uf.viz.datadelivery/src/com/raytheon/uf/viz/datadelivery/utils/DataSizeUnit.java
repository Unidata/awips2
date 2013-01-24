package com.raytheon.uf.viz.datadelivery.utils;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

/**
 * Enumeration for Data Size units and conversions. Based off of TimeUnit class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2013  1420       mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlType(name = "ruleUnit")
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
}