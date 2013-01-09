package com.raytheon.uf.viz.datadelivery.utils;

/**
 * Enumeration for Data Size units and conversions.
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
public enum DataSizeUtil {
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
        public long convert(long l, DataSizeUtil ds) {
            return ds.toByte(l);
        }
    },
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
        public long convert(long l, DataSizeUtil ds) {
            return ds.toKB(l);
        }
    },
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
        public long convert(long l, DataSizeUtil ds) {
            return ds.toMB(l);
        }
    },
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
        public long convert(long l, DataSizeUtil ds) {
            return ds.toGB(l);
        }
    };

    private static final long CONV = 1024;

    /** String unit */
    private String unit;

    private DataSizeUtil(String unit) {
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
    public abstract long convert(long l, DataSizeUtil ds);
}