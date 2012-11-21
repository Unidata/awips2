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
package com.raytheon.uf.common.stats.util;

import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Utility class for data size conversions. KB vs MB vs GB
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2012    728     mpduff      Initial creation.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class UnitUtils implements ISerializableObject {
    /** Bytes per Kilobyte */
    private static final double BYTES_PER_KILOBYTE = 1024.0;

    /** Different unit types for statistics */
    public static enum UnitTypes {
        DATA_SIZE, TIME, COUNT
    }

    /**
     * Data Size Conversions
     */
    public static enum DataSize {
        KB("KB", BYTES_PER_KILOBYTE), MB("MB", BYTES_PER_KILOBYTE
                * BYTES_PER_KILOBYTE), GB("GB", BYTES_PER_KILOBYTE
                * BYTES_PER_KILOBYTE * BYTES_PER_KILOBYTE);

        private final String unit;

        private final double conversion;

        private static Set<String> dataUnits;

        private DataSize(String unit, double conversion) {
            this.unit = unit;
            this.conversion = conversion;
            populateSet();
        }

        private static void populateSet() {
            dataUnits = new HashSet<String>();
            dataUnits.add("KB");
            dataUnits.add("MB");
            dataUnits.add("GB");
        }

        public String getDataUnit() {
            return unit;
        }

        public double getConversion() {
            return conversion;
        }

        public static Set<String> getDataUnits() {
            return dataUnits;
        }
    }

    /**
     * Time Conversions.
     */
    public static enum TimeConversion {
        MS("ms", 1), Second("seconds", TimeUtil.MILLIS_PER_SECOND), Minute(
                "minutes", TimeUtil.MILLIS_PER_MINUTE), Hour("hours",
                TimeUtil.MILLIS_PER_HOUR);

        private static Set<String> dataUnits;

        private final String unit;

        private final double conversion;

        private TimeConversion(String unit, double conversion) {
            this.unit = unit;
            this.conversion = conversion;
            populateSet();
        }

        private static void populateSet() {
            dataUnits = new HashSet<String>();
            dataUnits.add("seconds");
            dataUnits.add("ms");
            dataUnits.add("minutes");
            dataUnits.add("hours");
        }

        public String getDataUnit() {
            return unit;
        }

        public double getConversion() {
            return conversion;
        }

        public static Set<String> getDataUnits() {
            return dataUnits;
        }
    }

    /** The event type */
    private String eventType;

    /** The data type */
    private String dataType;

    /** The display unit */
    @DynamicSerializeElement
    private String displayUnit;

    /** The unit type */
    @DynamicSerializeElement
    private UnitTypes unitType;

    /** Copnversion factor */
    @DynamicSerializeElement
    private double conversion = 1;

    /**
     * Constructor
     */
    public UnitUtils() {

    }

    /**
     * Constructor
     *
     * @param eventType
     *            event type
     * @param dataType
     *            event attribute/data type
     */
    public UnitUtils(String eventType, String dataType) {
        this.eventType = eventType;
        this.dataType = dataType;
        // Default to count
        this.unitType = UnitTypes.COUNT;
    }

    /**
     * The largest value of the data set. This is used to determine which
     * conversion to use if one is not specified.
     *
     * @param value
     *            Largest value of the data set
     *
     * @return The conversion factor
     */
    public void setConversion(double value) {
        // Which unit type is it?
        if (unitType == UnitTypes.COUNT) {
            conversion = 1;
        } else if (unitType == UnitTypes.DATA_SIZE) {
            if (value < DataSize.MB.getConversion()) {
                conversion = DataSize.KB.getConversion();
            } else if (value < DataSize.GB.getConversion()) {
                conversion = DataSize.MB.getConversion();
            } else if (value >= DataSize.GB.getConversion()) {
                conversion = DataSize.GB.getConversion();
            }
        } else if (unitType == UnitTypes.TIME) {
            if (value < TimeUtil.MILLIS_PER_MINUTE) {
                conversion = TimeUtil.MILLIS_PER_SECOND;
            } else if (value < TimeUtil.MILLIS_PER_HOUR) {
                conversion = TimeUtil.MILLIS_PER_MINUTE;
            } else {
                conversion = TimeUtil.MILLIS_PER_SECOND;
            }
        }
    }

    /**
     * @return the eventType
     */
    public String getEventType() {
        return eventType;
    }

    /**
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * Get the conversion
     *
     * @return
     */
    public double getConversion() {
        return conversion;
    }

    /**
     * @return the unitType
     */
    public UnitTypes getUnitType() {
        return unitType;
    }

    /**
     * @param unitType
     *            the unitType to set
     */
    public void setUnitType(UnitTypes unitType) {
        this.unitType = unitType;
    }

    /**
     * @return the displayUnit
     */
    public String getDisplayUnit() {
        return displayUnit;
    }

    /**
     * Set the display unit.
     *
     * @param displayUnit
     */
    public void setDisplayUnit(String displayUnit) {
        this.displayUnit = displayUnit;

        // Determine the unitType
        if (DataSize.getDataUnits().contains(displayUnit)) {
            unitType = UnitTypes.DATA_SIZE;
        } else if (TimeConversion.getDataUnits().contains(displayUnit)) {
            unitType = UnitTypes.TIME;
        }

        if (unitType == UnitTypes.DATA_SIZE) {
            if (displayUnit.equals(DataSize.KB.getDataUnit())) {
                conversion = DataSize.KB.getConversion();
            } else if (displayUnit.equals(DataSize.MB.getDataUnit())) {
                conversion = DataSize.MB.getConversion();
            } else if (displayUnit.equals(DataSize.GB.getDataUnit())) {
                conversion = DataSize.GB.getConversion();
            }
        } else if (unitType == UnitTypes.TIME) {
            if (displayUnit.equals(TimeConversion.MS.getDataUnit())) {
                conversion = 1;
            } else if (displayUnit.equals(TimeConversion.Second.getDataUnit())) {
                conversion = TimeUtil.MILLIS_PER_SECOND;
            } else if (displayUnit.equals(TimeConversion.Minute.getDataUnit())) {
                conversion = TimeUtil.MILLIS_PER_MINUTE;
            }
        }
    }
}
