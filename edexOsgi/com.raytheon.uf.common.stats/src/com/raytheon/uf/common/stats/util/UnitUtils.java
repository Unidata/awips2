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

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.units.DataSizeUnit;

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
 * Jan 17, 2013   1357     mpduff      Refactored to use DataSizeUnit and TimeUnit.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class UnitUtils implements ISerializableObject {
    /** Different unit types for statistics */
    public static enum UnitTypes {
        DATA_SIZE, TIME, COUNT
    }

    /**
     * Time Conversions.
     */
    public static enum TimeConversion {
        MS("ms", "MILLISECONDS"), Second("Seconds", "SECONDS"), Minute(
                "Minutes", "MINUTES"), Hour("Hours", "HOURS");

        private final String unit;

        private final String fullUnit;

        private TimeUnit timeUnit;

        private TimeConversion(String unit, String fullUnit) {
            this.unit = unit;
            this.fullUnit = fullUnit;
            timeUnit = TimeUnit.valueOf(fullUnit);
        }

        public String getDataUnit() {
            return unit;
        }

        public String getFullUnit() {
            return fullUnit;
        }

        public TimeUnit getTimeUnit() {
            return timeUnit;
        }

        public static TimeConversion getInstance(String unit) {
            return TIME_UNIT_LOOKUP.get(unit);
        }
    }

    /**
     * Data size unit lookup map.
     */
    private static final Map<String, DataSizeUnit> DATA_SIZE_UNIT_LOOKUP;
    static {
        DataSizeUnit[] values = DataSizeUnit.values();
        Map<String, DataSizeUnit> map = new LinkedHashMap<String, DataSizeUnit>(
                values.length);
        for (DataSizeUnit dataSize : values) {
            map.put(dataSize.getUnit(), dataSize);
        }
        DATA_SIZE_UNIT_LOOKUP = Collections.unmodifiableMap(map);
    }

    /**
     * Time unit lookup map.
     */
    private static final Map<String, TimeConversion> TIME_UNIT_LOOKUP;
    static {
        TimeConversion[] values = TimeConversion.values();
        Map<String, TimeConversion> map = new LinkedHashMap<String, TimeConversion>(
                values.length);
        for (TimeConversion timeConversion : values) {
            map.put(timeConversion.getDataUnit(), timeConversion);
        }
        TIME_UNIT_LOOKUP = Collections.unmodifiableMap(map);
    }

    /** The event type */
    @DynamicSerializeElement
    private String eventType;

    /** The data type */
    @DynamicSerializeElement
    private String dataType;

    /** The display unit */
    @DynamicSerializeElement
    private String displayUnit;

    /** The unit type */
    @DynamicSerializeElement
    private UnitTypes unitType;

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
     * @param displayUnit
     *            the displayUnit to set
     */
    public void setDisplayUnit(String displayUnit) {
        this.displayUnit = displayUnit;
        setUnitType(displayUnit);
    }

    /**
     * @param eventType
     *            the eventType to set
     */
    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    /**
     * Set the unit type based on the display unit.
     * 
     * @param displayUnit
     *            The display unit
     */
    public void setUnitType(String displayUnit) {
        if (TIME_UNIT_LOOKUP.containsKey(displayUnit)) {
            this.setUnitType(UnitTypes.TIME);
        } else if (DATA_SIZE_UNIT_LOOKUP.containsKey(displayUnit)) {
            this.setUnitType(UnitTypes.DATA_SIZE);
        }
    }

    /**
     * Get the available units for the provided unit type.
     * 
     * @param type
     *            The type of unit
     * @return The available units
     */
    @VisibleForTesting
    public Set<String> getUnitOptions(UnitTypes type) {
        Set<String> units = Collections.emptySet();

        switch (type) {
        case DATA_SIZE:
            units = DATA_SIZE_UNIT_LOOKUP.keySet();
            break;
        case TIME:
            units = TIME_UNIT_LOOKUP.keySet();
            break;
        case COUNT:
            units = new HashSet<String>();
            units.add("Count");
            break;
        default:
            break;
        }

        return units;
    }

    /**
     * Convert the value in original units to displayUnits.
     * 
     * @param unit
     *            original unit
     * @param value
     *            in bytes
     * @return converted value
     */
    @VisibleForTesting
    public double convertDataSizeValue(DataSizeUnit unit, double value) {
        DataSizeUnit ds = DataSizeUnit.fromString(displayUnit);
        if (ds != null) {
            if (ds == DataSizeUnit.BYTE) {
                return unit.toByte((long) value);
            } else if (ds == DataSizeUnit.KB) {
                return unit.toKB((long) value);
            } else if (ds == DataSizeUnit.MB) {
                return unit.toMB((long) value);
            } else if (ds == DataSizeUnit.GB) {
                return unit.toGB((long) value);
            }

            return value;
        }

        return value;
    }

    /**
     * Convert a time from one unit to the display unit.
     * 
     * @param unit
     *            Originating unit
     * @param value
     *            value to convert
     * @return value converted to display unit
     */
    @VisibleForTesting
    public long convertTimeValue(TimeConversion unit, long value) {
        TimeConversion outputTc = TimeConversion.getInstance(displayUnit);
        return outputTc.getTimeUnit().convert(value, unit.getTimeUnit());
    }

    /**
     * Convert the provided value. Time types expect source units to be ms and
     * data size units to be Bytes.
     * 
     * @param value
     * @param view
     * @param displayUnit
     * @return The converted value
     */
    public double convertValue(double value) {
        if (getUnitType() == UnitTypes.TIME) {
            return convertTimeValue(TimeConversion.MS, (long) value);
        } else {
            return convertDataSizeValue(DataSizeUnit.BYTE, value);
        }

    }

    /**
     * Get the different unit options for the provided unit type.
     */
    public Set<String> getUnitOptions() {
        return this.getUnitOptions(this.unitType);
    }
}
