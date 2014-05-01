/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

/**
 * Vertical level information which can be a point or range.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class VerticalCoordinate implements Comparable<VerticalCoordinate> {
    
    public static final Unit<Length> FLIGHT_LEVEL_UNIT = SI.HECTO(NonSI.FOOT);
    
    static {
        UnitFormat.getInstance().alias(FLIGHT_LEVEL_UNIT, "FL");
        UnitFormat.getInstance().label(FLIGHT_LEVEL_UNIT, "FL");
    }

    public static enum Reference {
        ABOVE_GROUND("AGL", 2000, "height above ground"), ABOVE_MSL("AMSL",
                2005, "gravity-related height"), ABOVE_ELLIPSOID("AEH", 2002,
                "ellipsoidal height"), PRESSURE_LEVEL("PL", 2003,
                "barometric altitude"), FLIGHT_LEVEL("FL", 2003, "flight level"), UNKNOWN(
                "", 2000, "unknown");

        private static final Map<String, Reference> ABB_MAP;

        static {
            Map<String, Reference> map = new HashMap<String, VerticalCoordinate.Reference>(
                    Reference.values().length);
            for (Reference r : Reference.values()) {
                map.put(r.abbreviation, r);
            }
            ABB_MAP = Collections.unmodifiableMap(map);
        }

        public static Reference fromAbbreviation(String abb) {
            Reference rval = ABB_MAP.get(abb);
            if ( rval == null){
                rval = Reference.UNKNOWN;
            }
            return rval;
        }
        
        public final String abbreviation;

        public final int datumType;

        public final String longName;

        private Reference(String abb, int datum, String longName) {
            this.abbreviation = abb;
            this.datumType = datum;
            this.longName = longName;
        }
    };

    private final double min;

    private final double max;

    private final Unit<?> units;

    private final Reference ref;

    private final boolean range;

    private VerticalCoordinate(double min, double max, Unit<?> units,
            Reference ref, boolean isRange) {
        this.min = Math.min(min, max);
        this.max = Math.max(min, max);
        this.units = units;
        this.ref = ref;
        this.range = isRange;
    }

    public VerticalCoordinate(double min, double max, Unit<?> units,
            Reference ref) {
        this(min, max, units, ref, true);
    }

    public VerticalCoordinate(double min, double max, Unit<?> units) {
        this(min, max, units, Reference.ABOVE_MSL);
    }

    public VerticalCoordinate(double value, Unit<?> units, Reference ref) {
        this(value, value, units, ref, false);
    }

    public VerticalCoordinate(double value, Unit<?> units) {
        this(value, units, Reference.ABOVE_MSL);
    }

    public double getValue() {
        return min;
    }

    /**
     * @return the min
     */
    public double getMin() {
        return min;
    }

    /**
     * @return the max
     */
    public double getMax() {
        return max;
    }

    /**
     * @return the units
     */
    public Unit<?> getUnits() {
        return units;
    }

    /**
     * @return the ref
     */
    public Reference getRef() {
        return ref;
    }

    /**
     * @return the range
     */
    public boolean isRange() {
        return range;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(VerticalCoordinate o) {
        if (o == null) {
            return 1;
        }
        byte state = 0x00;
        if (this.range) {
            state |= 0x01;
        }
        if (o.range) {
            state |= 0x02;
        }
        switch (state) {
        case 0:
            // both single value
            return Double.compare(this.min, o.min);
        case 1:// this range, other single
        case 3:// both range
            if (this.min > o.max) {
                return 1;
            }
            if (this.max < o.min) {
                return -1;
            }
            return 0;
        case 2:
            // this single, other range
            if (this.min > o.max) {
                return 1;
            }
            if (this.min < o.min) {
                return -1;
            }
            return 0;
        }

        return 0;
    }

}
