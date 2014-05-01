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

package com.raytheon.uf.common.style;

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlValue;

/**
 * Abstract style preferences
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2007            njensen     Initial creation
 * Nov 14, 2013 2361       njensen     Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractStylePreferences {

    @XmlRootElement(name = "displayUnits")
    public static class DisplayUnit {

        @XmlValue
        public String unitString;

        /**
         * While the unitString value is parsed into a valid Unit object which
         * is used for data transformation. The label does not change the actual
         * data in any way, it is displayed unparsed to the user. The label
         * should represent a value equivalant to the unitString but allows
         * non-standard formatting
         */
        @XmlAttribute
        public String label;

        /**
         * @return this DisplayUnit's Unit, null is returned if this instance
         *         has no unitString
         */
        public Unit<?> getUnit() {
            Unit<?> unit = null;
            if (unitString != null && unitString.length() > 0) {
                try {
                    unit = UnitFormat.getUCUMInstance().parseProductUnit(
                            unitString, new ParsePosition(0));
                } catch (ParseException e) {
                    throw new RuntimeException(e);
                }
            }
            return unit;
        }

        public String getLabel() {
            if (label != null) {
                return label;
            } else if (getUnit() != null) {
                return unitString;
            } else {
                return null;
            }
        }

    }

    /*
     * A comma separated list of user definable display flags.
     */
    public static class DisplayFlags {
        @XmlValue
        public String value;

        private Set<String> flags = null;

        public boolean hasFlag(String flag) {
            return getFlags().contains(flag);
        }

        private Set<String> getFlags() {
            if (flags == null) {
                flags = new HashSet<String>();
                if (value != null) {
                    flags.addAll(Arrays.asList(value.split(",")));
                }
            }
            return flags;
        }
    }

    @XmlElementRef
    private final DisplayUnit displayUnits;

    @XmlElement(required = false)
    private final DisplayFlags displayFlags;

    /**
     * Default Constructor
     */
    public AbstractStylePreferences() {
        this.displayUnits = new DisplayUnit();
        this.displayFlags = new DisplayFlags();
    }

    public AbstractStylePreferences(AbstractStylePreferences prefs) {
        this.displayUnits = prefs.displayUnits;
        this.displayFlags = prefs.displayFlags;
    }

    /**
     * @return the displayUnits
     */
    public Unit<?> getDisplayUnits() {
        return this.displayUnits.getUnit();
    }

    public String getDisplayUnitLabel() {
        return this.displayUnits.getLabel();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((displayUnits.getUnit() == null) ? 0 : displayUnits
                        .getUnit().hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
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
        AbstractStylePreferences other = (AbstractStylePreferences) obj;
        if (displayUnits.getUnit() == null) {
            if (other.displayUnits.getUnit() != null) {
                return false;
            }
        } else if (!displayUnits.getUnit().equals(other.displayUnits.getUnit())) {
            return false;
        }
        return true;
    }

    /**
     * @return the displayFlags
     */
    public DisplayFlags getDisplayFlags() {
        return displayFlags;
    }

}
