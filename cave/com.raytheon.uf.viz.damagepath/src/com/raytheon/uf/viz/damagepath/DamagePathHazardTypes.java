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
package com.raytheon.uf.viz.damagepath;

import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang.StringUtils;

/**
 * Class to represent configured list of possible hazard causes for a damage
 * path. Default file is common_static/damagepath/hazard-types.xml.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 27, 2016  #5287     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@XmlRootElement(name = "HazardTypes")
@XmlAccessorType(XmlAccessType.NONE)
public class DamagePathHazardTypes {

    @XmlRootElement(name = "hazard")
    @XmlAccessorType(XmlAccessType.NONE)
    public static class HazardTypeProperties {

        @XmlElement
        @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
        private String longName;

        @XmlElement
        @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
        private String abbreviation;

        /**
         * Default constructor. Should not be used except by JAXB to unmarshal
         * data from XML.
         */
        public HazardTypeProperties() {
            this(StringUtils.EMPTY, StringUtils.EMPTY);
        }

        /**
         * Construct an instance of this object with the specified name and
         * abbreviation.
         * 
         * @param longName
         *            Long form description of the hazard.
         * @param abbreviation
         *            Abbreviation for the hazard. Convention is to use 2
         *            characters.
         */
        public HazardTypeProperties(String longName, String abbreviation) {
            this.longName = longName;
            this.abbreviation = abbreviation;
        }

        /**
         * Takes the given UI string representation of the object and parses out
         * just the abbreviation. Expected format is
         * "<longName> (<abbreviation>)".
         * 
         * @param uiString
         * @return
         */
        public static String getAbbreviationFromUIString(String uiString) {
            int idx = uiString.lastIndexOf('(');
            if (idx > -1) {
                return uiString.substring(idx + 1, idx + 3);
            }

            return StringUtils.EMPTY;
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("HazardTypeProperties [longName=");
            builder.append(longName);
            builder.append(", abbreviation=");
            builder.append(abbreviation);
            builder.append("]");
            return builder.toString();
        }

        /**
         * Returns a string representation of the object useful for display in
         * UI elements.
         * 
         * @return UI-friendly string representation of this object.
         */
        public String toUIString() {
            StringBuilder builder = new StringBuilder();
            builder.append(longName);
            builder.append(" (");
            builder.append(abbreviation);
            builder.append(")");
            return builder.toString();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((abbreviation == null) ? 0 : abbreviation.hashCode());
            result = prime * result
                    + ((longName == null) ? 0 : longName.hashCode());
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
            HazardTypeProperties other = (HazardTypeProperties) obj;
            if (abbreviation == null) {
                if (other.abbreviation != null) {
                    return false;
                }
            } else if (!abbreviation.equals(other.abbreviation)) {
                return false;
            }
            if (longName == null) {
                if (other.longName != null) {
                    return false;
                }
            } else if (!longName.equals(other.longName)) {
                return false;
            }
            return true;
        }

        public String getLongName() {
            return longName;
        }

        public void setLongName(String longName) {
            this.longName = longName;
        }

        public String getAbbreviation() {
            return abbreviation;
        }

        public void setAbbreviation(String abbreviation) {
            this.abbreviation = abbreviation;
        }
    }

    @XmlElementRef
    private Collection<HazardTypeProperties> hazards;

    /**
     * Default constructor. Should not be used except by JAXB to unmarshal data
     * from XML.
     */
    public DamagePathHazardTypes() {
    }

    /**
     * Constructs a new instance of this object with the given collection of
     * {@code HazardTypeProperties} objects.
     * 
     * @param hazards
     */
    public DamagePathHazardTypes(Collection<HazardTypeProperties> hazards) {
        this.setHazards(hazards);
    }

    public Collection<HazardTypeProperties> getHazards() {
        return hazards;
    }

    public void setHazards(Collection<HazardTypeProperties> hazards) {
        this.hazards = hazards;
    }
}
