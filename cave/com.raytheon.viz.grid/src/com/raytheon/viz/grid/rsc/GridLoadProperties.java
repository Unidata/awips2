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
package com.raytheon.viz.grid.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;

/**
 * Loads properties to have different display types (Image, Contour etc..) for
 * grid data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           Ticket#    Engineer    Description
 * ------------   ---------- ----------- --------------------------
 * July 18, 2008   #1280     S. Manoj     Initial creation
 * 
 * </pre>
 * 
 * @author smanoj
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridLoadProperties extends LoadProperties {

    public GridLoadProperties() {
        super();
    }

    public GridLoadProperties(DisplayType type) {
        super();
        getCapabilities().getCapability(null, DisplayTypeCapability.class)
                .setDisplayType(type);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        DisplayType displayType = getCapabilities().getCapability(null,
                DisplayTypeCapability.class).getDisplayType();
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((displayType == null) ? 0 : displayType.hashCode());
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
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        GridLoadProperties other = (GridLoadProperties) obj;

        DisplayType displayType = getCapabilities().getCapability(null,
                DisplayTypeCapability.class).getDisplayType();
        DisplayType otherType = other.getCapabilities().getCapability(null,
                DisplayTypeCapability.class).getDisplayType();

        if (displayType == null) {
            if (otherType != null) {
                return false;
            }
        } else if (!displayType.equals(otherType)) {
            return false;
        }
        return true;
    }

    public boolean equalsIgnoreType(Object obj) {
        return super.equals(obj);
    }

    @XmlAttribute(name = "displayType")
    public DisplayType getDisplayType() {
        return getCapabilities().getCapability(null,
                DisplayTypeCapability.class).getDisplayType();
    }

    public void setDisplayType(DisplayType displayType) {
        getCapabilities().getCapability(null, DisplayTypeCapability.class)
                .setDisplayType(displayType);
    }

}
