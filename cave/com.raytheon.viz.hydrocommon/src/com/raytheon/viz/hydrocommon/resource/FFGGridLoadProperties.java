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
package com.raytheon.viz.hydrocommon.resource;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Flash Flood Guidance Grid Load Properties.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009 2256       mpduff      Initial creation.  Moved here 
 *                                     additional functionality
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@XmlType(name="ffgGridLoadProperties")
public class FFGGridLoadProperties extends LoadProperties {

    /**
     * Display type for a grid data(Image, Contour etc...)
     */
    @XmlAttribute
    private DisplayType displayType;

    public FFGGridLoadProperties() {
        displayType = DisplayType.IMAGE;
    }

    /**
     * Constructor
     * 
     * @param displayType
     */
    public FFGGridLoadProperties(DisplayType displayType) {
        this.displayType = displayType;
    }

    /**
     * Returns the display type.
     * 
     * @return displayType
     */
    public DisplayType getDisplayType() {
        return displayType;
    }

    /**
     * Sets the display type.
     * 
     * @param displayType
     */
    public void setDisplayType(DisplayType displayType) {
        this.displayType = displayType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
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
        FFGGridLoadProperties other = (FFGGridLoadProperties) obj;
        if (displayType == null) {
            if (other.displayType != null) {
                return false;
            }
        } else if (!displayType.equals(other.displayType)) {
            return false;
        }
        return true;
    }
}
