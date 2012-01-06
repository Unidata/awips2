/*****************************************************************************************
 * COPYRIGHT (c), 2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

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

    /**
     * Display type for a grid data(Image, Contour etc...)
     */
    @XmlAttribute
    private DisplayType displayType;

    public GridLoadProperties() {
        this.displayType = DisplayType.CONTOUR;
    }

    /**
     * Constructor
     * 
     * @param displayType
     */
    public GridLoadProperties(DisplayType displayType) {
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
        GridLoadProperties other = (GridLoadProperties) obj;
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
