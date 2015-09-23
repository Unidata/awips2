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
package com.raytheon.uf.edex.plugin.grid.netcdf.description.coverage;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.edex.netcdf.description.AbstractFieldDescription;
import com.raytheon.uf.edex.netcdf.description.AttributeDescription;
import com.raytheon.uf.edex.netcdf.description.ValueDescription;
import com.raytheon.uf.edex.netcdf.description.VariableDescription;

/**
 * Base class for describing the coordinate information of a GridCoverage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2015 4696       nabowle     Initial creation
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class CoverageCoordinatesDescription implements
        CoverageFieldDescription {

    @XmlElements({
        @XmlElement(name = "latitudeValue", type = ValueDescription.class),
        @XmlElement(name = "latitudeVariable", type = VariableDescription.class),
        @XmlElement(name = "latitudeAttribute", type = AttributeDescription.class) })
    protected AbstractFieldDescription latitude;

    @XmlElements({
            @XmlElement(name = "longitudeValue", type = ValueDescription.class),
            @XmlElement(name = "longitudeVariable", type = VariableDescription.class),
            @XmlElement(name = "longitudeAttribute", type = AttributeDescription.class) })
    protected AbstractFieldDescription longitude;

    public CoverageCoordinatesDescription() {
        super();
    }

    /**
     * @return the latitude
     */
    public AbstractFieldDescription getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(AbstractFieldDescription latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public AbstractFieldDescription getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(AbstractFieldDescription longitude) {
        this.longitude = longitude;
    }
}
