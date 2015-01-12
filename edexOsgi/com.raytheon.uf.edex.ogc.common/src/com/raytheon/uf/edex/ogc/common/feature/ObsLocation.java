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
package com.raytheon.uf.edex.ogc.common.feature;

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import net.opengis.gml.v_3_1_1.DirectPositionType;
import net.opengis.gml.v_3_1_1.PointType;

import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;


/**
 * ObsLocation OGC representation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2013  1746     dhladky     Initial creation
 * Jul 23, 2014 3410      bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "obsLocation", propOrder = {
    "location"
})
public class ObsLocation {

    @XmlElement(required = true)
    protected PointType location;
    @XmlAttribute
    protected String stationId;
    @XmlAttribute
    protected Integer elevation;
    
    /**
     * public cons
     */
    public ObsLocation() {
    	
    }
    
    public ObsLocation(SurfaceObsLocation sol) {
    	
    	this.setStationId(sol.getStationId());
    	this.setElevation(sol.getElevation());
        Double lat = sol.getLatitude().doubleValue();
        Double lon = sol.getLongitude().doubleValue();
		PointType point = new PointType();
		DirectPositionType pos = new DirectPositionType();
		pos.setValue(Arrays.asList(lon, lat));
		point.setPos(pos);
		this.setLocation(point);
    }

    /**
     * Gets the value of the location property.
     * 
     * @return
     *     possible object is
     *     {@link PointType }
     *     
     */
    public PointType getLocation() {
        return location;
    }

    /**
     * Sets the value of the location property.
     * 
     * @param value
     *     allowed object is
     *     {@link PointType }
     *     
     */
    public void setLocation(PointType value) {
        this.location = value;
    }

    /**
     * Gets the value of the stationId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStationId() {
        return stationId;
    }

    /**
     * Sets the value of the stationId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStationId(String value) {
        this.stationId = value;
    }

    /**
     * Gets the value of the elevation property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getElevation() {
        return elevation;
    }

    /**
     * Sets the value of the elevation property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setElevation(Integer value) {
        this.elevation = value;
    }

}
