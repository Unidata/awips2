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
package com.raytheon.uf.common.dataplugin.acarssounding.tools;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2009            jkorman     Initial creation
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class Airport {

    @XmlElement
    private String id;

    @XmlElement
    private Float latitude;

    @XmlElement
    private Float longitude;

    @XmlElement
    private Double elevation;

    @XmlElement
    private Double distance;

    /**
     * 
     */
    public Airport() {

    }

    /**
     * 
     * @param airport
     * @return
     */
    public final Airport copy() {
        Airport a = new Airport();
        a.setId(getId());
        a.setDistance(getDistance());
        a.setElevation(getElevation());
        a.setLatitude(getLatitude());
        a.setLongitude(getLongitude());

        return a;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the latitude
     */
    public Float getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(Float latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public Float getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(Float longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the elevation
     */
    public Double getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(Double elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the distance
     */
    public Double getDistance() {
        return distance;
    }

    /**
     * @param distance
     *            the distance to set
     */
    public void setDistance(Double distance) {
        this.distance = distance;
    }

    /**
     * 
     */
    public String toString() {
        return String.format("%s", id);
    }
}
