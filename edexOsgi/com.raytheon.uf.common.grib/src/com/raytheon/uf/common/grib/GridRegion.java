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
package com.raytheon.uf.common.grib;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * Contains the information about a given Grib Model Region.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2016  5182       tjensen     Initial creation
 * Apr 19, 2016 5572       bsteffen    Move to common
 * 
 * </pre>
 * 
 * @author tjensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridRegion {

    @XmlElement
    private String name;

    @XmlElement
    private Double lat1;

    @XmlElement
    private Double lon1;

    @XmlElement
    private Double lat2;

    @XmlElement
    private Double lon2;

    public Polygon getRegionGeometry() {
        return MapUtil.createGeometry(lat1, lon1, lat2, lon2);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Double getLat1() {
        return lat1;
    }

    public void setLat1(Double lat1) {
        this.lat1 = lat1;
    }

    public Double getLon1() {
        return lon1;
    }

    public void setLon1(Double lon1) {
        this.lon1 = lon1;
    }

    public Double getLat2() {
        return lat2;
    }

    public void setLat2(Double lat2) {
        this.lat2 = lat2;
    }

    public Double getLon2() {
        return lon2;
    }

    public void setLon2(Double lon2) {
        this.lon2 = lon2;
    }

}
