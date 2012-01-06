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
package com.raytheon.uf.common.monitor.scan;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * CWA ThreatLocation
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/06/2009   2037       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ThreatLocation implements ISerializableObject {
    
    @XmlElement
    @DynamicSerializeElement
    public String locationName = null;
    @XmlElement
    @DynamicSerializeElement
    public String wfoName = null;
    @XmlElement
    @DynamicSerializeElement
    public Double lat = null;
    @XmlElement
    @DynamicSerializeElement
    public Double lon = null;
       
    /**
     * For thrift
     */
    public ThreatLocation() {
        
    }
    
    /**
     * For actual use
     */
    public ThreatLocation(String wfoName, String locationName, Coordinate coor) {
        this.wfoName = wfoName;
        this.locationName = locationName;
        this.lat = coor.y;
        this.lon = coor.x;
    }
    
    public String getLocationName() {
        return locationName;
    }

    public void setLocationName(String locationName) {
        this.locationName = locationName;
    }
   
    public Double getLat() {
        return lat;
    }
    
    public void setLat(Double lat) {
        this.lat = lat;
    }
    
    public Double getLon() {
        return lon;
    }
    
    public void setLon(Double lon) {
        this.lon = lon;
    }
   
    public Coordinate getCoor() {
        return new Coordinate(getLon(), getLat(), 0.0);
    }

    public String getWfoName() {
        return wfoName;
    }

    public void setWfoName(String wfoName) {
        this.wfoName = wfoName;
    }
}
