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
package com.raytheon.uf.common.dataplugin.profiler;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ProfilerSite implements Serializable {
    
    private static final long serialVersionUID = 1L;
    
    @XmlAttribute
    private String stationId;
    @XmlAttribute
    private String profilerId;
    @XmlAttribute
    private String profilerName;
    @XmlAttribute
    private Double latitude;
    @XmlAttribute
    private Double longitude;
    @XmlAttribute
    private Double elevation;
    @XmlElement
    private String comments;
    
    public String getStationId() {
        return stationId;
    }
    
    public void setStationId(String stationId) {
        this.stationId = stationId;
    }
    
    public String getProfilerId() {
        return profilerId;
    }
    
    public void setProfilerId(String profilerId) {
        this.profilerId = profilerId;
    }
    
    public String getProfilerName() {
        return profilerName;
    }
    
    public void setProfilerName(String profilerName) {
        this.profilerName = profilerName;
    }
    
    public Double getLatitude() {
        return latitude;
    }
    
    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }
    
    public Double getLongitude() {
        return longitude;
    }
    
    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }
    
    public Double getElevation() {
        return elevation;
    }

    public void setElevation(Double elevation) {
        this.elevation = elevation;
    }
    
    public String getComments() {
        return comments;
    }
    public void setComments(String comments) {
        this.comments = comments;
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("profiler{");
        sb.append(stationId);
        sb.append(",");
        sb.append(profilerId);
        sb.append(",");
        sb.append(profilerName);
        sb.append("}[");
        sb.append(longitude);
        sb.append("|");
        sb.append(latitude);
        sb.append("|");
        sb.append(elevation);
        sb.append("]");
        if(comments != null) {
            sb.append(":");
            sb.append(comments);
        }
        return sb.toString();
    }

}
