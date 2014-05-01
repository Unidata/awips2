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
package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * Monitoring area configuration xml.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 21, 2009            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AreaIdXML {

    @XmlElements({ @XmlElement(name = "StationID", type = StationIdXML.class) })
    private ArrayList<StationIdXML> stationIds = new ArrayList<StationIdXML>();

    @XmlAttribute(name = "id")
    private String areaId;

    @XmlAttribute(name = "type")
    private ZoneType type;

    @XmlAttribute(name = "cLat", required = false)
    private Double cLat;

    @XmlAttribute(name = "cLon", required = false)
    private Double cLon;

    public enum ZoneType {
        MARITIME, REGULAR;
    };

    public AreaIdXML() {
    }

    public ArrayList<StationIdXML> getStationIds() {
        return stationIds;
    }

    public void setStationIds(ArrayList<StationIdXML> stationIds) {
        this.stationIds = stationIds;
    }

    /**
     * @return the areaId
     */
    public String getAreaId() {
        return areaId;
    }

    /**
     * @param areaId
     *            the areaId to set
     */
    public void setAreaId(String areaId) {
        this.areaId = areaId;
    }

    public void addStationIdXml(StationIdXML stationXml) {
        stationIds.add(stationXml);
    }

    /**
     * @return the type
     */
    public ZoneType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(ZoneType type) {
        this.type = type;
    }

    /**
     * @return the cLat
     */
    public Double getCLat() {
        return cLat;
    }

    /**
     * @param lat
     *            the cLat to set
     */
    public void setCLat(Double lat) {
        cLat = lat;
    }

    /**
     * @return the cLon
     */
    public Double getCLon() {
        return cLon;
    }

    /**
     * @param lon
     *            the cLon to set
     */
    public void setCLon(Double lon) {
        cLon = lon;
    }
}
