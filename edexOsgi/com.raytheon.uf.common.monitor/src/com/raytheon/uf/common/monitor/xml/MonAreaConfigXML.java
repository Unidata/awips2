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
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Monitoring area configuration xml.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------
 * Dec 21, 2009           lvenable  Initial creation
 * May 15, 2014  3086     skorolev  Changed type for timeWindow element.
 * Sep 03, 2015  3841     skorolev  Added containsArea method.
 * May 21, 2019  7689     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 */
@XmlRootElement(name = "MonitorAreaConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class MonAreaConfigXML {

    @XmlElements({ @XmlElement(name = "AreaID", type = AreaIdXML.class) })
    private List<AreaIdXML> areaIds = new ArrayList<>();

    /**
     * Time period in hours during which stations are monitoring.
     */
    @XmlElements({ @XmlElement(name = "timeWindow", type = Double.class) })
    private double timeWindow;

    /**
     * The shortest distance between center of site and a ship.
     */
    @XmlElements({ @XmlElement(name = "shipDistance", type = Integer.class) })
    private int shipDistance;

    /**
     * Flag to use the Fog Monitor overall threat level.
     */
    @XmlElements({ @XmlElement(name = "useAlgorithms", type = Boolean.class) })
    private boolean useAlgorithms;

    public MonAreaConfigXML() {
    }

    public List<AreaIdXML> getAreaIds() {
        return areaIds;
    }

    public void setAreaIds(List<AreaIdXML> areaIds) {
        this.areaIds = areaIds;
    }

    /**
     * @return the timeWindow
     */
    public double getTimeWindow() {
        return timeWindow;
    }

    /**
     * @param hours
     *            the timeWindow to set
     */
    public void setTimeWindow(double hours) {
        this.timeWindow = hours;
    }

    /**
     * @return the shipDistance
     */
    public int getShipDistance() {
        return shipDistance;
    }

    /**
     * @param shipDistance
     *            the shipDistance to set
     */
    public void setShipDistance(int shipDistance) {
        this.shipDistance = shipDistance;
    }

    /**
     * @return the useAlgorithms
     */
    public boolean isUseAlgorithms() {
        return useAlgorithms;
    }

    /**
     * @param useAlgorithms
     *            the useAlgorithms to set
     */
    public void setUseAlgorithms(boolean useAlgorithms) {
        this.useAlgorithms = useAlgorithms;
    }

    /**
     * @param areaXml
     */
    public void addAreaId(AreaIdXML areaXml) {
        areaIds.add(areaXml);
    }

    /**
     * Returns true if areaXML exist for area ID.
     *
     * @param area
     *            area ID
     * @return areaXML
     */
    public boolean containsArea(String areaId) {
        Iterator<AreaIdXML> itr = getAreaIds().iterator();
        while (itr.hasNext()) {
            if (itr.next().getAreaId().equals(areaId)) {
                return true;
            }
        }
        return false;
    }
}
