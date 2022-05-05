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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * Class containing the XML data specifying the area ID and an array of
 * AreaThresholdXML data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Dec 15, 2009  3963     lvenable  Initial creation
 * Jan 04, 2016  5115     skorolev  moved from com.raytheon.uf.viz.monitor.xml
 * May 07, 2019  7689     randerso  Code cleanup
 *
 * </pre>
 *
 * @author lvenable
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AreaXML {
    @XmlElement(name = "AreaID")
    private String areaId;

    @XmlElements({
            @XmlElement(name = "AreaThreshold", type = AreaThresholdXML.class) })
    private List<AreaThresholdXML> areaThresholds;

    private Map<String, AreaThresholdXML> areaThresholdMap;

    /**
     * Nullary constructor
     */
    public AreaXML() {
    }

    /**
     * @return the area ID
     */
    public String getAreaId() {
        return areaId;
    }

    /**
     * @param areaId
     *            the area ID
     */
    public void setAreaId(String areaId) {
        this.areaId = areaId;
    }

    /**
     * @return list of area thresholds
     */
    public List<AreaThresholdXML> getAreaThresholds() {
        return areaThresholds;
    }

    /**
     * @param areaThresholds
     *            list of area thresholds
     */
    public void setAreaThresholds(List<AreaThresholdXML> areaThresholds) {
        this.areaThresholds = areaThresholds;
    }

    private synchronized Map<String, AreaThresholdXML> getThresholdMap() {
        if (areaThresholdMap == null) {
            areaThresholdMap = new HashMap<>();

            for (AreaThresholdXML areaThreshold : areaThresholds) {
                areaThresholdMap.put(areaThreshold.getKey(), areaThreshold);
            }
        }

        return areaThresholdMap;
    }

    /**
     * @param key
     * @return the red threshold value for key
     */
    public double getRedValue(String key) {
        return getThresholdMap().get(key).getRed();
    }

    /**
     * @param key
     * @param value
     *            the red threshold value for key
     */
    public void setRedValue(String key, double value) {
        getThresholdMap().get(key).setRed(value);
    }

    /**
     * @param key
     * @return the yellow threshold value for key
     */
    public double getYellowValue(String key) {
        return getThresholdMap().get(key).getYellow();
    }

    /**
     * @param key
     * @param value
     *            the yellow threshold value for key
     */
    public void setYellowValue(String key, double value) {
        getThresholdMap().get(key).setYellow(value);
    }

    /**
     * @return an independent copy of this AreaXML
     */
    public AreaXML copy() {
        AreaXML newAreaXML = new AreaXML();
        newAreaXML.areaId = this.areaId;
        newAreaXML.areaThresholds = new ArrayList<>(this.areaThresholds.size());
        for (AreaThresholdXML a : this.areaThresholds) {
            newAreaXML.areaThresholds.add(a.copy());
        }
        return newAreaXML;
    }
}
