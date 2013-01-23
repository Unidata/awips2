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
package com.raytheon.uf.viz.datadelivery.subscription.subset.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import org.apache.commons.lang.StringUtils;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml;

/**
 * Subset Timing Tab saved settings object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            mpduff       Initial creation
 * Aug 21, 2012 0743       djohnson     Add specificDate, use append rather than concatenate strings.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class TimeXML implements IDisplayXml {

    @XmlElement(name = "latestData")
    protected boolean latestData;

    @XmlElements({ @XmlElement(name = "cycle", type = Integer.class) })
    protected List<Integer> cycleList = new ArrayList<Integer>();
    
    @XmlElements({ @XmlElement(name = "fcstHour", type = String.class) })
    protected List<String> fcstHourList = new ArrayList<String>();

    /**
     * @return the latestData
     */
    public boolean isLatestData() {
        return latestData;
    }

    /**
     * @param latestData the latestData to set
     */
    public void setLatestData(boolean latestData) {
        this.latestData = latestData;
    }

    /**
     * @return the cycle
     */
    public List<Integer> getCycles() {
        return cycleList;
    }

    /**
     * @param cycle the cycle to set
     */
    public void setCycles(List<Integer> cycles) {
        this.cycleList = cycles;
    }
    
    /**
     * @param cycle
     */
    public void addCycle(int cycle) {
        this.cycleList.add(cycle);
    }

    /**
     * @return the fcstHour
     */
    public List<String> getFcstHours() {
        return fcstHourList;
    }

    /**
     * @param fcstHours the fcstHour to set
     */
    public void setFcstHours(List<String> fcstHours) {
        this.fcstHourList = fcstHours;
    }

    /**
     * Add Forecast Hour
     * @param hour
     */
    public void addHour(String hour) {
        this.fcstHourList.add(hour);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml#getDisplayXmlString()
     */
    @Override
    public String getDisplayXmlString() {
        StringBuilder sb = new StringBuilder();
        
        sb.append(Util.EOL);
        if (latestData) {
            sb.append("Requesting Latest Data");
        } else {
            sb.append(getNonLatestData());
        }
        sb.append(Util.EOL);
        
        if (!CollectionUtil.isNullOrEmpty(cycleList)) {
            sb.append("Cycles:").append(Util.EOL);
            for (Integer cycle : cycleList) {
                sb.append(" ").append(
                        StringUtils.leftPad(cycle.toString(), 2, '0'));
            }
            sb.append(Util.EOL);
        }
        
        if (!CollectionUtil.isNullOrEmpty(fcstHourList)) {
            sb.append("Forecast Hours:").append(Util.EOL);
            for (String fcst: fcstHourList) {
                sb.append(" ").append(fcst);
            }
            sb.append(Util.EOL);
        }
        return sb.toString();
    }

    /**
     * Return the display string for non-latest data.
     * 
     * @return the display string
     */
    protected abstract String getNonLatestData();
}