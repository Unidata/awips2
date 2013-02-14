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
package com.raytheon.edex.plugin.gfe.paraminfo;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.time.TimeRange;

/**
 * 
 * Container class to hold a list of metadata pertaining to grid parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2010 #6372      bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@XmlRootElement(name = "gridParamInfo")
@XmlAccessorType(XmlAccessType.NONE)
public class GridParamInfo {

    /** The generating processes associated with this model */
    @XmlElementWrapper(name = "valtimeMINUSreftime", required = false)
    @XmlElement(name = "fcst")
    private ArrayList<Integer> times = new ArrayList<Integer>();

    /** List of parameter information */
    @XmlElements({ @XmlElement(name = "gridParameterInfo", type = ParameterInfo.class) })
    private ArrayList<ParameterInfo> gridParamInfo;

    /**
     * @return the gridParamInfo
     */
    public ArrayList<ParameterInfo> getGridParamInfo() {
        return gridParamInfo;
    }

    /**
     * @param gridParamInfo
     *            the gridParamInfo to set
     */
    public void setGridParamInfo(ArrayList<ParameterInfo> gridParamInfo) {
        this.gridParamInfo = gridParamInfo;
    }

    /**
     * Gets the parameter information pertaining to the provided parameter name
     * 
     * @param parameter
     *            The parameter to get the information for
     * @return The parameter information
     */
    public ParameterInfo getParameterInfo(String parameter) {
        for (ParameterInfo info : gridParamInfo) {
            if (info.getShort_name().equals(parameter)) {
                return info;
            }
        }
        return null;
    }

    /**
     * @return the times
     */
    public ArrayList<Integer> getTimes() {
        return times;
    }

    /**
     * @param times
     *            the times to set
     */
    public void setTimes(ArrayList<Integer> times) {
        this.times = times;
    }

    public List<TimeRange> getAvailableTimes(Date refTime) {
        List<TimeRange> availTimes = new ArrayList<TimeRange>();
        for (int i = 1; i < times.size(); i++) {
            availTimes.add(new TimeRange(refTime.getTime() + times.get(i - 1)
                    * 1000, refTime.getTime() + times.get(i) * 1000));
        }
        return availTimes;
    }

}
