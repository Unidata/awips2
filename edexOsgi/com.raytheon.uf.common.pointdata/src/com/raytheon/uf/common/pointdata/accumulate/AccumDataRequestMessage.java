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
package com.raytheon.uf.common.pointdata.accumulate;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2010            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@DynamicSerialize
public class AccumDataRequestMessage implements IServerRequest {

    @DynamicSerializeElement
    private String pluginName;

    @DynamicSerializeElement
    private String parameter;

    @DynamicSerializeElement
    private String stationParameter;

    @DynamicSerializeElement
    private String[] stations;

    @DynamicSerializeElement
    private String timeParameter;

    @DynamicSerializeElement
    private long[] times;

    @DynamicSerializeElement
    private int totalMinutes;

    @DynamicSerializeElement
    private int incMinutes;

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the stationParameter
     */
    public String getStationParameter() {
        return stationParameter;
    }

    /**
     * @param stationParameter
     *            the stationParameter to set
     */
    public void setStationParameter(String stationParameter) {
        this.stationParameter = stationParameter;
    }

    /**
     * @return the totalMinutes
     */
    public int getTotalMinutes() {
        return totalMinutes;
    }

    /**
     * @param totalMinutes
     *            the totalMinutes to set
     */
    public void setTotalMinutes(int totalMinutes) {
        this.totalMinutes = totalMinutes;
    }

    /**
     * @return the incMinutes
     */
    public int getIncMinutes() {
        return incMinutes;
    }

    /**
     * @param incMinutes
     *            the incMinutes to set
     */
    public void setIncMinutes(int incMinutes) {
        this.incMinutes = incMinutes;
    }

    /**
     * @return the stations
     */
    public String[] getStations() {
        return stations;
    }

    /**
     * @param stations
     *            the stations to set
     */
    public void setStations(String[] stations) {
        this.stations = stations;
    }

    /**
     * @return the timeParameter
     */
    public String getTimeParameter() {
        return timeParameter;
    }

    /**
     * @param timeParameter
     *            the timeParameter to set
     */
    public void setTimeParameter(String timeParameter) {
        this.timeParameter = timeParameter;
    }

    /**
     * @return the times
     */
    public long[] getTimes() {
        return times;
    }

    /**
     * @param times
     *            the times to set
     */
    public void setTimes(long[] times) {
        this.times = times;
    }

}
