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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Container used to store the calculated/processed information associated with
 * a single lid and source that will be written to the Point Temperature file.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2018 7184       bkowal      Initial creation
 * Apr 06, 2018 7184       bkowal      Set all output to 0 when specified.
 *
 * </pre>
 *
 * @author bkowal
 */

public class TemperatureStationOutput {

    private Double min = null;

    private Double max = null;

    private final Map<SynopticHour, TemperatureOutputValue> synopticHourDataMap;

    public TemperatureStationOutput(
            final Collection<SynopticHour> synopticHourOrder,
            final boolean setZero) {
        synopticHourDataMap = new LinkedHashMap<>(synopticHourOrder.size(),
                1.0f);
        for (SynopticHour synopticHour : synopticHourOrder) {
            final TemperatureOutputValue temperatureOutputValue = new TemperatureOutputValue();
            if (setZero) {
                temperatureOutputValue.setValue(0.0);
            }
            synopticHourDataMap.put(synopticHour, temperatureOutputValue);
        }
    }

    /**
     * @return the min
     */
    public Double getMin() {
        return min;
    }

    /**
     * @param min
     *            the min to set
     */
    public void setMin(Double min) {
        this.min = min;
    }

    /**
     * @return the max
     */
    public Double getMax() {
        return max;
    }

    /**
     * @param max
     *            the max to set
     */
    public void setMax(Double max) {
        this.max = max;
    }

    /**
     * @return the synopticHourDataMap
     */
    public Map<SynopticHour, TemperatureOutputValue> getSynopticHourDataMap() {
        return synopticHourDataMap;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("TemperatureStationOutput [");
        sb.append("min=").append(min);
        sb.append(", max=").append(max);
        sb.append(", synopticHourDataMap=").append(synopticHourDataMap);
        sb.append("]");
        return sb.toString();
    }
}