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
package com.raytheon.uf.common.dataplugin.poessounding;

import java.io.Serializable;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * ProfilerLevel contains the data for a single vertical level observation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080303           1026 jkorman     Initial implementation.
 * 20131022           2361 njensen     Remove XML annotations
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
public class POESSoundingLevel implements Serializable {

    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    private Integer pressure;

    @DynamicSerializeElement
    private Double temperature;

    @DynamicSerializeElement
    private Double mixingRatio;

    /**
     * Construct an empty instance.
     */
    public POESSoundingLevel() {
    }

    /**
     * @return the pressure
     */
    public Integer getPressure() {
        return pressure;
    }

    /**
     * @param pressure
     *            the pressure to set
     */
    public void setPressure(Integer pressure) {
        this.pressure = pressure;
    }

    /**
     * @return the temperature
     */
    public Double getTemperature() {
        return temperature;
    }

    /**
     * @param temperature
     *            the temperature to set
     */
    public void setTemperature(Double temperature) {
        this.temperature = temperature;
    }

    /**
     * @return the mixingRatio
     */
    public Double getMixingRatio() {
        return mixingRatio;
    }

    /**
     * @param mixingRatio
     *            the mixingRatio to set
     */
    public void setMixingRatio(Double mixingRatio) {
        this.mixingRatio = mixingRatio;
    }

}
