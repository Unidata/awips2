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
package com.raytheon.uf.common.dataplugin.modelsounding;

import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_CLOUD_COVER;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_OMEGA;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_PRESS;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_SPEC_HUM;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_TEMP;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_U_COMP;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_V_COMP;

import java.io.Serializable;

import com.raytheon.uf.common.pointdata.PointDataView;

/**
 * SoundingLevel contains the data for a single vertical level forecast.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Dec 02, 2013  2537     bsteffen    Move to common, remove unnecessary 
 *                                    fields, remove ISerializableObject
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SoundingLevel implements Serializable {

    private static final long serialVersionUID = 1L;

    private final PointDataView pointDataView;

    private final int index;

    SoundingLevel(PointDataView pointDataView, int index) {
        this.index = index;
        this.pointDataView = pointDataView;
    }

    /** @return the pressure */
    public float getPressure() {
        return pointDataView.getFloat(LVL_PRESS, index);
    }

    /**
     * @param pressure
     *            the pressure to set
     */
    public void setPressure(float pressure) {
        pointDataView.setFloat(LVL_PRESS, pressure, index);
    }

    /** @return the temperature */
    public float getTemperature() {
        return pointDataView.getFloat(LVL_TEMP, index);
    }

    /**
     * @param temperature
     *            the temperature to set
     */
    public void setTemperature(float temperature) {
        pointDataView.setFloat(LVL_TEMP, temperature, index);
    }

    /**
     * Get the u wind component of the horizontal wind.
     * 
     * @return the uWind
     */
    public float getUcWind() {
        return pointDataView.getFloat(LVL_U_COMP, index);
    }

    /**
     * Set the u wind component of the horizontal wind.
     * 
     * @param wind
     *            the uWind to set
     */
    public void setUcWind(float wind) {
        pointDataView.setFloat(LVL_U_COMP, wind, index);
    }

    /**
     * Get the v wind component of the horizontal wind.
     * 
     * @return the vWind
     */
    public float getVcWind() {
        return pointDataView.getFloat(LVL_V_COMP, index);
    }

    /**
     * Set the v wind component of the horizontal wind.
     * 
     * @param wind
     *            the vWind to set
     */
    public void setVcWind(float wind) {
        pointDataView.setFloat(LVL_V_COMP, wind, index);
    }

    /** @return the specificHumidity */
    public float getSpecificHumidity() {
        return pointDataView.getFloat(LVL_SPEC_HUM, index);
    }

    /**
     * @param specificHumidity
     *            the specificHumidity to set
     */
    public void setSpecificHumidity(float specificHumidity) {
        pointDataView.setFloat(LVL_SPEC_HUM, specificHumidity, index);
    }

    /** @return the omega */
    public float getOmega() {
        return pointDataView.getFloat(LVL_OMEGA, index);
    }

    /**
     * @param omega
     *            the omega to set
     */
    public void setOmega(float omega) {
        pointDataView.setFloat(LVL_OMEGA, omega, index);
    }

    /** @return the lyrCldCvr */
    public float getLyrCldCvr() {
        return pointDataView.getFloat(LVL_CLOUD_COVER, index);
    }

    /**
     * @param lyrCldCvr
     *            the lyrCldCvr to set
     */
    public void setLyrCldCvr(float lyrCldCvr) {
        pointDataView.setFloat(LVL_CLOUD_COVER, lyrCldCvr, index);
    }

}
