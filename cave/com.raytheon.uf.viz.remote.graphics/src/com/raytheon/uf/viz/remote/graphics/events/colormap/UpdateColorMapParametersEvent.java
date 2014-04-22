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
package com.raytheon.uf.viz.remote.graphics.events.colormap;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingImage;

/**
 * Event for updating the ColorMapParameters on an IColormappedImage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 08, 2012           mschenke    Initial creation
 * Mar 06, 2014  2831     bsteffen    Add support for no data value.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class UpdateColorMapParametersEvent extends
        AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private byte[] alphaMask;

    @DynamicSerializeElement
    private float colorMapMax;

    @DynamicSerializeElement
    private float colorMapMin;

    @DynamicSerializeElement
    private float dataMax;

    @DynamicSerializeElement
    private float dataMin;

    @DynamicSerializeElement
    private boolean logarithmic;

    @DynamicSerializeElement
    private float logFactor;

    @DynamicSerializeElement
    private boolean mirror;

    @DynamicSerializeElement
    private boolean useMask;

    @DynamicSerializeElement
    private double noDataValue;

    /**
     * @return the alphaMask
     */
    public byte[] getAlphaMask() {
        return alphaMask;
    }

    /**
     * @param alphaMask
     *            the alphaMask to set
     */
    public void setAlphaMask(byte[] alphaMask) {
        this.alphaMask = alphaMask;
    }

    /**
     * @return the colorMapMax
     */
    public float getColorMapMax() {
        return colorMapMax;
    }

    /**
     * @param colorMapMax
     *            the colorMapMax to set
     */
    public void setColorMapMax(float colorMapMax) {
        this.colorMapMax = colorMapMax;
    }

    /**
     * @return the colorMapMin
     */
    public float getColorMapMin() {
        return colorMapMin;
    }

    /**
     * @param colorMapMin
     *            the colorMapMin to set
     */
    public void setColorMapMin(float colorMapMin) {
        this.colorMapMin = colorMapMin;
    }

    /**
     * @return the dataMax
     */
    public float getDataMax() {
        return dataMax;
    }

    /**
     * @param dataMax
     *            the dataMax to set
     */
    public void setDataMax(float dataMax) {
        this.dataMax = dataMax;
    }

    /**
     * @return the dataMin
     */
    public float getDataMin() {
        return dataMin;
    }

    /**
     * @param dataMin
     *            the dataMin to set
     */
    public void setDataMin(float dataMin) {
        this.dataMin = dataMin;
    }

    /**
     * @return the logarithmic
     */
    public boolean isLogarithmic() {
        return logarithmic;
    }

    /**
     * @param logarithmic
     *            the logarithmic to set
     */
    public void setLogarithmic(boolean logarithmic) {
        this.logarithmic = logarithmic;
    }

    /**
     * @return the logFactor
     */
    public float getLogFactor() {
        return logFactor;
    }

    /**
     * @param logFactor
     *            the logFactor to set
     */
    public void setLogFactor(float logFactor) {
        this.logFactor = logFactor;
    }

    /**
     * @return the mirror
     */
    public boolean isMirror() {
        return mirror;
    }

    /**
     * @param mirror
     *            the mirror to set
     */
    public void setMirror(boolean mirror) {
        this.mirror = mirror;
    }

    /**
     * @return the useMask
     */
    public boolean isUseMask() {
        return useMask;
    }

    /**
     * @param useMask
     *            the useMask to set
     */
    public void setUseMask(boolean useMask) {
        this.useMask = useMask;
    }

    public double getNoDataValue() {
        return noDataValue;
    }

    public void setNoDataValue(double noDataValue) {
        this.noDataValue = noDataValue;
    }

    /**
     * Set the ColorMapParameters for the event
     * 
     * @param parameters
     */
    public void setColorMapParameters(ColorMapParameters parameters) {
        if (parameters != null) {
            setAlphaMask(parameters.getAlphaMask());
            setColorMapMin(parameters.getColorMapMin());
            setColorMapMax(parameters.getColorMapMax());
            setDataMin(parameters.getDataMin());
            setDataMax(parameters.getDataMax());
            setLogarithmic(parameters.isLogarithmic());
            setLogFactor(parameters.getLogFactor());
            setMirror(parameters.isMirror());
            setUseMask(parameters.isUseMask());
            setNoDataValue(parameters.getNoDataValue());
        }
    }

    /**
     * Get the update colormap parameters event as ColorMapParameters object
     * 
     * @return
     */
    public ColorMapParameters getColorMapParameters() {
        ColorMapParameters params = new ColorMapParameters();
        params.setAlphaMask(getAlphaMask());
        params.setColorMapMin(getColorMapMin());
        params.setColorMapMax(getColorMapMax());
        params.setDataMin(getDataMin());
        params.setDataMax(getDataMax());
        params.setLogarithmic(isLogarithmic());
        params.setLogFactor(getLogFactor());
        params.setMirror(isMirror());
        params.setUseMask(isUseMask());
        params.setNoDataValue(getNoDataValue());
        return params;
    }

    /**
     * @param wrapper
     * @param parameters
     * @return
     */
    public static UpdateColorMapParametersEvent createEvent(
            AbstractDispatchingImage<?> wrapper, ColorMapParameters parameters) {
        UpdateColorMapParametersEvent event = RemoteGraphicsEventFactory
                .createEvent(UpdateColorMapParametersEvent.class, wrapper);
        event.setColorMapParameters(parameters);
        return event;
    }

}
