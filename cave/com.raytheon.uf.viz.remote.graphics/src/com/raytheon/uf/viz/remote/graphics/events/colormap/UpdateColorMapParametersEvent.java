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

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2012            mschenke     Initial creation
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
    private float[] red;

    @DynamicSerializeElement
    private float[] blue;

    @DynamicSerializeElement
    private float[] green;

    @DynamicSerializeElement
    private float[] alpha;

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
     * @return the red
     */
    public float[] getRed() {
        return red;
    }

    /**
     * @param red
     *            the red to set
     */
    public void setRed(float[] red) {
        this.red = red;
    }

    /**
     * @return the blue
     */
    public float[] getBlue() {
        return blue;
    }

    /**
     * @param blue
     *            the blue to set
     */
    public void setBlue(float[] blue) {
        this.blue = blue;
    }

    /**
     * @return the green
     */
    public float[] getGreen() {
        return green;
    }

    /**
     * @param green
     *            the green to set
     */
    public void setGreen(float[] green) {
        this.green = green;
    }

    /**
     * @return the alpha
     */
    public float[] getAlpha() {
        return alpha;
    }

    /**
     * @param alpha
     *            the alpha to set
     */
    public void setAlpha(float[] alpha) {
        this.alpha = alpha;
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

    /**
     * Get the update colormap parameters event as ColorMapParameters object
     * 
     * @return
     */
    public ColorMapParameters asColorMapParameters() {
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
        if (red != null && green != null && blue != null && alpha != null) {
            params.setColorMap(new ColorMap("" + getObjectId(), red, green,
                    blue, alpha));
        }
        return params;
    }

    public static UpdateColorMapParametersEvent createEvent(
            DispatchingObject<?> creator, ColorMapParameters parameters) {
        UpdateColorMapParametersEvent event = RemoteGraphicsEventFactory
                .createEvent(UpdateColorMapParametersEvent.class, creator);
        event.setAlphaMask(parameters.getAlphaMask());
        IColorMap cmap = parameters.getColorMap();
        if (cmap != null) {
            event.setRed(cmap.getRed());
            event.setBlue(cmap.getBlue());
            event.setGreen(cmap.getGreen());
            event.setAlpha(cmap.getAlpha());
        }
        event.setColorMapMin(parameters.getColorMapMin());
        event.setColorMapMax(parameters.getColorMapMax());
        event.setDataMin(parameters.getDataMin());
        event.setDataMax(parameters.getDataMax());
        event.setLogarithmic(parameters.isLogarithmic());
        event.setLogFactor(parameters.getLogFactor());
        event.setMirror(parameters.isMirror());
        event.setUseMask(parameters.isUseMask());
        return event;
    }

}
