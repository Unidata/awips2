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
package com.raytheon.uf.viz.remote.graphics.events.imagery;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class UpdateImageDataEvent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private boolean interpolated = false;

    @DynamicSerializeElement
    private float brightness = 1.0f;

    @DynamicSerializeElement
    private float contrast = 1.0f;

    /**
     * @return the interpolated
     */
    public boolean isInterpolated() {
        return interpolated;
    }

    /**
     * @param interpolated
     *            the interpolated to set
     */
    public void setInterpolated(boolean interpolated) {
        this.interpolated = interpolated;
    }

    /**
     * @return the brightness
     */
    public float getBrightness() {
        return brightness;
    }

    /**
     * @param brightness
     *            the brightness to set
     */
    public void setBrightness(float brightness) {
        this.brightness = brightness;
    }

    /**
     * @return the contrast
     */
    public float getContrast() {
        return contrast;
    }

    /**
     * @param contrast
     *            the contrast to set
     */
    public void setContrast(float contrast) {
        this.contrast = contrast;
    }

}
