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
package com.raytheon.uf.viz.remote.graphics.events.fonts;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;

/**
 * Event for updating a font object's magnification, smoothing, or scaling
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class UpdateFontDataEvent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private float magnification;

    @DynamicSerializeElement
    private Boolean scaleOnMagnify;

    @DynamicSerializeElement
    private boolean smoothing;

    @DynamicSerializeElement
    private boolean scaleFont;

    /**
     * @return the magnification
     */
    public float getMagnification() {
        return magnification;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(float magnification) {
        this.magnification = magnification;
    }

    /**
     * @return the scaleOnMagnify
     */
    public Boolean getScaleOnMagnify() {
        return scaleOnMagnify;
    }

    /**
     * @param scaleOnMagnify
     *            the scaleOnMagnify to set
     */
    public void setScaleOnMagnify(Boolean scaleOnMagnify) {
        this.scaleOnMagnify = scaleOnMagnify;
    }

    /**
     * @return the smoothing
     */
    public boolean getSmoothing() {
        return smoothing;
    }

    /**
     * @param smoothing
     *            the smoothing to set
     */
    public void setSmoothing(boolean smoothing) {
        this.smoothing = smoothing;
    }

    /**
     * @return the scaleFont
     */
    public boolean getScaleFont() {
        return scaleFont;
    }

    /**
     * @param scaleFont
     *            the scaleFont to set
     */
    public void setScaleFont(boolean scaleFont) {
        this.scaleFont = scaleFont;
    }

}
