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
package com.raytheon.uf.viz.remote.graphics.events.rendering;

import java.util.Arrays;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Frame that specifies the begining of a new rendering sequence. EndFrameEvent
 * signals the end of the rendering sequence
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
public class BeginFrameEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private double extentFactor;

    @DynamicSerializeElement
    private double[] extentCenter;

    @DynamicSerializeElement
    private int[] rgb;

    /**
     * @return the extentFactor
     */
    public double getExtentFactor() {
        return extentFactor;
    }

    /**
     * @param extentFactor
     *            the extentFactor to set
     */
    public void setExtentFactor(double extentFactor) {
        this.extentFactor = extentFactor;
    }

    /**
     * @return the extentCenter
     */
    public double[] getExtentCenter() {
        return extentCenter;
    }

    /**
     * @param extentCenter
     *            the extentCenter to set
     */
    public void setExtentCenter(double[] extentCenter) {
        this.extentCenter = extentCenter;
    }

    /**
     * @return the rgb
     */
    public int[] getRgb() {
        return rgb;
    }

    /**
     * @param rgb
     *            the rgb to set
     */
    public void setRgb(int[] rgb) {
        this.rgb = rgb;
    }

    public void setColor(RGB color) {
        if (color != null) {
            rgb = new int[] { color.red, color.green, color.blue };
        }
    }

    public RGB getColor() {
        if (rgb != null) {
            return new RGB(rgb[0], rgb[1], rgb[2]);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsRenderEvent
     * #createDiffObject(com.raytheon.uf.viz.remote
     * .graphics.events.rendering.IRenderEvent)
     */
    @Override
    public IRenderEvent createDiffObject(IRenderEvent event) {
        BeginFrameEvent diffEvent = (BeginFrameEvent) event;
        BeginFrameEvent diffObject = new BeginFrameEvent();
        if (Arrays.equals(extentCenter, diffEvent.extentCenter) == false) {
            diffObject.extentCenter = diffEvent.extentCenter;
        }
        diffObject.extentFactor = diffEvent.extentFactor;
        if (Arrays.equals(rgb, diffEvent.rgb) == false) {
            diffObject.rgb = diffEvent.rgb;
        }
        return diffObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        BeginFrameEvent event = (BeginFrameEvent) diffEvent;
        if (event.extentCenter != null) {
            this.extentCenter = event.extentCenter;
        }
        this.extentFactor = event.extentFactor;
        if (event.rgb != null) {
            rgb = event.rgb;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        BeginFrameEvent other = (BeginFrameEvent) obj;
        if (!Arrays.equals(extentCenter, other.extentCenter))
            return false;
        if (Double.doubleToLongBits(extentFactor) != Double
                .doubleToLongBits(other.extentFactor))
            return false;
        if (!Arrays.equals(rgb, other.rgb))
            return false;
        return true;
    }

}
