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
package com.raytheon.uf.viz.remote.graphics.events.clipping;

import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event for setting up the clipping pane for rendering
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class SetupClippingPane extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private double[] extent;

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
        SetupClippingPane diffEvent = (SetupClippingPane) event;
        SetupClippingPane diffObject = new SetupClippingPane();
        if (Arrays.equals(extent, diffEvent.extent) == false) {
            diffObject.extent = diffEvent.extent;
        }
        return diffObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent#
     * applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        SetupClippingPane diffObject = (SetupClippingPane) diffEvent;
        if (diffObject.extent != null) {
            extent = diffObject.extent;
        }
    }

    /**
     * @return the extent
     */
    public double[] getExtent() {
        return extent;
    }

    /**
     * @param extent
     *            the extent to set
     */
    public void setExtent(double[] extent) {
        this.extent = extent;
    }

    public void setIExtent(IExtent extent) {
        if (extent != null) {
            setExtent(new double[] { extent.getMinX(), extent.getMaxX(),
                    extent.getMinY(), extent.getMaxY() });
        }
    }

    public IExtent getIExtent() {
        if (extent != null) {
            return new PixelExtent(extent[0], extent[1], extent[2], extent[3]);
        }
        return null;
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
        SetupClippingPane other = (SetupClippingPane) obj;
        if (!Arrays.equals(extent, other.extent))
            return false;
        return true;
    }

}
