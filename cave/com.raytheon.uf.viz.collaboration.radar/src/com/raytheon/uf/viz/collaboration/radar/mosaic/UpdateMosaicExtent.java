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
package com.raytheon.uf.viz.collaboration.radar.mosaic;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;

/**
 * Event object for updating mosaic images extent
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class UpdateMosaicExtent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private double[] extent;

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
        return new PixelExtent(extent[0], extent[1], extent[2], extent[3]);
    }

}
