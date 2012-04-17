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
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;

/**
 * Event for creating a new mosaic image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class CreateMosaicImageEvent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private int[] bounds;

    @DynamicSerializeElement
    private UpdateMosaicExtent extent;

    @DynamicSerializeElement
    private UpdateColorMapParametersEvent colorMapParameters;

    /**
     * @return the bounds
     */
    public int[] getBounds() {
        return bounds;
    }

    /**
     * @param bounds
     *            the bounds to set
     */
    public void setBounds(int[] bounds) {
        this.bounds = bounds;
    }

    /**
     * @return the extent
     */
    public UpdateMosaicExtent getExtent() {
        return extent;
    }

    /**
     * @param extent
     *            the extent to set
     */
    public void setExtent(UpdateMosaicExtent extent) {
        this.extent = extent;
    }

    /**
     * @return the colorMapParameters
     */
    public UpdateColorMapParametersEvent getColorMapParameters() {
        return colorMapParameters;
    }

    /**
     * @param colorMapParameters
     *            the colorMapParameters to set
     */
    public void setColorMapParameters(
            UpdateColorMapParametersEvent colorMapParameters) {
        this.colorMapParameters = colorMapParameters;
    }

}
