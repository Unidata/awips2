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
package com.raytheon.uf.viz.remote.graphics.events.offscreen;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class CreateOffscreenImageEvent extends AbstractDispatchingObjectEvent
        implements ICreationEvent {

    @DynamicSerializeElement
    private String bufferType;

    @DynamicSerializeElement
    private int[] dimensions;

    @DynamicSerializeElement
    private UpdateColorMapParametersEvent colorMapParamters;

    /**
     * @return the bufferType
     */
    public String getBufferType() {
        return bufferType;
    }

    /**
     * @param bufferType
     *            the bufferType to set
     */
    public void setBufferType(String bufferType) {
        this.bufferType = bufferType;
    }

    /**
     * @return the dimensions
     */
    public int[] getDimensions() {
        return dimensions;
    }

    /**
     * @param dimensions
     *            the dimensions to set
     */
    public void setDimensions(int[] dimensions) {
        this.dimensions = dimensions;
    }

    /**
     * @return the colorMapParamters
     */
    public UpdateColorMapParametersEvent getColorMapParamters() {
        return colorMapParamters;
    }

    /**
     * @param colorMapParamters
     *            the colorMapParamters to set
     */
    public void setColorMapParamters(
            UpdateColorMapParametersEvent colorMapParamters) {
        this.colorMapParamters = colorMapParamters;
    }

}
