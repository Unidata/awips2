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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;

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
public class CreateColormappedImageEvent extends AbstractDispatchingObjectEvent
        implements ICreationEvent {

    @DynamicSerializeElement
    private UpdateColorMapParametersEvent colorMapParameters;

    @DynamicSerializeElement
    private UpdateColorMapEvent colorMap;

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

    /**
     * @return the colorMap
     */
    public UpdateColorMapEvent getColorMap() {
        return colorMap;
    }

    /**
     * @param colorMap
     *            the colorMap to set
     */
    public void setColorMap(UpdateColorMapEvent colorMap) {
        this.colorMap = colorMap;
    }

}