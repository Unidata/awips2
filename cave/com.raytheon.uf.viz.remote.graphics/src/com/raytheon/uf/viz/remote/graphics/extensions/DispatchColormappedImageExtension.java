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
package com.raytheon.uf.viz.remote.graphics.extensions;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingColormappedImage;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingColormappedImage.DispatchingColormappedCallback;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchColormappedImageExtension extends
        AbstractDispatchingImageExtension implements IColormappedImageExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension
     * #
     * initializeRaster(com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback
     * , com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        IColormappedImageExtension targetExt;
        try {
            targetExt = target.getWrappedObject().getExtension(
                    IColormappedImageExtension.class);
        } catch (VizException e) {
            throw new RuntimeException(
                    "Could not get IColormappedImageExtension from targetObject",
                    e);
        }
        DispatchingColormappedCallback wrapper = new DispatchingColormappedCallback(
                dataCallback);
        IColormappedImage actualImage = targetExt.initializeRaster(wrapper,
                colorMapParameters);
        return new DispatchingColormappedImage(actualImage, wrapper,
                target.getDispatcher());
    }

}
