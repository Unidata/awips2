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
package com.raytheon.uf.viz.core.drawables.ext.colormap;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;

/**
 * General colormapped image extension. Uses
 * {@link ColormappedRenderedImageCallback} to construct RenderedImages
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GeneralColormappedImageExtension extends
        GraphicsExtension<IGraphicsTarget> implements
        IColormappedImageExtension {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IColormappedImageExtension#
     * initializeRaster
     * (com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IImage initializeRaster(IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        return target.initializeRaster(new ColormappedRenderedImageCallback(
                dataCallback, colorMapParameters));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(IGraphicsTarget target) {
        return Compatibilty.GENERIC.value;
    }

}
