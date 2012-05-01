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

import java.awt.image.RenderedImage;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.data.prep.Colormapper;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * General {@link IRenderedImageCallback} that takes a
 * {@link IColorMapDataRetrievalCallback} and {@link ColorMapParameters} and
 * will colormap the data returned from the callback into a
 * {@link RenderedImage}
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

public class ColormappedRenderedImageCallback implements IRenderedImageCallback {

    private IColorMapDataRetrievalCallback callback;

    private ColorMapParameters parameters;

    /**
     * Construct a ColormappedRenderedImageCallback for the colormap data
     * callback and parameters
     * 
     * @param dataCallback
     * @param parameters
     */
    public ColormappedRenderedImageCallback(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters parameters) {
        this.callback = dataCallback;
        this.parameters = parameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.data.IRenderedImageCallback#getImage()
     */
    @Override
    public RenderedImage getImage() throws VizException {
        ColorMapData colorMapData = callback.getColorMapData();
        return Colormapper.colorMap(colorMapData, parameters);
    }
}
