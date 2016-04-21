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
package com.raytheon.uf.viz.truecolor.extension;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Imaging extension for "true color" images. Where a set of
 * {@link IColormappedImage} images are assigned to each band of an RGB channel
 * image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface ITrueColorImagingExtension extends IImagingExtension {

    public static enum Channel {
        RED, GREEN, BLUE;
    }

    public static interface ITrueColorImage extends IImage {

        public void setImages(Channel channel, DrawableImage... images);

        public DrawableImage[] getImages(Channel channel);

        public void setSize(int[] bounds);

        public void setImageExtent(IExtent extent);

    }

    /**
     * Creates a true color image with the given imageBounds and imageExtent
     * 
     * @param imageBounds
     * @param imageExtent
     * @param params
     * @return
     * @throws VizException
     */
    public ITrueColorImage initializeRaster(int[] imageBounds,
            IExtent imageExtent) throws VizException;
}
