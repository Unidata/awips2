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

import java.util.Map;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * {@link IImagingExtension} used to create and draw multi-channel R,G,B images
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IMultiChannelImageExtension extends IImagingExtension {

    public static enum Channel {
        RED, GREEN, BLUE;
    }

    public static class ChannelData {
        public String name;

        public ColorMapParameters parameters;

        public boolean invert;

        public ChannelData(String name, ColorMapParameters params,
                boolean invert) {
            this.name = name;
            this.parameters = params;
        }
    }

    public static interface IMultiChannelImage extends IImage {

        public Map<Channel, IImageChannel> getImageMapping();

        public void setImageMapping(Map<Channel, IImageChannel> mapping);

    }

    public static interface IImageChannel extends IColormappedImage {

        public void setInverted(boolean inverted);

    }

    /**
     * Construct a true color image, this
     * 
     * @param channelMap
     * @return
     * @throws VizException
     */
    public IMultiChannelImage constructImage(
            Map<Channel, IImageChannel> imageMapping) throws VizException;

    /**
     * Construct a IImageChannel for a IMultiChannelImage
     * 
     * @param callback
     * @param inverted
     * @return
     * @throws VizException
     */
    public IImageChannel constructImage(
            IColorMapDataRetrievalCallback callback, ChannelData channelData)
            throws VizException;

}
