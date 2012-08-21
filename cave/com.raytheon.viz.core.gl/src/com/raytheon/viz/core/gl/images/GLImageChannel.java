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
package com.raytheon.viz.core.gl.images;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.ChannelData;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.IImageChannel;
import com.raytheon.viz.core.gl.internal.ext.GLMultiChannelImageExtension;

/**
 * GL implementation of {@link IImageChannel}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLImageChannel extends GLDelegateImage<GLColormappedImage>
        implements IImageChannel {

    private ChannelData channelData;

    /**
     * @param target
     * @param image
     * @param extensionClass
     */
    public GLImageChannel(GLColormappedImage image, ChannelData channelData) {
        super(image, GLMultiChannelImageExtension.class);
        this.channelData = channelData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IColormappedImage#getColorMapParameters
     * ()
     */
    @Override
    public ColorMapParameters getColorMapParameters() {
        return image.getColorMapParameters();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IColormappedImage#setColorMapParameters
     * (com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public void setColorMapParameters(ColorMapParameters params) {
        image.setColorMapParameters(params);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IColormappedImage#getValue(int,
     * int)
     */
    @Override
    public double getValue(int x, int y) {
        return image.getValue(x, y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension
     * .IChannelImage#setInvert(boolean)
     */
    @Override
    public void setInverted(boolean inverted) {
        channelData.invert = inverted;
    }

    /**
     * @return the invert
     */
    public boolean isInverted() {
        return channelData.invert;
    }

}
