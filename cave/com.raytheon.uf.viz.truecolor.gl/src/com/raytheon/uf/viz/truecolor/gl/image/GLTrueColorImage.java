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
package com.raytheon.uf.viz.truecolor.gl.image;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.ITrueColorImage;
import com.raytheon.viz.core.gl.images.GLDelegateImage;
import com.raytheon.viz.core.gl.images.GLImage;

/**
 * TODO Add Description
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

public class GLTrueColorImage extends GLDelegateImage<GLImage> implements
        ITrueColorImage {

    private static class RGBCallback implements IRenderedImageCallback {
        private int[] bounds;

        private RGBCallback(int[] bounds) {
            this.bounds = bounds;
        }

        @Override
        public RenderedImage getImage() throws VizException {
            return new BufferedImage(bounds[0], bounds[1],
                    BufferedImage.TYPE_INT_ARGB);
        }
    }

    private boolean repaint = true;

    private int[] bounds;

    private IExtent imageExtent;

    private Map<Channel, DrawableImage[]> channelMap = new HashMap<Channel, DrawableImage[]>();

    /**
     * @param extensionClass
     */
    public GLTrueColorImage(Class<? extends IImagingExtension> extensionClass,
            int[] bounds, IExtent imageExtent) {
        super(new GLImage(new RGBCallback(bounds), IImagingExtension.class),
                extensionClass);
        this.bounds = bounds;
        this.imageExtent = imageExtent;
    }

    /**
     * @return the imageExtent
     */
    public IExtent getImageExtent() {
        return imageExtent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.
     * ITrueColorImage
     * #setImages(com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension
     * .Channel, com.raytheon.uf.viz.core.DrawableImage[])
     */
    @Override
    public void setImages(Channel channel, DrawableImage... images) {
        DrawableImage[] prev = channelMap.put(channel, images);
        if (prev != images) {
            // Try to find equal array
            if (prev != null && images != null && prev.length == images.length) {
                for (int i = 0; i < images.length; ++i) {
                    if (images[i].getImage() != prev[i].getImage()) {
                        repaint = true;
                        break;
                    }
                }
            } else {
                repaint = true;
            }
        }
    }

    /**
     * Get the images for the specified channel
     * 
     * @param channel
     * @return
     */
    public DrawableImage[] getImages(Channel channel) {
        return channelMap.get(channel);
    }

    /**
     * @return the repaint
     */
    public boolean isRepaint() {
        return repaint;
    }

    /**
     * @param repaint
     *            the repaint to set
     */
    public void setRepaint(boolean repaint) {
        this.repaint = repaint;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.
     * ITrueColorImage#setSize(int[])
     */
    @Override
    public void setSize(int[] bounds) {
        if (Arrays.equals(bounds, this.bounds) == false) {
            this.bounds = bounds;
            image.dispose();
            image = new GLImage(new RGBCallback(bounds),
                    IImagingExtension.class);
            repaint = true;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.
     * ITrueColorImage#setImageExtent(com.raytheon.uf.viz.core.IExtent)
     */
    @Override
    public void setImageExtent(IExtent extent) {
        if (extent.equals(this.imageExtent) == false) {
            this.imageExtent = extent;
            repaint = true;
        }
    }

}
