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
package com.raytheon.uf.viz.radar.gl.mosaic;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLDelegateImage;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension.IMosaicImage;

/**
 * GL implementation of IMosaicImage, wraps an offscreen image and contains
 * other DrawableImages to mosaic
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLMosaicImage extends GLDelegateImage<AbstractGLImage> implements
        IMosaicImage {

    private DrawableImage[] images;

    private boolean repaint;

    private int[] bounds;

    /**
     * @param target
     * @param image
     * @param extensionClass
     */
    public GLMosaicImage(AbstractGLImage image, int[] bounds) {
        super(image, GLRadarMosaicImageExtension.class);
        this.bounds = bounds;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.GLDelegateImage#getWidth()
     */
    @Override
    public int getWidth() {
        return bounds[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.GLDelegateImage#getHeight()
     */
    @Override
    public int getHeight() {
        return bounds[1];
    }

    /**
     * @return the repaint
     */
    public boolean isRepaint() {
        return repaint;
    }

    /**
     * @param repaint
     * @return
     */
    public void setRepaint(boolean repaint) {
        this.repaint = repaint;
    }

    public DrawableImage[] getImagesToMosaic() {
        return images;
    }

    public void setImagesToMosaic(DrawableImage... images) {
        this.images = images;
        repaint = true;
    }
}
