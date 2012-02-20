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
package com.raytheon.viz.core.gl;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.images.GLColormappedImage;

public interface IGLTarget extends IGraphicsTarget {

    public abstract GL getGl();

    public abstract GLU getGlu();

    /**
     * Active texture unit and bind a texture
     * 
     * @param textureUnit
     * @param GLImage
     */
    public abstract void bindTexture(int textureUnit, GLColormappedImage image);

    /**
     * 
     * @return
     */
    public abstract boolean makeContextCurrent();

    /**
     * 
     * @return
     */
    public abstract void releaseContext();

    /**
     * Get the modle view matrix settings
     * 
     * @return
     */
    public abstract double[] getModelView();

    /**
     * Get the projection matrix settings
     * 
     * @return
     */
    public abstract double[] getProjection();

    /**
     * Get the view port settings
     * 
     * @return
     */
    public abstract int[] getViewPort();

    /**
     * Convert pixel coordinates to window.
     * 
     * @param p
     * @return
     */
    public abstract double[] project(double[] p);

    /**
     * Convert screen x,y to pixel space
     * 
     * @param x
     *            screen x value
     * @param y
     *            screen y value
     * @param z
     *            0 near 1 far plane
     * @return pixel value
     */
    public abstract double[] unproject(double[] pos);

    /**
     * @return the bounds for the target pane.
     */
    public abstract Rectangle getBounds();

    /**
     * Dispose a vbo
     * 
     * @param id
     *            the vbo id
     */
    public abstract void disposeVBO(int id);

    /**
     * Dispose the occlusion query
     * 
     * @param id
     */
    public abstract void disposeOcclusionQueries(int[] id);

    /**
     * 
     * @param display
     * @throws VizException
     */
    public void beginOcclusionTest(IRenderableDisplay display)
            throws VizException;

    /**
     * End the occlusion test. Commands will update the framebuffer
     * 
     * @throws VizException
     */
    public void endOcclusionTest() throws VizException;

    /**
     * call drawRaster using a specified shader program
     * 
     * @param image
     * @param extent
     * @param paintProps
     * @param shaderProgram
     * @return
     * @throws VizException
     */
    public abstract boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps, String shaderProgram)
            throws VizException;

    /**
     * call drawRaster with a specified shader program
     * 
     * @param image
     *            the image reference object to draw
     * @param extent
     *            the extent of the drawable area
     * @param paintProps
     *            the paint properties
     * @param mode
     *            the drawing mode (synchronous, asynchronous)
     * @return status whether the raster was able to be drawn
     * @throws VizException
     */
    public abstract boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps, RasterMode mode, String shaderProgram)
            throws VizException;

    public abstract boolean drawRasters(String shader,
            PaintProperties paintProps, DrawableImage... images)
            throws VizException;

    /**
     * Checks the glError state and does a UFStatus message
     */
    public abstract void handleError(int errorid);

}
