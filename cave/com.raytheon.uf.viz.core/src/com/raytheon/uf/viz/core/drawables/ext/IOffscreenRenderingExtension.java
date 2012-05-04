package com.raytheon.uf.viz.core.drawables.ext;

import java.nio.Buffer;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;

public interface IOffscreenRenderingExtension extends
        IGraphicsExtensionInterface {
    /**
     * All drawing between a call to renderOffscreen and the next call to
     * renderOnscreen will be drawn to offscreenImage rather than to the screen.
     * Will use the current screen's world as the coverage area for the
     * offscreen image
     * 
     * @param offscreenImage
     *            image to render to
     * @throws VizException
     */
    public void renderOffscreen(IImage offscreenImage) throws VizException;

    /**
     * All drawing between a call to renderOffscreen and the next call to
     * renderOnscreen will be drawn to offscreenImage rather than to the screen.
     * Will use the extent passed in as the world coverage area for the
     * offscreen image
     * 
     * @param offscreenImage
     * @param offscreenExtent
     * @throws VizException
     */
    public void renderOffscreen(IImage offscreenImage, IExtent offscreenExtent)
            throws VizException;

    /**
     * Reset rendering to the screen. This only needs to be called if
     * renderOffscreen has been called.
     * 
     * @throws VizException
     */
    public void renderOnscreen() throws VizException;

    /**
     * Construct an offscreen image with the specified dimensions. This image
     * will be an RGB based image
     * 
     * @param dimensions
     * @return
     * @throws VizException
     */
    public IImage constructOffscreenImage(int[] dimensions) throws VizException;

    /**
     * Construct an offscreen image for given Buffer type and size
     * 
     * @param dataType
     * @param dataBounds
     * @return
     * @throws VizException
     */
    public IColormappedImage constructOffscreenImage(
            Class<? extends Buffer> dataType, int[] dimensions)
            throws VizException;

    /**
     * Construct an offscreen image for given Buffer type and size, applying
     * ColorMapParameters to offscreen image where possible
     * 
     * @param dataType
     * @param dataBounds
     * @param parameters
     * @return
     * @throws VizException
     */
    public IColormappedImage constructOffscreenImage(
            Class<? extends Buffer> dataType, int[] dimensions,
            ColorMapParameters parameters) throws VizException;
}
