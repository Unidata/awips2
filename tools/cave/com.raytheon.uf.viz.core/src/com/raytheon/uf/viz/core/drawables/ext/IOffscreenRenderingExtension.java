package com.raytheon.uf.viz.core.drawables.ext;

import java.nio.Buffer;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;

public interface IOffscreenRenderingExtension {
    /**
     * All drawing between a call to renderOffscreen and the next call to
     * renderOnscreen will be drawn to offscreenImage rather than to the screen.
     * 
     * @param offscreenImage
     *            image to render to
     * @throws VizException
     */
    public void renderOffscreen(IImage offscreenImage) throws VizException;

    /**
     * Reset rendering to the screen. This only needs to be called if
     * renderOffscreen has been called.
     * 
     * @throws VizException
     */
    public void renderOnscreen() throws VizException;

    /**
     * Construct an offscreen image for given Buffer type and size
     * 
     * @param dataType
     * @param dataBounds
     * @return
     * @throws VizException
     */
    public IImage constructOffscreenImage(Class<? extends Buffer> dataType,
            int[] dimensions) throws VizException;

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
    public IImage constructOffscreenImage(Class<? extends Buffer> dataType,
            int[] dimensions, ColorMapParameters parameters)
            throws VizException;
}
