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
package com.raytheon.viz.core.gl.ext;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.util.Stack;

import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLByteDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLColorMapData;
import com.raytheon.viz.core.gl.dataformat.GLColorMapDataFormatFactory;
import com.raytheon.viz.core.gl.ext.imaging.GLColormappedImageExtension;
import com.raytheon.viz.core.gl.ext.imaging.GLDefaultImagingExtension;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLImage;
import com.raytheon.viz.core.gl.images.GLOffscreenColormappedImage;
import com.raytheon.viz.core.gl.internal.GLView2D;

/**
 * 
 * Provides logic for creating offscreen images in GL so that data can be
 * rendered and saved for better efficiency. When rendering offscreen all render
 * events to the target are sent to an offscreen image, this image can later be
 * rendered onscreen.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2012            bsteffen     Initial creation
 * Mar 21, 2013 1806       bsteffen    Update GL mosaicing to use dynamic data
 *                                     format for offscreen textures.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLOffscreenRenderingExtension extends GraphicsExtension<IGLTarget>
        implements IGraphicsExtensionInterface {

    private static class ViewInfo {
        private IView view;

        private AbstractGLImage image;

        /**
         * @param extent
         * @param bounds
         */
        public ViewInfo(IView view, AbstractGLImage image) {
            this.view = view;
            this.image = image;
        }
    }

    private static boolean checkedLuminance = false;

    private static boolean supportsLuminance = true;

    private Stack<ViewInfo> viewStack = new Stack<ViewInfo>();

    private ViewInfo currentInfo = null;

    /**
     * Begins offscreen rendering to the glImage. All rendering after this call
     * will occur on the image until {@link #endOffscreenRendering()} is called.
     * Render area will be set to the current view's extent
     * 
     * @param glImage
     * @throws VizException
     */
    public void beginOffscreenRendering(AbstractGLImage glImage)
            throws VizException {
        beginOffscreenRendering(glImage, target.getView().getExtent());
    }

    /**
     * Begins offscreen rendering to the glImage. All rendering after this call
     * will occur on the image until {@link #endOffscreenRendering()} is called.
     * Render area will be set to the offscreenExtent provided
     * 
     * @param glImage
     * @throws VizException
     */
    public void beginOffscreenRendering(AbstractGLImage glImage,
            IExtent offscreenExtent) throws VizException {
        if (glImage.getStatus() == IImage.Status.UNLOADED
                || glImage.getStatus() == IImage.Status.LOADING) {
            glImage.setStatus(IImage.Status.LOADING);
            glImage.stage();
        }

        if (glImage.getStatus() == IImage.Status.STAGED) {
            glImage.target(target);
        }

        IView view = new GLView2D(offscreenExtent);

        if (currentInfo == null) {
            // Use null for image so we use canvas when we pop
            viewStack.push(new ViewInfo(target.getView(), null));
        } else {
            viewStack.push(currentInfo);
        }
        setCurrentView(new ViewInfo(view, glImage));
    }

    /**
     * Ends offscreen rendering. Should only be called if a call to
     * {@link #beginOffscreenRendering(AbstractGLImage)} or
     * {@link #beginOffscreenRendering(AbstractGLImage, IExtent)} was
     * successfully called first first
     * 
     * @throws VizException
     */
    public void endOffscreenRendering() throws VizException {
        if (viewStack.isEmpty() == false) {
            setCurrentView(viewStack.pop());
        }
    }

    private void setCurrentView(ViewInfo current) throws VizException {
        currentInfo = current;
        Rectangle bounds = null;
        if (currentInfo.image == null) {
            // Null bounds means use current targets bounds
            bounds = new Rectangle(target.getBounds().width,
                    target.getBounds().height);
            // Set currentInfo to null since we are using screen/display info
            // and we don't want to cache old info
            currentInfo = null;
            target.getGl().glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT, 0);
        } else {
            bounds = new Rectangle(currentInfo.image.getWidth(),
                    currentInfo.image.getHeight());
            currentInfo.image.usaAsFrameBuffer();
        }

        target.setView(current.view);
        target.getGl().glViewport(0, 0, bounds.width, bounds.height);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IGraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(IGLTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

    /**
     * Creates an RGB based image for use in offscreen rendering
     * 
     * @param dimensions
     * @return
     * @throws VizException
     */
    public AbstractGLImage constructOffscreenImage(final int[] dimensions)
            throws VizException {
        return new GLImage(new IRenderedImageCallback() {
            @Override
            public RenderedImage getImage() throws VizException {
                return new BufferedImage(dimensions[0], dimensions[1],
                        BufferedImage.TYPE_INT_RGB);
            }
        }, GLDefaultImagingExtension.class);
    }

    /**
     * Creates a colormapped offscreen image with the specified dataType and
     * dimensions
     * 
     * @param dataType
     * @param dimensions
     * @return
     * @throws VizException
     */
    public GLOffscreenColormappedImage constructOffscreenImage(
            ColorMapDataType dataType, int[] dimensions) throws VizException {
        return constructOffscreenImage(dataType, dimensions, null);
    }

    /**
     * Creates a colormapped offscreen image with the specified dataType and
     * dimensions and default colormap parameters
     * 
     * @param dataType
     * @param dimensions
     * @param parameters
     * @return
     * @throws VizException
     */
    public GLOffscreenColormappedImage constructOffscreenImage(
            final ColorMapDataType dataType, final int[] dimensions,
            ColorMapParameters parameters) throws VizException {
        AbstractGLColorMapDataFormat format = null;

        if (supportsLuminance) {
            format = GLColorMapDataFormatFactory
                    .getGLColorMapDataFormat(dataType);
        } else {
            format = new NoLuminanceDataFormat(dataType);
        }

        GLOffscreenColormappedImage image = new GLOffscreenColormappedImage(
                new GLColorMapData(format, dataType, dimensions), parameters,
                GLColormappedImageExtension.class);

        if (!checkedLuminance) {
            checkedLuminance = true;
            try {
                beginOffscreenRendering(image);
            } catch (VizException e) {
                // Log this so it is easy to see in the console logs.
                new VizException(
                        "Graphics card does not support luminance textures.", e)
                        .printStackTrace(System.out);
                // assume we don't support luminance
                supportsLuminance = false;
                // Reconstruct image
                image = constructOffscreenImage(dataType, dimensions,
                        parameters);
            } finally {
                endOffscreenRendering();
            }
        }

        return image;
    }

    private static final class NoLuminanceDataFormat extends GLByteDataFormat {

        // Used to get the original min/max which makes signed bytes work and
        // theoretically will give better looking results for other integer data
        // types.
        private final AbstractGLColorMapDataFormat originalFormat;

        private NoLuminanceDataFormat(ColorMapDataType originalType) {
            this.originalFormat = GLColorMapDataFormatFactory
                    .getGLColorMapDataFormat(originalType);
        }

        @Override
        public int getTextureInternalFormat() {
            return GL.GL_RGB8;
        }

        @Override
        public int getTextureFormat() {
            return GL.GL_RGB;
        }

        @Override
        public int getValuesPerPixel() {
            return 3;
        }

        @Override
        public double getDataFormatMin() {
            return originalFormat.getDataFormatMin();
        }

        @Override
        public double getDataFormatMax() {
            return originalFormat.getDataFormatMax();
        }

    }

}
