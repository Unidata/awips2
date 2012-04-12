package com.raytheon.viz.core.gl.ext;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.util.Stack;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLByteDataFormat;
import com.raytheon.viz.core.gl.dataformat.IGLColorMapDataFormatProvider;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.internal.GLView2D;
import com.raytheon.viz.core.gl.internal.ext.GLColormappedImageExtension;

public class GLOffscreenRenderingExtension extends GraphicsExtension<IGLTarget>
        implements IOffscreenRenderingExtension {

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * renderOffscreen(com.raytheon.uf.viz.core.drawables.IImage)
     */
    @Override
    public void renderOffscreen(IImage offscreenImage) throws VizException {
        renderOffscreen(offscreenImage, target.getView().getExtent());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * renderOffscreen(com.raytheon.uf.viz.core.drawables.IImage,
     * com.raytheon.uf.viz.core.IExtent)
     */
    @Override
    public void renderOffscreen(IImage offscreenImage, IExtent offscreenExtent)
            throws VizException {
        if (!(offscreenImage instanceof AbstractGLImage)) {
            throw new VizException(
                    "Can only use GLImages as offscreen frameBuffer on GLTarget");
        }
        AbstractGLImage glImage = (AbstractGLImage) offscreenImage;
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

    @Override
    public void renderOnscreen() throws VizException {
        if (viewStack.size() > 0) {
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(int[])
     */
    @Override
    public IImage constructOffscreenImage(final int[] dimensions)
            throws VizException {
        return target.initializeRaster(new IRenderedImageCallback() {
            @Override
            public RenderedImage getImage() throws VizException {
                return new BufferedImage(dimensions[0], dimensions[1],
                        BufferedImage.TYPE_INT_RGB);
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(java.lang.Class, java.awt.Rectangle)
     */
    @Override
    public AbstractGLImage constructOffscreenImage(
            Class<? extends Buffer> dataType, int[] dimensions)
            throws VizException {
        return constructOffscreenImage(dataType, dimensions, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(java.lang.Class, java.awt.Rectangle)
     */
    @Override
    public AbstractGLImage constructOffscreenImage(
            Class<? extends Buffer> dataType, final int[] dimensions,
            ColorMapParameters parameters) throws VizException {
        int width = dimensions[0];
        int height = dimensions[1];
        // Need to add support for multiple buffer types
        Buffer imageBuffer = null;
        if (dataType.isAssignableFrom(ByteBuffer.class)) {
            int pixels = 3;
            if (supportsLuminance) {
                pixels = 1;
            }
            byte[] buf = new byte[width * height * pixels];
            imageBuffer = ByteBuffer.wrap(buf);
        }

        if (imageBuffer != null) {
            AbstractGLImage image = null;
            final Buffer buffer = imageBuffer;
            GLColormappedImageExtension cmapExt = target
                    .getExtension(GLColormappedImageExtension.class);
            if (supportsLuminance) {
                image = cmapExt.initializeRaster(
                        new IColorMapDataRetrievalCallback() {

                            @Override
                            public ColorMapData getColorMapData()
                                    throws VizException {
                                return new ColorMapData(buffer, dimensions);
                            }
                        }, parameters);
            } else {
                image = cmapExt.initializeRaster(new GLOffscreenDataCallback(
                        buffer, dimensions), parameters);
            }
            if (!checkedLuminance) {
                checkedLuminance = true;
                try {
                    renderOffscreen(image);
                    renderOnscreen();
                } catch (VizException e) {
                    // assume we don't support luminance
                    supportsLuminance = false;
                    // Reconstruct image
                    image = constructOffscreenImage(dataType, dimensions,
                            parameters);
                }
            }
            return image;
        } else {
            return null;
        }
    }

    private static final class GLOffscreenDataCallback implements
            IColorMapDataRetrievalCallback, IGLColorMapDataFormatProvider {

        private Buffer dataBuffer;

        private int[] dimensions;

        private GLOffscreenDataCallback(Buffer dataBuffer, int[] dimensions) {
            this.dataBuffer = dataBuffer;
            this.dimensions = dimensions;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.core.gl.dataprep.IGLColorMapDataRetrievalCallback
         * #getGLColorMapData
         * (com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback
         * .ColorMapData)
         */
        @Override
        public AbstractGLColorMapDataFormat getGLColorMapDataFormat(
                ColorMapData colorMapData) {
            return new GLByteDataFormat() {

                /*
                 * (non-Javadoc)
                 * 
                 * @see com.raytheon.viz.core.gl.dataprep.GLByteDataFormat#
                 * getTextureInternalFormat()
                 */
                @Override
                public int getTextureInternalFormat() {
                    return GL.GL_RGB8;
                }

                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat
                 * #getTextureFormat()
                 */
                @Override
                public int getTextureFormat() {
                    return GL.GL_RGB;
                }

                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat
                 * #getPointsPerPixel()
                 */
                @Override
                public int getValuesPerPixel() {
                    return 3;
                }

            };
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback#
         * getColorMapData()
         */
        @Override
        public ColorMapData getColorMapData() throws VizException {
            return new ColorMapData(dataBuffer, dimensions);
        }
    }

}
