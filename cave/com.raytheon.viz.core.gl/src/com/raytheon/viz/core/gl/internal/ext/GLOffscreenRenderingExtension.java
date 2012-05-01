package com.raytheon.viz.core.gl.internal.ext;

import java.nio.Buffer;
import java.nio.ByteBuffer;

import javax.media.opengl.GL;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLByteDataFormat;
import com.raytheon.viz.core.gl.dataformat.IGLColorMapDataFormatProvider;
import com.raytheon.viz.core.gl.images.AbstractGLImage;

public class GLOffscreenRenderingExtension extends GraphicsExtension<IGLTarget>
        implements IOffscreenRenderingExtension {

    private static boolean checkedLuminance = false;

    private static boolean supportsLuminance = true;

    @Override
    public void renderOffscreen(IImage offscreenImage) throws VizException {
        if (!(offscreenImage instanceof AbstractGLImage)) {
            throw new VizException(
                    "Can only use GLImages as offscreen frameBuffer on GLTarget");
        }
        AbstractGLImage glImage = (AbstractGLImage) offscreenImage;
        if (glImage.getStatus() == IImage.Status.UNLOADED
                || glImage.getStatus() == IImage.Status.LOADING) {
            glImage.setStatus(IImage.Status.LOADING);
            glImage.stageTexture();
        }

        if (glImage.getStatus() == IImage.Status.STAGED) {
            glImage.target(target);
        }
        target.getGl()
                .glViewport(0, 0, glImage.getWidth(), glImage.getHeight());
        glImage.usaAsFrameBuffer();
    }

    @Override
    public void renderOnscreen() throws VizException {
        Rectangle canvasSize = target.getBounds();
        target.getGl().glViewport(0, 0, canvasSize.width, canvasSize.height);
        target.getGl().glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT, 0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IGraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(IGLTarget target) {
        return Compatibilty.TARGET_COMPATIBLE.value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(java.lang.Class, java.awt.Rectangle)
     */
    @Override
    public IImage constructOffscreenImage(Class<? extends Buffer> dataType,
            int[] dimensions) throws VizException {
        return constructOffscreenImage(dataType, dimensions, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(java.lang.Class, java.awt.Rectangle)
     */
    @Override
    public IImage constructOffscreenImage(Class<? extends Buffer> dataType,
            final int[] dimensions, ColorMapParameters parameters)
            throws VizException {
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
            IImage image = null;
            final Buffer buffer = imageBuffer;
            IColormappedImageExtension cmapExt = target
                    .getExtension(IColormappedImageExtension.class);
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
