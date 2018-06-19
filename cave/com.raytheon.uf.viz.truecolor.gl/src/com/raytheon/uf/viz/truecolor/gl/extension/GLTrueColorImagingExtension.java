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
package com.raytheon.uf.viz.truecolor.gl.extension;

import java.util.IdentityHashMap;
import java.util.Map;

import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ImagingSupport;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension;
import com.raytheon.uf.viz.truecolor.gl.image.GLTrueColorImage;
import com.raytheon.viz.core.gl.ext.GLOffscreenRenderingExtension;
import com.raytheon.viz.core.gl.ext.imaging.GLColormappedImageExtension;
import com.raytheon.viz.core.gl.ext.imaging.GLDataMappingFactory.GLDataMapping;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLSLStructFactory;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLColormappedImage;
import com.raytheon.viz.core.gl.images.AbstractGLImage;

/**
 * GL implementation of the {@link ITrueColorImagingExtension}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug  6, 2012            mschenke    Initial creation
 * Nov  4, 2013 2492       mschenke    Reworked to use GLSL Data mapping
 * Jan 27, 2016 DR 17997   jgerth      Support for gamma control
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLTrueColorImagingExtension extends AbstractGLSLImagingExtension
        implements ITrueColorImagingExtension {

    private AbstractGLImage writeToImage;

    private Channel renderingChannel;

    private double renderingGamma = 1.0;

    private Map<ColorMapParameters, Object> parameters = new IdentityHashMap<ColorMapParameters, Object>();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension#
     * initializeRaster(int[], com.raytheon.uf.viz.core.IExtent)
     */
    @Override
    public ITrueColorImage initializeRaster(int[] imageBounds,
            IExtent imageExtent) throws VizException {
        return new GLTrueColorImage(GLTrueColorImagingExtension.class,
                imageBounds, imageExtent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#getShaderProgramName
     * ()
     */
    @Override
    public String getShaderProgramName() {
        return "truecolor";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#preImageRender
     * (com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.viz.core.gl.images.AbstractGLImage,
     * com.raytheon.uf.viz.core.PixelCoverage)
     */
    @Override
    public Object preImageRender(PaintProperties paintProps,
            AbstractGLImage image, PixelCoverage imageCoverage)
            throws VizException {
        if (image instanceof GLTrueColorImage) {
            GLTrueColorImage trueColorImage = (GLTrueColorImage) image;
            if (trueColorImage.isRepaint()) {
                // Reset current bit mask
                parameters.clear();
                writeToImage = trueColorImage;
                GLOffscreenRenderingExtension extension = target
                        .getExtension(GLOffscreenRenderingExtension.class);
                try {
                    extension.beginOffscreenRendering(trueColorImage,
                            trueColorImage.getImageExtent());
                    boolean allPainted = true;
                    for (Channel channel : Channel.values()) {
                        renderingChannel = channel;
                        renderingGamma = trueColorImage.getGamma(channel);
                        DrawableImage[] imagesToDraw = trueColorImage
                                .getImages(channel);
                        if (imagesToDraw != null && imagesToDraw.length > 0) {
                            // Make sure images are staged before we mosaic them
                            ImagingSupport.prepareImages(target, imagesToDraw);

                            // Each image needs to draw separately due to gl
                            // issues when zoomed in very far, rendered parts
                            // near the corners don't show all the pixels for
                            // each image. Pushing and popping GL_TEXTURE_BIT
                            // before/after each render fixes this issue
                            for (DrawableImage di : imagesToDraw) {
                                allPainted &= drawRasters(paintProps, di);
                            }
                            // Need to set repaint based on if drawing
                            // completed.
                            trueColorImage.setRepaint(allPainted == false);
                        }
                    }
                } catch (VizException e) {
                    // Exception: end rendering now instead of postImageRender
                    extension.endOffscreenRendering();
                    throw e;
                }
                renderingChannel = null;
                renderingGamma = 1.0;
                writeToImage = null;
                trueColorImage.setImageParameters(parameters.keySet());
                trueColorImage.bind(target.getGl());
                return imageCoverage;
            } else {
                target.drawRasters(paintProps,
                        new DrawableImage(trueColorImage.getWrappedImage(),
                                imageCoverage));
                return null;
            }
        } else if (image instanceof AbstractGLColormappedImage) {
            GL gl = target.getGl();

            GLColormappedImageExtension.setupDataMapping(gl,
                    (AbstractGLColormappedImage) image, GL.GL_TEXTURE2,
                    GL.GL_TEXTURE3);
            // bind on GL_TEXTURE1 as 0 is channel image
            writeToImage.bind(gl, GL.GL_TEXTURE1);
            return image;
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#postImageRender
     * (com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.viz.core.gl.images.AbstractGLImage, java.lang.Object)
     */
    @Override
    public void postImageRender(PaintProperties paintProps,
            AbstractGLImage image, Object data) throws VizException {
        if (image instanceof GLTrueColorImage) {
            target.getExtension(GLOffscreenRenderingExtension.class)
                    .endOffscreenRendering();
            target.drawRasters(paintProps, new DrawableImage(
                    ((GLTrueColorImage) image).getWrappedImage(),
                    (PixelCoverage) data));
        } else if (writeToImage != null) {
            GL gl = target.getGl();
            // Unbind the writeToImage from GL_TEXTURE1
            gl.glActiveTexture(GL.GL_TEXTURE1);
            gl.glBindTexture(writeToImage.getTextureStorageType(), 0);

            // Unbind the data mapped textures
            gl.glActiveTexture(GL.GL_TEXTURE2);
            gl.glBindTexture(GL.GL_TEXTURE_1D, 0);

            gl.glActiveTexture(GL.GL_TEXTURE3);
            gl.glBindTexture(GL.GL_TEXTURE_1D, 0);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#loadShaderData
     * (com.raytheon.viz.core.gl.glsl.GLShaderProgram,
     * com.raytheon.uf.viz.core.drawables.IImage,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void loadShaderData(GLShaderProgram program, IImage image,
            PaintProperties paintProps) throws VizException {
        if (image instanceof GLTrueColorImage) {
            GLTrueColorImage glImage = (GLTrueColorImage) image;
            program.setUniform("band", -1);
            program.setUniform("trueColorTexture", 0);
            program.setUniform("expectedMask", glImage.getColorMask());
        } else {
            if (image instanceof AbstractGLColormappedImage == false) {
                throw new VizException(
                        "Can only render colormapped images in true color");
            }

            AbstractGLColormappedImage cmapImage = (AbstractGLColormappedImage) image;
            ColorMapParameters colorMapParameters = cmapImage
                    .getColorMapParameters();

            parameters.put(colorMapParameters, null);

            GLSLStructFactory.createDataTexture(program, "rawData", 0,
                    cmapImage);

            int numMappingValues = 0;
            GLDataMapping mapping = cmapImage.getDataMapping();
            if (mapping != null && mapping.isValid()) {
                numMappingValues = mapping.getNumMappingValues();
            }
            GLSLStructFactory.createDataMapping(program, "dataMapping", 2, 3,
                    numMappingValues);

            GLSLStructFactory.createColorMapping(program, "colorMapping", -1,
                    -1, colorMapParameters);

            program.setUniform("gamma", renderingGamma);

            // Set the composite image data
            program.setUniform("trueColorTexture", 1);
            program.setUniform("width", (float) writeToImage.getWidth());
            program.setUniform("height", (float) writeToImage.getHeight());

            // Set the band we are rendering to
            program.setUniform("band", renderingChannel.ordinal());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#enableBlending
     * (javax.media.opengl.GL)
     */
    @Override
    protected void enableBlending(GL gl) {
        // Do not enable blending for this extension as it messes with alpha
        // values between passes
    }

}
