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

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ImagingSupport;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension;
import com.raytheon.uf.viz.truecolor.gl.image.GLTrueColorImage;
import com.raytheon.viz.core.gl.ext.GLOffscreenRenderingExtension;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLColormappedImage;

/**
 * GL implementation of the {@link ITrueColorImagingExtension}
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

public class GLTrueColorImagingExtension extends AbstractGLSLImagingExtension
        implements ITrueColorImagingExtension {

    private AbstractGLImage writeToImage;

    private Channel renderingChannel;

    private Map<ColorMapParameters, Object> parameters = new IdentityHashMap<ColorMapParameters, Object>();

    /**
     * Alpha value to add to current alpha. When all 3 passes occur, alpha
     * should be >= 1.0
     */
    private float alphaStep;

    /**
     * Flag if alpha check should be performed in shader. Alpha vals less than
     * one will be set to 0
     */
    private boolean checkAlpha;

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
                parameters.clear();
                writeToImage = trueColorImage;
                GLOffscreenRenderingExtension extension = target
                        .getExtension(GLOffscreenRenderingExtension.class);
                try {
                    extension.renderOffscreen(trueColorImage,
                            trueColorImage.getImageExtent());
                    boolean allPainted = true;
                    int channels = trueColorImage.getNumberOfChannels();
                    // Calculate our alpha step, ensure channels * alphaStep >=
                    // 1.0
                    alphaStep = 1.0f / (channels - 0.1f);
                    int i = 1;
                    for (Channel channel : Channel.values()) {
                        renderingChannel = channel;
                        DrawableImage[] imagesToDraw = trueColorImage
                                .getImages(channel);
                        if (imagesToDraw != null && imagesToDraw.length > 0) {
                            // Perform alpha check on last pass
                            checkAlpha = i == channels;
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
                            ++i;
                        }
                    }
                } finally {
                    extension.renderOnscreen();
                }
                renderingChannel = null;
                writeToImage = null;
            }

            trueColorImage.setImageParameters(parameters.keySet());
            target.drawRasters(paintProps,
                    new DrawableImage(trueColorImage.getWrappedImage(),
                            imageCoverage));
            // Don't actually render this image now since we just did it
            return null;
        } else {
            GL gl = target.getGl();
            // bind on GL_TEXTURE1 as 0 is channel image
            writeToImage.bind(gl, GL.GL_TEXTURE1);
            return image;
        }
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
        GL gl = target.getGl();
        // Unbind the writeToImage from GL_TEXTURE1
        gl.glActiveTexture(GL.GL_TEXTURE1);
        gl.glBindTexture(writeToImage.getTextureStorageType(), 0);
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
        if (image instanceof GLColormappedImage == false) {
            throw new VizException(
                    "Can only render colormapped images in true color");
        }

        GLColormappedImage cmapImage = (GLColormappedImage) image;
        ColorMapParameters colorMapParameters = cmapImage
                .getColorMapParameters();
        parameters.put(colorMapParameters, null);
        int textureType = cmapImage.getTextureType();

        // Set the band image data
        program.setUniform("rawTex", 0);
        program.setUniform("naturalMin", colorMapParameters.getDataMin());
        program.setUniform("naturalMax", colorMapParameters.getDataMax());
        program.setUniform("cmapMin", colorMapParameters.getColorMapMin());
        program.setUniform("cmapMax", colorMapParameters.getColorMapMax());
        program.setUniform("isFloat", textureType == GL.GL_FLOAT
                || textureType == GL.GL_HALF_FLOAT_ARB ? 1 : 0);
        program.setUniform("noDataValue", colorMapParameters.getNoDataValue());

        // Set the composite image data
        program.setUniform("trueColorTexture", 1);
        program.setUniform("width", writeToImage.getWidth());
        program.setUniform("height", writeToImage.getHeight());

        // Set the band we are rendering to
        program.setUniform("band", renderingChannel.ordinal());
        program.setUniform("checkAlpha", checkAlpha);
        program.setUniform("alphaStep", alphaStep);
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
