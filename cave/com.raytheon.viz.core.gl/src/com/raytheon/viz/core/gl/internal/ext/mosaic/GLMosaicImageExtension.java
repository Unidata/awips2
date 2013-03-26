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
package com.raytheon.viz.core.gl.internal.ext.mosaic;

import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapDataType;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IImage.Status;
import com.raytheon.uf.viz.core.drawables.ImagingSupport;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.ext.GLOffscreenRenderingExtension;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLColormappedImage;

/**
 * Extension used for rendering radar mosaic images
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2011            mschenke     Initial creation
 * Mar 21, 2013 1806       bsteffen    Update GL mosaicing to use dynamic data
 *                                     format for offscreen textures.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLMosaicImageExtension extends AbstractGLSLImagingExtension
        implements IMosaicImageExtension {

    private GLColormappedImage writeToImage;

    public GLMosaicImage initializeRaster(int[] imageBounds,
            IExtent imageExtent, ColorMapParameters params) throws VizException {
        // Since byte is the most common type of mosaic start with a byte image. It might switch later if needed.
        return new GLMosaicImage(target.getExtension(
                GLOffscreenRenderingExtension.class).constructOffscreenImage(
                ColorMapDataType.BYTE, imageBounds, params), imageBounds,
                imageExtent, this.getClass());
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
        return "mosaicOrdered";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.ext.AbstractGLImagingExtension#preImageRender
     * (com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.viz.core.gl.images.AbstractGLImage)
     */
    @Override
    public synchronized Object preImageRender(PaintProperties paintProps,
            AbstractGLImage image, PixelCoverage coverage) throws VizException {
        if (image instanceof GLMosaicImage) {
            GLMosaicImage mosaicImage = (GLMosaicImage) image;
            if (mosaicImage.isRepaint()) {
                writeToImage = getWriteToImage(mosaicImage);
                GLOffscreenRenderingExtension extension = target
                        .getExtension(GLOffscreenRenderingExtension.class);
                try {
                    extension.renderOffscreen(mosaicImage,
                            mosaicImage.getImageExtent());
                    DrawableImage[] imagesToMosaic = mosaicImage
                            .getImagesToMosaic();
                    // Make sure images are staged before we mosaic them
                    ImagingSupport.prepareImages(target, imagesToMosaic);

                    boolean allPainted = true;
                    // Each image needs to draw separately due to gl issues when
                    // zoomed in very far, rendered parts near the corners don't
                    // show all the pixels for each image. Pushing and popping
                    // GL_TEXTURE_BIT before/after each render fixes this issue
                    for (DrawableImage di : imagesToMosaic) {
                        allPainted &= drawRasters(paintProps, di);
                    }
                    // Need to set repaint based on if drawing completed.
                    mosaicImage.setRepaint(allPainted == false);
                } finally {
                    extension.renderOnscreen();
                }
                writeToImage = null;
            }

            target.drawRasters(paintProps,
                    new DrawableImage(mosaicImage.getWrappedImage(), coverage));
            // Don't actually render this image now since we just did it
            return null;
        } else {
            GL gl = target.getGl();
            // activate on texture2 as 0 is radar image and 1 is colormap
            gl.glActiveTexture(GL.GL_TEXTURE1);
            gl.glBindTexture(writeToImage.getTextureStorageType(),
                    writeToImage.getTextureid());
            return image;
        }
    }

    private GLColormappedImage getWriteToImage(GLMosaicImage mosaicImage)
            throws VizException {
        ColorMapDataType neededType = null;
        for (DrawableImage di : mosaicImage.getImagesToMosaic()) {
            IImage image = di.getImage();
            if (image.getStatus() != Status.LOADED) {
                continue;
            }
            if (image instanceof GLColormappedImage) {
                GLColormappedImage colorMapImage = (GLColormappedImage) image;
                ColorMapDataType type = colorMapImage.getColorMapDataType();
                if (neededType == null) {
                    neededType = type;
                } else if (neededType != type) {
                    // Mosaicing images of different types?
                    // No Idea how to handle this
                    return mosaicImage.getWrappedImage();
                }
            }
        }
        GLColormappedImage writeTo = mosaicImage.getWrappedImage();
        if (neededType != null && neededType != writeTo.getColorMapDataType()) {
            GLOffscreenRenderingExtension offscreenExt = target
                    .getExtension(GLOffscreenRenderingExtension.class);
            int[] dimensions = { writeTo.getWidth(), writeTo.getHeight() };
            writeTo = offscreenExt.constructOffscreenImage(neededType,
                    dimensions, writeTo.getColorMapParameters());
            mosaicImage.setWrappedImage(writeTo);
        }
        return writeTo;
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
        // activate on texture2 as 0 is radar image
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
        program.setUniform("imageData", 0);
        program.setUniform("mosaicTexture", 1);

        // pass in width and height
        program.setUniform("height", writeToImage.getHeight());
        program.setUniform("width", writeToImage.getWidth());
    }

}
