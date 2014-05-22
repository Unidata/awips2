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

import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ImagingSupport;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.ext.GLOffscreenRenderingExtension;
import com.raytheon.viz.core.gl.ext.imaging.GLColormappedImageExtension;
import com.raytheon.viz.core.gl.ext.imaging.GLDataMappingFactory.GLDataMapping;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLSLStructFactory;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLColormappedImage;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLOffscreenColormappedImage;

/**
 * Extension used for rendering radar mosaic images
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 16, 2011           mschenke    Initial creation
 * Mar 21, 2013  1806     bsteffen    Update GL mosaicing to use dynamic data
 *                                    format for offscreen textures.
 * Oct 16, 2013  2333     mschenke    Cleaned up render logic, switched to use 
 *                                    GLOffscreenColormappedImage
 * Nov 20, 2013  2492     bsteffen    Mosaic in image units.
 * Apr 08, 2014  2950     bsteffen    Always use float for maximum precision 
 *                                    offscreen so interpolation works.
 * Apr 18, 2014  2947     bsteffen    Fix mosaicing of datamapped images.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class GLMosaicImageExtension extends
        AbstractGLSLImagingExtension implements IMosaicImageExtension {

    private GLOffscreenColormappedImage writeToImage;

    @Override
    public GLMosaicImage initializeRaster(int[] imageBounds,
            IExtent imageExtent, ColorMapParameters params) throws VizException {
        // Since byte is the most common type of mosaic start with a byte image.
        // It might switch later if needed when images to mosaic are set
        return new GLMosaicImage(target.getExtension(
                GLOffscreenRenderingExtension.class).constructOffscreenImage(
                ColorMapDataType.FLOAT, imageBounds, params), imageBounds,
                imageExtent, this.getClass());
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
            boolean drawMosaic = true;
            if (mosaicImage.isRepaint()) {
                DrawableImage[] imagesToMosaic = mosaicImage
                        .getImagesToMosaic();
                // Make sure images are staged before we mosaic them
                ImagingSupport.prepareImages(target, imagesToMosaic);

                writeToImage = mosaicImage.getWrappedImage();
                if (writeToImage != null) {
                    GLOffscreenRenderingExtension extension = target
                            .getExtension(GLOffscreenRenderingExtension.class);
                    try {
                        extension.beginOffscreenRendering(mosaicImage,
                                mosaicImage.getImageExtent());

                        boolean allPainted = true;
                        // Each image needs to draw separately due to gl issues
                        // when zoomed in very far, rendered parts near the
                        // corners don't show all the pixels for each image.
                        // Pushing and popping GL_TEXTURE_BIT before/after each
                        // render fixes this issue
                        for (DrawableImage di : imagesToMosaic) {
                            allPainted &= drawRasters(paintProps, di);
                        }
                        // Need to set repaint based on if drawing completed.
                        mosaicImage.setRepaint(allPainted == false);
                    } finally {
                        extension.endOffscreenRendering();
                    }
                    writeToImage = null;
                } else {
                    drawMosaic = false;
                    mosaicImage.setRepaint(true);
                }
            }

            if (drawMosaic) {
                target.drawRasters(paintProps,
                        new DrawableImage(mosaicImage.getWrappedImage(),
                                coverage));
            }
        } else if (image instanceof AbstractGLColormappedImage) {
            GL gl = target.getGl();
            // activate on texture2 as 0 is radar image and 1 is colormap
            gl.glActiveTexture(GL.GL_TEXTURE1);
            gl.glBindTexture(writeToImage.getTextureStorageType(),
                    writeToImage.getTextureid());

            GLColormappedImageExtension.setupDataMapping(gl,
                    (AbstractGLColormappedImage) image,
                    writeToImage.getDataUnit(), GL.GL_TEXTURE2, GL.GL_TEXTURE3);
            return image;
        }
        // Fall through here, no actual rendering will occur
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
        GL gl = target.getGl();
        // activate on texture2 as 0 is radar image
        gl.glActiveTexture(GL.GL_TEXTURE1);
        gl.glBindTexture(writeToImage.getTextureStorageType(), 0);

        gl.glActiveTexture(GL.GL_TEXTURE2);
        gl.glBindTexture(GL.GL_TEXTURE_1D, 0);

        gl.glActiveTexture(GL.GL_TEXTURE3);
        gl.glBindTexture(GL.GL_TEXTURE_1D, 0);
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
    public void loadShaderData(GLShaderProgram program, IImage iimage,
            PaintProperties paintProps) throws VizException {
        AbstractGLColormappedImage image = null;
        if (iimage instanceof AbstractGLColormappedImage == false) {
            throw new VizException(
                    "Cannot apply glsl mosaicing shader to non gl colormap image");
        }
        image = (AbstractGLColormappedImage) iimage;

        GLSLStructFactory.createDataTexture(program, "imageData", 0, image);

        int numMappingValues = 0;
        GLDataMapping mapping = image.getDataMapping();
        if (mapping != null && mapping.isValid()) {
            numMappingValues = mapping.getNumMappingValues();
        }
        GLSLStructFactory.createDataMapping(program, "imageToMosaic", 2, 3,
                numMappingValues);

        GLSLStructFactory.createDataTexture(program, "mosaicData", 1,
                writeToImage);
    }

}
