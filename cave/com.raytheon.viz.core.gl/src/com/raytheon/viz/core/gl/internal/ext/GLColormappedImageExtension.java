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
package com.raytheon.viz.core.gl.internal.ext;

import java.nio.ByteBuffer;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLColormappedImage;
import com.raytheon.viz.core.gl.objects.GLTextureObject;

/**
 * GL {@link IColormappedImageExtension} implementation, creates
 * {@link GLColormappedImage} objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLColormappedImageExtension extends AbstractGLSLImagingExtension
        implements IColormappedImageExtension {

    private static class GLColormappedImageExtensionData {
        public GLColormappedImage alphaMaskTexture;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IColormappedImageExtension#
     * initializeRaster
     * (com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public GLColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        return new GLColormappedImage(dataCallback, colorMapParameters,
                GLColormappedImageExtension.class);
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
    public Object preImageRender(PaintProperties paintProps,
            AbstractGLImage image, PixelCoverage coverage) throws VizException {
        GLColormappedImageExtensionData data = null;
        if (image instanceof GLColormappedImage) {
            data = new GLColormappedImageExtensionData();
            GL gl = target.getGl();
            GLColormappedImage glImage = (GLColormappedImage) image;
            // First see if the colormap has been loaded
            ColorMapParameters usedColorMapParameters = ((IColormappedImage) glImage)
                    .getColorMapParameters();
            if (usedColorMapParameters == null
                    || usedColorMapParameters.getColorMap() == null) {
                return null;
            }

            if (usedColorMapParameters.isUseMask()) {
                final byte[] mask = usedColorMapParameters.getAlphaMask();
                data.alphaMaskTexture = initializeRaster(
                        new IColorMapDataRetrievalCallback() {
                            @Override
                            public ColorMapData getColorMapData()
                                    throws VizException {
                                return new ColorMapData(ByteBuffer.wrap(mask),
                                        new int[] { mask.length, 1 });
                            }
                        }, usedColorMapParameters);
                data.alphaMaskTexture.stage();
                data.alphaMaskTexture.target(target);
            }

            // Get and stage colormap texture
            GLTextureObject cmapTexture = target
                    .getColorMapTexture(usedColorMapParameters);

            if (data.alphaMaskTexture != null) {
                gl.glActiveTexture(GL.GL_TEXTURE2);
                gl.glBindTexture(data.alphaMaskTexture.getTextureStorageType(),
                        data.alphaMaskTexture.getTextureid());
            }

            gl.glActiveTexture(GL.GL_TEXTURE1);
            cmapTexture.bind(gl, GL.GL_TEXTURE_1D);

            if (glImage.isInterpolated()) {
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MIN_FILTER,
                        GL.GL_LINEAR);
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MAG_FILTER,
                        GL.GL_LINEAR);
            } else {
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MIN_FILTER,
                        GL.GL_NEAREST);
                gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MAG_FILTER,
                        GL.GL_NEAREST);
            }
        }
        return data;
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
        GLColormappedImageExtensionData imageData = (GLColormappedImageExtensionData) data;
        GL gl = target.getGl();
        if (imageData.alphaMaskTexture != null) {
            gl.glActiveTexture(GL.GL_TEXTURE2);
            gl.glBindTexture(
                    imageData.alphaMaskTexture.getTextureStorageType(), 0);

            imageData.alphaMaskTexture.dispose();
        }

        gl.glActiveTexture(GL.GL_TEXTURE1);
        gl.glBindTexture(GL.GL_TEXTURE_1D, 0);
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
        return "colormapRaster";
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
        // Get image as AbstractGLImage
        GLColormappedImage image = null;
        if (iimage instanceof GLColormappedImage == false) {
            throw new VizException(
                    "Cannot apply glsl colormap raster shader to non gl colormap image");
        }
        image = (GLColormappedImage) iimage;

        GLColormappedImage colormappedImg = (GLColormappedImage) image;
        ColorMapParameters colorMapParameters = colormappedImg
                .getColorMapParameters();

        program.setUniform("colorMapSz", colorMapParameters.getColorMap()
                .getSize());
        int textureType = image.getTextureType();
        boolean isFloat = textureType == GL.GL_FLOAT
                || textureType == GL.GL_HALF_FLOAT_ARB;
        double dataMin = colorMapParameters.getDataMin();
        double dataMax = colorMapParameters.getDataMax();
        if (isFloat == false) {
            // get format from image and get data min/max from it
            dataMin = image.getDataMin();
            dataMax = image.getDataMax();
        }

        program.setUniform("isFloat", isFloat);
        program.setUniform("logarithmic",
                colorMapParameters.isLogarithmic() ? 1 : 0);
        program.setUniform("logFactor", colorMapParameters.getLogFactor());
        program.setUniform("mirror", colorMapParameters.isMirror() ? 1 : 0);

        program.setUniform("applyMask", colorMapParameters.isUseMask() ? 1 : 0);

        program.setUniform("naturalMin", dataMin);
        program.setUniform("naturalMax", dataMax);
        program.setUniform("cmapMin", colorMapParameters.getColorMapMin());
        program.setUniform("cmapMax", colorMapParameters.getColorMapMax());

        program.setUniform("alphaMask", 2);
        program.setUniform("colorMap", 1);
        program.setUniform("rawText", 0);

        program.setUniform("brightness", image.getBrightness());
        program.setUniform("contrast", image.getContrast());
        program.setUniform("alpha", paintProps.getAlpha());
    }

}
