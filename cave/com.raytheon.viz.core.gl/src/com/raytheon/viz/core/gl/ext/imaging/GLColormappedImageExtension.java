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
package com.raytheon.viz.core.gl.ext.imaging;

import java.nio.ByteBuffer;

import javax.measure.unit.Unit;
import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.dataformat.GLByteDataFormat;
import com.raytheon.viz.core.gl.ext.imaging.GLDataMappingFactory.GLDataMapping;
import com.raytheon.viz.core.gl.glsl.AbstractGLSLImagingExtension;
import com.raytheon.viz.core.gl.glsl.GLSLStructFactory;
import com.raytheon.viz.core.gl.glsl.GLShaderProgram;
import com.raytheon.viz.core.gl.images.AbstractGLColormappedImage;
import com.raytheon.viz.core.gl.images.AbstractGLImage;
import com.raytheon.viz.core.gl.images.GLBufferCMTextureData;
import com.raytheon.viz.core.gl.images.GLCMTextureData;
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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 18, 2011           mschenke    Initial creation
 * Feb 14, 2013  1616     bsteffen    Add option for interpolation of colormap
 *                                    parameters, disable colormap interpolation
 *                                    by default.
 * Oct 16, 2013  2333     mschenke    Cleaned up load shader method, used isScaled.
 *                                    Added support for colormapping in non-data unit.
 * Nov 20, 2013  2492     bsteffen    Mosaic in image units.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLColormappedImageExtension extends AbstractGLSLImagingExtension
        implements IColormappedImageExtension {

    private static class GLColormappedImageExtensionData {
        public GLCMTextureData alphaMask;
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
        if (image instanceof AbstractGLColormappedImage) {
            data = new GLColormappedImageExtensionData();
            GL gl = target.getGl();
            AbstractGLColormappedImage glImage = (AbstractGLColormappedImage) image;
            // First see if the colormap has been loaded
            ColorMapParameters usedColorMapParameters = ((IColormappedImage) glImage)
                    .getColorMapParameters();
            if (usedColorMapParameters == null
                    || usedColorMapParameters.getColorMap() == null) {
                return null;
            }

            // Get and stage colormap texture
            GLTextureObject cmapTexture = target
                    .getColorMapTexture(usedColorMapParameters);

            gl.glActiveTexture(GL.GL_TEXTURE1);
            cmapTexture.bind(gl, GL.GL_TEXTURE_1D);

            if (usedColorMapParameters.isInterpolate()) {
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

            if (usedColorMapParameters.isUseMask()) {
                data.alphaMask = setupAlphaMasking(gl, GL.GL_TEXTURE2,
                        usedColorMapParameters.getAlphaMask());
            }

            setupDataMapping(gl, glImage, GL.GL_TEXTURE3, GL.GL_TEXTURE4);
        }
        return data;
    }

    /**
     * Sets up a {@link GLCMTextureData} for an alpha mask for use in image
     * rendering
     * 
     * @param gl
     * @param maskTexBinding
     * @param mask
     * @return The GLCMTextureData the alpha mask is bound to or null if the
     *         texture failed to initialize
     * @throws VizException
     */
    public static GLCMTextureData setupAlphaMasking(GL gl, int maskTexBinding,
            byte[] mask) throws VizException {
        GLBufferCMTextureData maskData = new GLBufferCMTextureData(
                new ColorMapData(ByteBuffer.wrap(mask),
                        new int[] { mask.length }, ColorMapDataType.BYTE),
                new GLByteDataFormat());
        gl.glActiveTexture(maskTexBinding);
        if (maskData.loadTexture(gl)) {
            gl.glBindTexture(maskData.getTextureStorageType(),
                    maskData.getTexId());
        } else {
            maskData.dispose();
            maskData = null;
        }
        return maskData;
    }

    /**
     * Sets up a {@link GLDataMapping} for use in image rendering. Data will be
     * mapped to the image's {@link ColorMapParameters#getColorMapUnit()}
     * 
     * @param gl
     * @param glImage
     * @param dataMappedTexBinding
     * @param colorMappedTexBinding
     * @throws VizException
     */
    public static void setupDataMapping(GL gl,
            AbstractGLColormappedImage glImage, int dataMappedTexBinding,
            int colorMappedTexBinding) throws VizException {
        setupDataMapping(gl, glImage, glImage.getColorMapParameters()
                .getColorMapUnit(), dataMappedTexBinding, colorMappedTexBinding);
    }

    /**
     * Sets up a {@link GLDataMapping} for use in image renderingData will be
     * mapped to the unit provided
     * 
     * @param gl
     * @param glImage
     * @param colorMapUnit
     * @param dataMappedTexBinding
     * @param colorMappedTexBinding
     * @throws VizException
     */
    public static void setupDataMapping(GL gl,
            AbstractGLColormappedImage glImage, Unit<?> colorMapUnit,
            int dataMappedTexBinding, int colorMappedTexBinding)
            throws VizException {
        ColorMapParameters colorMapParameters = glImage.getColorMapParameters();
        // Get GLDataMapping and generate if datamapping is not set. If
        // datamapping is not set, the data has already been mapped to
        // colorMapUnits and we need not do anything
        GLDataMapping dataMapping = glImage.getDataMapping();
        if (dataMapping == null && colorMapParameters.getDataMapping() == null) {
            Unit<?> dataUnit = glImage.getDataUnit();
            int colorMapSize = colorMapParameters.getColorMap().getSize();
            float colorMapMin = colorMapParameters.getColorMapMin();
            float colorMapMax = colorMapParameters.getColorMapMax();
            dataMapping = GLDataMappingFactory.constructGLDataMapping(gl,
                    dataUnit, colorMapUnit, colorMapMin, colorMapMax,
                    colorMapSize);
            glImage.setDataMapping(dataMapping);
        }

        if (dataMapping != null && dataMapping.isValid()) {
            GLCMTextureData glDataMapping = dataMapping.getDataMapping();
            gl.glActiveTexture(dataMappedTexBinding);
            if (glDataMapping.isLoaded() == false) {
                glDataMapping.loadTexture(gl);
            }
            if (glDataMapping.isLoaded()) {
                gl.glBindTexture(glDataMapping.getTextureStorageType(),
                        glDataMapping.getTexId());
            }

            GLCMTextureData glColorMapping = dataMapping.getColorMapping();
            gl.glActiveTexture(colorMappedTexBinding);
            if (glColorMapping.isLoaded() == false) {
                glColorMapping.loadTexture(gl);
            }
            if (glColorMapping.isLoaded()) {
                gl.glBindTexture(glColorMapping.getTextureStorageType(),
                        glColorMapping.getTexId());
            }
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
        GLColormappedImageExtensionData imageData = (GLColormappedImageExtensionData) data;
        GL gl = target.getGl();

        gl.glActiveTexture(GL.GL_TEXTURE1);
        gl.glBindTexture(GL.GL_TEXTURE_1D, 0);

        if (imageData.alphaMask != null) {
            gl.glActiveTexture(GL.GL_TEXTURE2);
            gl.glBindTexture(imageData.alphaMask.getTextureStorageType(), 0);

            imageData.alphaMask.dispose();
        }

        gl.glActiveTexture(GL.GL_TEXTURE3);
        gl.glBindTexture(GL.GL_TEXTURE_1D, 0);

        gl.glActiveTexture(GL.GL_TEXTURE4);
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
        AbstractGLColormappedImage image = null;
        if (iimage instanceof AbstractGLColormappedImage == false) {
            throw new VizException(
                    "Cannot apply glsl colormap raster shader to non gl colormap image");
        }
        image = (AbstractGLColormappedImage) iimage;

        ColorMapParameters colorMapParameters = image.getColorMapParameters();

        GLSLStructFactory.createDataTexture(program, "rawData", 0, image);

        int numMappingValues = 0;
        GLDataMapping mapping = image.getDataMapping();
        if (mapping != null && mapping.isValid()) {
            numMappingValues = mapping.getNumMappingValues();
        }
        GLSLStructFactory.createDataMapping(program, "dataMapping", 3, 4,
                numMappingValues);

        GLSLStructFactory.createColorMapping(program, "colorMapping", 1, 2,
                colorMapParameters);

        GLSLStructFactory.createColorModifiers(program, "modifiers",
                paintProps.getAlpha(), image.getBrightness(),
                image.getContrast());
    }
}
