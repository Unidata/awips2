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
package com.raytheon.viz.core.gl.images;

import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.ext.imaging.GLDataMappingFactory.GLDataMapping;
import com.sun.opengl.util.texture.TextureCoords;

/**
 * Base implementation of a gl colormapped image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2013 2333       mschenke    Initial creation
 * Nov  4, 2013 2492       mschenke    Reworked to use GLSL Data mapping
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractGLColormappedImage extends AbstractGLImage
        implements IColormappedImage {

    protected ColorMapParameters colorMapParameters;

    protected GLCMTextureData data;

    private GLDataMapping dataMapping;

    public AbstractGLColormappedImage(GLCMTextureData data,
            ColorMapParameters params,
            Class<? extends IImagingExtension> extensionClass) {
        super(extensionClass);
        this.data = data;
        this.colorMapParameters = params;
        if (data.isLoaded()) {
            setStatus(Status.LOADED);
        } else if (data.isStaged()) {
            setStatus(Status.STAGED);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#stageTexture()
     */
    @Override
    public boolean stageTexture() throws VizException {
        if (data == null) {
            throw new VizException(
                    "Cannot stage texture, image has been disposed");
        }
        return data.stageTexture();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.internal.GLImage#loadTexture(javax.media.opengl
     * .GLContext)
     */
    @Override
    public void loadTexture(GL gl) throws VizException {
        if (data.loadTexture(gl)) {
            // Add to texture cache
            setStatus(Status.LOADED);
        } else {
            setStatus(Status.FAILED);
        }
    }

    /**
     * Return the texture's data type
     * 
     * Example: GL.GL_FLOAT
     * 
     * @return the data type of the texture
     * 
     */
    public int getTextureType() {
        return data.getTextureType();
    }

    public ColorMapDataType getColorMapDataType() {
        return data.getColorMapDataType();
    }

    /**
     * Return the texture's format
     * 
     * Example: GL.GL_LUMINANCE
     * 
     * @return the texture format
     */
    public int getTextureFormat() {
        return data.getTextureFormat();
    }

    /**
     * Return the texture's internal format
     * 
     * This is the format of the texture after driver manipulation
     * 
     * Example: GL.GL_LUMINANCE8
     * 
     * @return the texture internal format
     */
    public int getTextureInternalFormat() {
        return data.getTextureInternalFormat();
    }

    /**
     * @return the textureid
     */
    public int getTextureid() {
        return data.getTexId();
    }

    /**
     * Returns the GL format of the texture data
     * 
     * @return
     */
    public AbstractGLColorMapDataFormat getDataFormat() {
        return data.getDataFormat();
    }

    /**
     * the absolute minimum value of a pixel in this image. {@link Double#NaN}
     * if no absolute minimum exists
     * 
     * @return
     */
    public double getDataMin() {
        return data.getDataMin();
    }

    /**
     * the absolute maximum value of a pixel in this image. {@link Double#NaN}
     * if no absolute maximum exists
     * 
     * @return
     */
    public double getDataMax() {
        return data.getDataMax();
    }

    /**
     * Returns true if the image values will be scaled when loaded into GL
     * 
     * @return
     */
    public boolean isImageFormatScaled() {
        return data.isDataFormatScaled();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IColormappedImage#getColorMapParameters()
     */
    @Override
    public ColorMapParameters getColorMapParameters() {
        return this.colorMapParameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IColormappedImage#setColorMapParameters
     * (com.raytheon.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public void setColorMapParameters(ColorMapParameters params) {
        this.colorMapParameters = params;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.internal.images.GLImage#getTextureStorageType()
     */
    @Override
    public int getTextureStorageType() {
        return data.getTextureStorageType();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.internal.images.GLImage#getHeight()
     */
    @Override
    public int getHeight() {
        return data.getDimensionSize(1);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.internal.images.GLImage#getWidth()
     */
    @Override
    public int getWidth() {
        return data.getDimensionSize(0);
    }

    @Override
    public void dispose() {
        super.dispose();
        if (data != null) {
            data.dispose();
            data = null;
        }
        if (dataMapping != null) {
            dataMapping.dispose();
            dataMapping = null;
        }
    }

    public void setDataMapping(GLDataMapping dataMapping) {
        this.dataMapping = dataMapping;
    }

    public GLDataMapping getDataMapping() {
        return dataMapping;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#getStatus()
     */
    @Override
    public Status getStatus() {
        Status status = super.getStatus();
        if (data == null) {
            if (status != Status.UNLOADED) {
                setStatus(Status.UNLOADED);
            }
        } else if (data.isLoaded()) {
            if (status != Status.LOADED) {
                setStatus(Status.LOADED);
            }
        } else if (data.isStaged()) {
            if (status != Status.STAGED) {
                setStatus(Status.STAGED);
            }
        } else if (data.isLoaded() == false && status == Status.LOADED) {
            if (data.isStaged()) {
                setStatus(Status.STAGED);
            } else {
                setStatus(Status.UNLOADED);
            }
        } else if (data.isStaged() == false && status == Status.STAGED) {
            setStatus(Status.UNLOADED);
        }
        return super.getStatus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#getTextureCoords()
     */
    @Override
    public TextureCoords getTextureCoords() {
        return new TextureCoords(0, 1, 1, 0);
    }

}
