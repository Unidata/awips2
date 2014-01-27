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
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLColorMapData;
import com.raytheon.viz.core.gl.objects.GLTextureObject;

/**
 * Colormappable texture data object. Does not provide source of data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2011             bsteffen    Initial creation
 * Mar 21, 2013 1806       bsteffen    Update GL mosaicing to use dynamic data
 *                                     format for offscreen textures.
 * Oct 16, 2013 2333       mschenke    Moved retrievable/Buffer parts out and
 *                                     into separate class.
 * Oct 23, 2013 2492       mschenke    Added support for 1D textures
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLCMTextureData {

    protected GLTextureObject tex;

    protected GLColorMapData data;

    /**
     * Constructs a GLCMTextureData with the specified GLColorMapData.
     * 
     * @param data
     */
    public GLCMTextureData(GLColorMapData data) {
        this.data = data;
        if (data == null && getClass().equals(GLCMTextureData.class)) {
            // If null data and class is not overridden, throw exception
            throw new IllegalArgumentException(
                    "null GLColorMapData is not allowed for GLCMTextureData");
        }
    }

    protected GLColorMapData getDataObject() {
        return data;
    }

    /**
     * Disposes the texture data object
     */
    public void dispose() {
        // Dispose the texture
        disposeTexture();
    }

    /**
     * Disposes the underlying texture in the texture data
     */
    public synchronized void disposeTexture() {
        if (isLoaded()) {
            tex.dispose();
            tex = null;
        }
    }

    /**
     * Stages the texture data for use.
     * 
     * @return true if successfully staged, false otherwise
     * @throws VizException
     */
    public synchronized boolean stageTexture() throws VizException {
        // There is no data to stage
        return isStaged();
    }

    /**
     * Loads the texture object into GL
     * 
     * @param gl
     * @return true if texture is loaded, false otherwise
     * @throws VizException
     */
    public synchronized boolean loadTexture(GL gl) throws VizException {
        // Don't need to load if we are already loaded
        if (isLoaded()) {
            return true;
        }
        // Make sure we have the data.
        if (!stageTexture()) {
            return false;
        }
        int type = getTextureStorageType();
        if (type == GL.GL_NONE) {
            throw new VizException("Unsupported dimension size for texture");
        }

        tex = new GLTextureObject(this);
        tex.bind(gl, type);

        gl.glTexParameteri(type, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
        gl.glTexParameteri(type, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP_TO_EDGE);
        gl.glTexParameteri(type, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
        gl.glTexParameteri(type, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

        boolean changeScaleAndBias = isDataFormatScaled() && isDataFormatSigned();
        if (changeScaleAndBias) {
            // GL maps signed data into the range -1 to 1, but gl trims
            // this to a valid range of 0 to 1, essentially removing
            // negative values. Adding a scale and bias remaps this from
            // 0 to 1, where 0 is the smallest negative number and 1 is
            // the largest positive number.
            gl.glPixelTransferf(GL.GL_RED_SCALE, 0.5f);
            gl.glPixelTransferf(GL.GL_RED_BIAS, 0.5f);
        }

        if (type == GL.GL_TEXTURE_1D) {
            createTexture1D(gl, type, getDimensionSize(0));
        } else if (type == GL.GL_TEXTURE_2D) {
            createTexture2D(gl, type, getDimensionSize(0), getDimensionSize(1));
        }

        if (changeScaleAndBias) {
            gl.glPixelTransferf(GL.GL_RED_SCALE, 1.0f);
            gl.glPixelTransferf(GL.GL_RED_BIAS, 0.0f);
        }

        return true;
    }

    /**
     * Creates a 2D texture for type, with width/height, w/h. Texture object
     * must be bound for this call to work using
     * {@link GLTextureObject#bind(GL, int)}
     * 
     * @param gl
     * @param type
     * @param w
     * @param h
     */
    protected void createTexture2D(GL gl, int type, int w, int h) {
        // Allocate our space on the graphics card, no buffer to upload so it
        // will be filled with default values initially (0s)
        gl.glTexImage2D(type, 0, getTextureInternalFormat(), w, h, 0,
                getTextureFormat(), getTextureType(), null);
    }

    /**
     * Creates a 1D texture for type, with width, w. Texture object must be
     * bound for this call to work using {@link GLTextureObject#bind(GL, int)}
     * 
     * @param gl
     * @param type
     * @param w
     * @param h
     */
    protected void createTexture1D(GL gl, int type, int w) {
        // Allocate our space on the graphics card, no buffer to upload so it
        // will be filled with default values initially (0s)
        gl.glTexImage1D(type, 0, getTextureInternalFormat(), w, 0,
                getTextureFormat(), getTextureType(), null);
    }

    /**
     * Checks if texture data is staged. If false, a call to
     * {@link #stageTexture()} is needed before texture can be loaded
     * 
     * @return true if staged
     */
    public boolean isStaged() {
        // There is nothing to stage, so we are good if data != null
        return getDataObject() != null;
    }

    /**
     * Checks if texture data is loaded. If false, a call to
     * {@link #loadTexture(GL)} is needed before the texture can be used in GL
     * 
     * @return true if staged
     */
    public boolean isLoaded() {
        return tex != null && tex.isValid();
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
     * Returns the size of the dimension index passed in (0=width,1=height)
     * 
     * @param dimension
     * @return
     */
    public int getDimensionSize(int dimension) {
        return data.getDimensionSize(dimension);
    }

    /**
     * Returns the texture type of the data (FLOAT,INT,SHORT,etc).
     * 
     * @return
     */
    public int getTextureType() {
        return data.getTextureType();
    }

    /**
     * Returns texture format (LUMINANCE,RGB)
     * 
     * @return
     */
    public int getTextureFormat() {
        return data.getTextureFormat();
    }

    /**
     * Returns texture gl internal format (number of bits: LUMINANCE8/16/32)
     * 
     * @return
     */
    public int getTextureInternalFormat() {
        return data.getTextureInternalFormat();
    }

    /**
     * Returns the minimum valid data value for the texture format. (0 for
     * unsigned byte, -128 for signed byte, etc)
     * 
     * @return
     */
    public double getDataMin() {
        return data.getDataFormatMin();
    }

    /**
     * Returns the maximium valid data value for the texture format. ( 255 for
     * usnigned byte, 127 for signed byte, etc)
     * 
     * @return
     */
    public double getDataMax() {
        return data.getDataFormatMax();
    }

    /**
     * Returns true if the data values when uploaded to GL are in a scaled
     * format and need to be converted to get actual data value
     * 
     * @return
     */
    public boolean isDataFormatScaled() {
        return data.isDataFormatScaled();
    }

    /**
     * Returns true if the data format is a signed data format (values in format
     * can be less than 0), false otherwise
     * 
     * @return true if signed format
     */
    public boolean isDataFormatSigned() {
        return data.isDataFormatSigned();
    }

    /**
     * The id of the texture in GL. Will not work if {@link #isLoaded()} returns
     * false
     * 
     * @return
     */
    public int getTexId() {
        return tex.getId();
    }

    /**
     * The texture storage type of the data. Will return {@link GL#GL_NONE} if
     * unsupported dimension is detected
     * 
     * @return
     */
    public int getTextureStorageType() {
        switch (data.getNumDimensions()) {
        case 1:
            return GL.GL_TEXTURE_1D;
        case 2:
            return GL.GL_TEXTURE_2D;
        default:
            return GL.GL_NONE;
        }
    }

    /**
     * Returns the {@link ColorMapDataType} for the texture
     * 
     * @return
     */
    public ColorMapDataType getColorMapDataType() {
        return data.getDataType();
    }

}
