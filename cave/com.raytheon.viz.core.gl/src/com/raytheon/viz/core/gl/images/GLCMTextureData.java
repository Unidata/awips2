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

import java.nio.Buffer;
import java.util.HashMap;
import java.util.Map;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.GLContextBridge;
import com.raytheon.viz.core.gl.dataformat.GLColorMapData;
import com.raytheon.viz.core.gl.dataformat.IGLColorMapDataFormatProvider;
import com.raytheon.viz.core.gl.internal.cache.IImageCacheable;
import com.raytheon.viz.core.gl.internal.cache.ImageCache;
import com.raytheon.viz.core.gl.internal.cache.ImageCache.CacheType;
import com.raytheon.viz.core.gl.objects.GLTextureObject;

/**
 * 
 * TODO Make use new ColorMapData retrieval callback stuff.... Keep old
 * CMDataPreparer stuff working
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLCMTextureData implements IImageCacheable {

    private GLTextureObject tex;

    private final IColorMapDataRetrievalCallback callback;

    private int refCount = 0;

    private GLColorMapData data = null;

    private GLCMTextureData(IColorMapDataRetrievalCallback callback) {
        this.callback = callback;
    }

    public void use() {
        refCount += 1;
    }

    public void dispose() {
        synchronized (texMap) {
            refCount -= 1;
            if (refCount == 0) {
                texMap.remove(callback);
                ImageCache.getInstance(CacheType.MEMORY).remove(this);
                ImageCache.getInstance(CacheType.TEXTURE).remove(this);
            }
        }
    }

    public synchronized void disposeTexture(GL gl) {
        if (isLoaded()) {
            tex.dispose();
            tex = null;
        }
        ImageCache.getInstance(CacheType.TEXTURE).remove(this);
    }

    public synchronized void disposeTextureData() {
        if (isStaged()) {
            data.setData(null);
        }
        ImageCache.getInstance(CacheType.MEMORY).remove(this);
    }

    public synchronized boolean stageTexture() throws VizException {
        // Don't need to stage if we are already in gpu
        if (isLoaded()) {
            return true;
        }
        // Don't need to stage if we already have data
        if (isStaged()) {
            return true;
        }

        // OK, Fetch the data
        ColorMapData cmData = callback.getColorMapData();
        if (cmData != null) {
            IGLColorMapDataFormatProvider glDataFormatCallback = IGLColorMapDataFormatProvider.defaultCallback;
            if (callback instanceof IGLColorMapDataFormatProvider) {
                glDataFormatCallback = (IGLColorMapDataFormatProvider) callback;
            }
            data = new GLColorMapData(cmData,
                    glDataFormatCallback.getGLColorMapDataFormat(cmData));
            if (isStaged()) {
                ImageCache.getInstance(CacheType.MEMORY).put(this);
                return true;
            }
        }
        // The data fetch didn't go well
        return false;
    }

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

        tex = new GLTextureObject(this);
        tex.bind(gl, type);

        gl.glTexParameteri(type, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
        gl.glTexParameteri(type, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP_TO_EDGE);
        gl.glTexParameteri(type, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
        gl.glTexParameteri(type, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

        if (getTextureType() == GL.GL_SHORT || getTextureType() == GL.GL_INT
                || getTextureType() == GL.GL_BYTE) {
            // GL maps signed data into the range -1 to 1, but gl trims
            // this to a valid range of 0 to 1, essentially removing
            // negative values. Adding a scale and bias remaps this from
            // 0 to 1, where 0 is the smallest negative number and 1 is
            // the largest positive number.
            gl.glPixelTransferf(GL.GL_RED_SCALE, 0.5f);
            gl.glPixelTransferf(GL.GL_RED_BIAS, 0.5f);
        }

        int w = data.getDimensionSize(0);
        int h = data.getDimensionSize(1);

        gl.glTexImage2D(type, 0, getTextureInternalFormat(), w, h, 0,
                getTextureFormat(), getTextureType(), data.getData().rewind());
        gl.glPixelTransferf(GL.GL_RED_SCALE, 1.0f);
        gl.glPixelTransferf(GL.GL_RED_BIAS, 0.0f);
        ImageCache.getInstance(CacheType.TEXTURE).put(this);
        return true;
    }

    public boolean isStaged() {
        return data != null && data.getData() != null;
    }

    public boolean isLoaded() {
        return tex != null && tex.isValid();
    }

    public int getDimensionSize(int dimension) {
        return data.getDimensionSize(dimension);
    }

    public int getTextureType() {
        return data.getTextureType();
    }

    public int getTextureFormat() {
        return data.getTextureFormat();
    }

    public int getTextureInternalFormat() {
        return data.getTextureInternalFormat();
    }

    public double getDataMin() {
        return data.getDataFormatMin();
    }

    public double getDataMax() {
        return data.getDataFormatMax();
    }

    public int getTexId() {
        if (isLoaded()) {
            ImageCache.getInstance(CacheType.TEXTURE).put(this);
        }
        return tex.getId();
    }

    public int getTextureStorageType() {
        return GL.GL_TEXTURE_2D;
    }

    public double getValue(int x, int y) {
        double value = Double.NaN;
        if (!isStaged() && isLoaded()) {
            GLContextBridge.makeMasterContextCurrent();
            GL gl = GLU.getCurrentGL();
            int textureStorageType = getTextureStorageType();
            int copybackTextureType = data.getCopyBackTextureType();
            Buffer copybackBuffer = data.getCopybackBuffer();
            gl.glEnable(textureStorageType);
            gl.glActiveTexture(GL.GL_TEXTURE0);
            tex.bind(gl, textureStorageType);
            gl.glGetTexImage(textureStorageType, 0, getTextureFormat(),
                    copybackTextureType, copybackBuffer.rewind());
            gl.glActiveTexture(GL.GL_TEXTURE0);
            gl.glBindTexture(textureStorageType, 0);
            gl.glDisable(textureStorageType);

            data.setTextureType(copybackTextureType);
            data.setData(copybackBuffer);
            GLContextBridge.releaseMasterContext();
        }
        if (data != null) {
            ImageCache.getInstance(CacheType.MEMORY).put(this);
            value = data.getValue(x, y).doubleValue();
        }
        return value;
    }

    private static Map<IColorMapDataRetrievalCallback, GLCMTextureData> texMap = new HashMap<IColorMapDataRetrievalCallback, GLCMTextureData>();

    public static GLCMTextureData getGlTextureId(
            IColorMapDataRetrievalCallback callback) {
        synchronized (texMap) {
            GLCMTextureData data = texMap.get(callback);
            if (data == null) {
                data = new GLCMTextureData(callback);
                texMap.put(callback, data);
            }
            data.use();
            return data;
        }
    }

    @Override
    public int getSize() {
        if (data != null) {
            int[] dimensions = data.getDimensions();
            int totalSize = data.getBytesPerPixel();
            for (int i = 0; i < dimensions.length; ++i) {
                totalSize *= dimensions[i];
            }
            return totalSize;
        }
        return 0;
    }

}
