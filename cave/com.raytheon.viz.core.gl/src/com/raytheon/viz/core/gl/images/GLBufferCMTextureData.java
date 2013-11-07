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

import javax.measure.unit.Unit;
import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.dataformat.GLBufferColorMapData;

/**
 * {@link GLCMTextureData} backed by a {@link Buffer}. The initial functions in
 * here were moved here from {@link GLRetrievableCMTextureData}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2013 2492       mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLBufferCMTextureData extends GLCMTextureData {

    public GLBufferCMTextureData(ColorMapData data,
            AbstractGLColorMapDataFormat format) {
        this(new GLBufferColorMapData(data, format));
    }

    public GLBufferCMTextureData(GLBufferColorMapData data) {
        super(data);
    }

    @Override
    protected GLBufferColorMapData getDataObject() {
        return (GLBufferColorMapData) super.getDataObject();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.GLCMTextureData#isStaged()
     */
    @Override
    public boolean isStaged() {
        GLBufferColorMapData data = getDataObject();
        // Override since we have our required data
        return data != null && data.getData() != null;
    }

    @Override
    public void dispose() {
        super.dispose();
        if (isStaged()) {
            data = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.images.GLCMTextureData#uploadTexture2D(javax
     * .media.opengl.GL, int, int, int)
     */
    @Override
    protected void createTexture2D(GL gl, int type, int w, int h) {
        gl.glTexImage2D(type, 0, getTextureInternalFormat(), w, h, 0,
                getTextureFormat(), getTextureType(), getDataObject().getData()
                        .rewind());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.images.GLCMTextureData#uploadTexture1D(javax
     * .media.opengl.GL, int, int)
     */
    @Override
    protected void createTexture1D(GL gl, int type, int w) {
        gl.glTexImage1D(type, 0, getTextureInternalFormat(), w, 0,
                getTextureFormat(), getTextureType(), getDataObject().getData()
                        .rewind());
    }

    /**
     * Returns the data value at the given coordinates. TODO: Add support for 1D
     * texture sampling, change x,y to int[] index?
     * 
     * @param x
     * @param y
     * @return
     */
    public double getValue(int x, int y) {
        double value = Double.NaN;
        if (isStaged()) {
            value = getDataObject().getValue(x, y).doubleValue();
        }
        return value;
    }

    /**
     * Returns the {@link Unit} associated with the data
     * 
     * @return the dataUnit
     */
    public Unit<?> getDataUnit() {
        GLBufferColorMapData data = getDataObject();
        return data != null ? data.getDataUnit() : null;
    }
}
