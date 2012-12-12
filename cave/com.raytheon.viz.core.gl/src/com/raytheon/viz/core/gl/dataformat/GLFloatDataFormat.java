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
package com.raytheon.viz.core.gl.dataformat;

import java.nio.Buffer;
import java.nio.FloatBuffer;

import javax.media.opengl.GL;

/**
 * GL Float data format
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLFloatDataFormat extends AbstractGLColorMapDataFormat {

    @Override
    public int getBytesPerPixel() {
        return 4;
    }

    @Override
    public int getTextureInternalFormat() {
        return GL.GL_LUMINANCE32F_ARB;
    }

    @Override
    public int getTextureType() {
        return GL.GL_FLOAT;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMin()
     */
    @Override
    public double getDataFormatMin() {
        return Double.NaN;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMax()
     */
    @Override
    public double getDataFormatMax() {
        return Double.NaN;
    }

    @Override
    public FloatBuffer getCopybackBuffer(GLColorMapData data) {
        int width = data.getDimensionSize(0);
        int height = data.getDimensionSize(1);
        return FloatBuffer.allocate(height * width);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#getValue
     * (int, int, com.raytheon.viz.core.gl.dataprep.GLColorMapData)
     */
    @Override
    public Float getValue(int x, int y, GLColorMapData data) {
        if (data.getTextureType() != GL.GL_FLOAT) {
            throw new IllegalArgumentException(
                    "Cannot process texture of type " + data.getTextureType());
        } else if (!(data.getData() instanceof FloatBuffer)) {
            throw new IllegalArgumentException(
                    "Expecting data to contain a FloatBuffer but instead it is a "
                            + data.getData().getClass().getSimpleName());
        }
        int width = data.getDimensionSize(0);
        int index = y * width + x;
        FloatBuffer buffer = (FloatBuffer) data.getData();
        return buffer.get(index);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#copyRow
     * (java.nio.Buffer, java.nio.Buffer, int)
     */
    @Override
    protected void copyRow(Buffer fromBuffer, Buffer toBuffer, int padding) {
        if (!(toBuffer instanceof FloatBuffer)) {
            throw new IllegalArgumentException(toBuffer.getClass()
                    .getSimpleName() + " is not a FloatBuffer");
        }
        if (!(fromBuffer instanceof FloatBuffer)) {
            throw new IllegalArgumentException(fromBuffer.getClass()
                    .getSimpleName() + " is not a FloatBuffer");
        }
        FloatBuffer dest = (FloatBuffer) toBuffer;
        dest.put((FloatBuffer) fromBuffer);
        for (int i = 0; i < padding; i++) {
            dest.put(0.0f);
        }
    }
}
