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
import java.nio.IntBuffer;

import javax.media.opengl.GL;

/**
 * GL Int data format
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

public class GLIntDataFormat extends AbstractGLColorMapDataFormat {

    @Override
    public int getBytesPerPixel() {
        return 4;
    }

    @Override
    public int getTextureInternalFormat() {
        return GL.GL_LUMINANCE;
    }

    @Override
    public int getTextureType() {
        return GL.GL_INT;
    }

    @Override
    public int getCopyBackTextureType() {
        return GL.GL_UNSIGNED_INT;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMin()
     */
    @Override
    public double getDataFormatMin() {
        return Integer.MIN_VALUE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMax()
     */
    @Override
    public double getDataFormatMax() {
        return Integer.MAX_VALUE;
    }

    @Override
    public IntBuffer getCopybackBuffer(GLColorMapData data) {
        int width = data.getDimensionSize(0);
        int height = data.getDimensionSize(1);
        return IntBuffer.allocate(height * width);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#getValue
     * (int, int, com.raytheon.viz.core.gl.dataprep.GLColorMapData)
     */
    @Override
    public Integer getValue(int x, int y, GLColorMapData data) {
        if (!(data.getData() instanceof IntBuffer)) {
            throw new IllegalArgumentException(
                    "Expecting data to contain a IntBuffer but instead it is a "
                            + data.getData().getClass().getSimpleName());
        }
        int width = data.getDimensionSize(0);
        int index = y * width + x;
        IntBuffer buffer = (IntBuffer) data.getData();
        int value = buffer.get(index);
        switch (data.getTextureType()) {
        case GL.GL_INT:
            // Raw data returned from this preparer
            return value;
        case GL.GL_UNSIGNED_INT:
            // Data copied back from gl.
            return value + Integer.MIN_VALUE;
        default:
            throw new IllegalArgumentException(
                    "Cannot process texture of type " + data.getTextureType());
        }
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
        if (!(toBuffer instanceof IntBuffer)) {
            throw new IllegalArgumentException(toBuffer.getClass()
                    .getSimpleName() + " is not a ShortBuffer");
        }
        if (!(fromBuffer instanceof IntBuffer)) {
            throw new IllegalArgumentException(fromBuffer.getClass()
                    .getSimpleName() + " is not a ShortBuffer");
        }
        IntBuffer dest = (IntBuffer) toBuffer;
        dest.put((IntBuffer) fromBuffer);
        for (int i = 0; i < padding; i++) {
            dest.put((short) 0);
        }
    }
}
