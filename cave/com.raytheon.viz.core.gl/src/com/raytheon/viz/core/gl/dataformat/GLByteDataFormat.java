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
import java.nio.ByteBuffer;

import javax.media.opengl.GL;

/**
 * GL Byte data format
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

public class GLByteDataFormat extends AbstractGLColorMapDataFormat {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#getTextureType
     * ()
     */
    @Override
    public int getTextureType() {
        return GL.GL_UNSIGNED_BYTE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#
     * getTextureInternalFormat()
     */
    @Override
    public int getTextureInternalFormat() {
        return GL.GL_LUMINANCE8;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMin()
     */
    @Override
    public double getDataFormatMin() {
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMax()
     */
    @Override
    public double getDataFormatMax() {
        return 0xFF;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#
     * getCopyBackBuffer()
     */
    @Override
    public Buffer getCopybackBuffer(GLColorMapData data) {
        int width = getAlignedWidth(data.getDimensionSize(0)
                * getValuesPerPixel());
        int height = data.getDimensionSize(1);
        return ByteBuffer.allocate(height * width);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#getValue
     * (int, int, java.nio.Buffer)
     */
    @Override
    public Short getValue(int x, int y, GLColorMapData data) {
        Buffer dataBuffer = data.getData();
        if (data.getTextureType() != GL.GL_UNSIGNED_BYTE) {
            throw new IllegalArgumentException(
                    "Cannot process texture of type " + data.getTextureType());
        } else if (!(dataBuffer instanceof ByteBuffer)) {
            throw new IllegalArgumentException(
                    "Expecting data to contain a ByteBuffer but instead it is a "
                            + dataBuffer != null ? dataBuffer.getClass()
                            .getSimpleName() : "null object");
        }
        int width = getAlignedWidth(data.getDimensionSize(0));
        int index = y * width + x;
        ByteBuffer buffer = (ByteBuffer) dataBuffer;
        return (short) (buffer.get(index) & 0xFF);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#
     * getBytesPerPixel()
     */
    @Override
    public int getBytesPerPixel() {
        return 1;
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
        if (!(toBuffer instanceof ByteBuffer)) {
            throw new IllegalArgumentException(toBuffer.getClass()
                    .getSimpleName() + " is not a ByteBuffer");
        }
        if (!(fromBuffer instanceof ByteBuffer)) {
            throw new IllegalArgumentException(fromBuffer.getClass()
                    .getSimpleName() + " is not a ByteBuffer");
        }
        ByteBuffer dest = (ByteBuffer) toBuffer;
        dest.put((ByteBuffer) fromBuffer);
        for (int i = 0; i < padding; i++) {
            dest.put((byte) 0);
        }
    }

}
