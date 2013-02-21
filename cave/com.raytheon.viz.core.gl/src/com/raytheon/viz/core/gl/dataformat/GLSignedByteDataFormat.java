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

import java.nio.ByteBuffer;

import javax.media.opengl.GL;

/**
 * GL Signed Byte data format
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

public class GLSignedByteDataFormat extends GLByteDataFormat {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataprep.GLByteDataFormat#getValue(int,
     * int, com.raytheon.viz.core.gl.dataprep.GLColorMapData)
     */
    @Override
    public Short getValue(int x, int y, GLColorMapData data) {
        if (!(data.getData() instanceof ByteBuffer)) {
            throw new IllegalArgumentException(
                    "Expecting data to contain a ByteBuffer but instead it is a "
                            + data.getData().getClass().getSimpleName());
        }
        int width = getAlignedWidth(data.getDimensionSize(0));
        int index = y * width + x;
        ByteBuffer buffer = (ByteBuffer) data.getData();
        byte value = buffer.get(index);
        switch (data.getTextureType()) {
        case GL.GL_BYTE:
            // Raw data returned from this preparer
            return (short) value;
        case GL.GL_UNSIGNED_BYTE:
            // Data copied back from gl.
            return (short) ((byte) (value + Byte.MIN_VALUE));
        default:
            throw new IllegalArgumentException(
                    "Cannot process texture of type " + data.getTextureType());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataformat.GLByteDataFormat#getDataFormatMin()
     */
    @Override
    public double getDataFormatMin() {
        return Byte.MIN_VALUE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataformat.GLByteDataFormat#getDataFormatMax()
     */
    @Override
    public double getDataFormatMax() {
        return Byte.MAX_VALUE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataformat.GLByteDataFormat#getTextureType()
     */
    @Override
    public int getTextureType() {
        return GL.GL_BYTE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getCopyBackTextureType()
     */
    @Override
    public int getCopyBackTextureType() {
        return GL.GL_UNSIGNED_BYTE;
    }

}
