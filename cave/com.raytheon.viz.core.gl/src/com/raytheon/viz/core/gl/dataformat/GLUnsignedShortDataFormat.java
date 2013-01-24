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

import java.nio.ShortBuffer;

import javax.media.opengl.GL;

/**
 * GL Unsigned short data format
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLUnsignedShortDataFormat extends GLShortDataFormat {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataformat.GLShortDataFormat#getTextureType()
     */
    @Override
    public int getTextureType() {
        return GL.GL_UNSIGNED_SHORT;
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
        return 0xFFFF;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.GLShortDataFormat#getValue(int,
     * int, com.raytheon.viz.core.gl.dataformat.GLColorMapData)
     */
    @Override
    public Number getValue(int x, int y, GLColorMapData data) {
        if (!(data.getData() instanceof ShortBuffer)) {
            throw new IllegalArgumentException(
                    "Expecting data to contain a ShortBuffer but instead it is a "
                            + data.getData().getClass().getSimpleName());
        }
        int width = getAlignedWidth(data.getDimensionSize(0));
        int index = y * width + x;
        ShortBuffer buffer = (ShortBuffer) data.getData();
        return (buffer.get(index) & 0xFFFF);
    }

}
