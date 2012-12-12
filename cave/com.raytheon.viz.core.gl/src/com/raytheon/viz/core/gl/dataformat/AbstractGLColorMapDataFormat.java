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

import javax.media.opengl.GL;

/**
 * Abstract gl data format object for colormapped data
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

public abstract class AbstractGLColorMapDataFormat {

    /**
     * GL textureType id
     * 
     * @return
     */
    public abstract int getTextureType();

    /**
     * GL Internal texture format
     * 
     * @return
     */
    public abstract int getTextureInternalFormat();

    /**
     * Gets the absolute minimum value a pixel can have in this format. For
     * example, the minimum value for an unsigned byte is 0, signed byte is
     * -127, etc. {@link Double#NaN} can be returned in case no absolute minimum
     * exists (such as case with floats and doubles)
     * 
     * @return
     */
    public abstract double getDataFormatMin();

    /**
     * Gets the absolute maximum value a pixel can have in this format. For
     * example, the minimum value for an unsigned byte is 255, signed byte is
     * 128, etc. {@link Double#NaN} can be returned in case no absolute maximum
     * exists (such as case with floats and doubles)
     * 
     * @return
     */
    public abstract double getDataFormatMax();

    /**
     * Create a buffer object for specified data for copying data from out of GL
     * 
     * @param data
     * @return
     */
    public abstract Buffer getCopybackBuffer(GLColorMapData data);

    /**
     * Get the value at position x,y for the given GL Data
     * 
     * @param x
     * @param y
     * @param data
     * @return
     */
    public abstract Number getValue(int x, int y, GLColorMapData data);

    /**
     * Get the number of bytes each pixel takes up
     * 
     * @return
     */
    public abstract int getBytesPerPixel();

    /**
     * TODO: Use this method for RGB types. Returns the number of points per
     * pixel cell
     * 
     * @return
     */
    public int getValuesPerPixel() {
        return 1;
    }

    /**
     * Copy a single row from a source buffer to a new buffer, used by
     * handleBufferSizing to copy data to new buffer when needed. Should copy
     * all remaining bytes in fromBuffer to toBuffer and increment the position
     * of both buffers.
     * 
     * @param fromBuffer
     *            - source buffer
     * @param toBuffer
     *            - dest buffer
     * @param padding
     *            - number of padding elements to add for alignment
     */
    protected abstract void copyRow(Buffer fromBuffer, Buffer toBuffer,
            int padding);

    /**
     * Formats the data buffer for gl
     * 
     * @param buffer
     * @param data
     * @return
     */
    public Buffer formatForGL(Buffer buffer, GLColorMapData data) {
        return handleBufferSizing(data, buffer, data.getDimensions());
    }

    /**
     * GL textureType id when reading data off graphics card
     * 
     * @return
     */
    public int getCopyBackTextureType() {
        return getTextureType();
    }

    /**
     * 
     * @return the texture format for this data, passed into glTexImage2D
     */
    public int getTextureFormat() {
        return GL.GL_LUMINANCE;
    }

    /**
     * Get the word aligned width for the data format width
     * 
     * @param width
     * @param bytesPerPixel
     * @return
     */
    protected int getAlignedWidth(int width) {
        int elementsPerWord = 4 / getBytesPerPixel();
        int padding = width % elementsPerWord;
        if (padding != 0) {
            width += elementsPerWord - padding;
        }
        return width;
    }

    /**
     * If the data needs to be padded for alignment in GL, this handles that.
     * 
     * @param buffer
     *            - the original data
     * @param datasetBounds
     *            - the bounds of this data within the totalDatasetDimensions
     * @return a buffer padded appropriately, or a the same buffer if it was
     *         good
     */
    protected Buffer handleBufferSizing(GLColorMapData data, Buffer buffer,
            int[] dimensions) {
        int sliceWidth = dimensions[0] * getValuesPerPixel();
        int sliceHeight = dimensions[1];
        int paddedSliceWidth = getAlignedWidth(sliceWidth);

        int totalDataSize = buffer.capacity();

        if (totalDataSize != paddedSliceWidth * sliceHeight) {
            if (totalDataSize != sliceWidth * sliceHeight) {
                // Im not sure what shape this data is in, so just panic.
                throw new IllegalStateException("Buffer is wrong size("
                        + totalDataSize + ") for data dimensions(" + sliceWidth
                        + "x" + sliceHeight + ")");
            }

            Buffer newBuffer = getCopybackBuffer(data);

            // Position the data at the start of the first row
            buffer.position(0);
            for (int i = 0; i < sliceHeight; i += 1) {
                // Advance to the correct position in this row.
                buffer.limit(buffer.position() + sliceWidth);
                copyRow(buffer, newBuffer, paddedSliceWidth - sliceWidth);
                buffer.limit(buffer.capacity());
            }
            buffer = newBuffer.rewind();
        }
        return buffer;
    }
}
