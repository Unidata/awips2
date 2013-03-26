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
package com.raytheon.uf.common.colormap.image;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

/**
 * 
 * Container for colormap data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2013            bsteffen    Seperate from IColorMapDataRetrievalCallback
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ColorMapData {

    public static enum ColorMapDataType {
        BYTE, SIGNED_BYTE, UNSIGNED_SHORT, SHORT, INT, FLOAT;
    }

    private Buffer buffer;

    private int[] dimensions;

    private ColorMapData.ColorMapDataType dataType;

    /**
     * 
     * @param buffer
     * @param dataSetBounds
     */
    public ColorMapData(Buffer buffer, int[] dimensions) {
        this(buffer, dimensions, getDataType(buffer));
    }

    /**
     * @param buffer
     * @param dataBounds
     * @param dataType
     */
    public ColorMapData(Buffer buffer, int[] dimensions,
            ColorMapData.ColorMapDataType dataType) {
        this.buffer = buffer;
        this.dimensions = dimensions;
        this.dataType = dataType;
    }

    /**
     * @param dataType
     * @param dataBounds
     */
    public ColorMapData(ColorMapDataType dataType, int[] dimensions) {
        this.buffer = getBuffer(dataType, dimensions);
        this.dimensions = dimensions;
        this.dataType = dataType;
    }

    public Buffer getBuffer() {
        return buffer;
    }

    public int[] getDimensions() {
        return dimensions;
    }

    public ColorMapData.ColorMapDataType getDataType() {
        return dataType;
    }

    private static ColorMapData.ColorMapDataType getDataType(Buffer buffer) {
        if (buffer instanceof FloatBuffer) {
            return ColorMapData.ColorMapDataType.FLOAT;
        } else if (buffer instanceof IntBuffer) {
            return ColorMapData.ColorMapDataType.INT;
        } else if (buffer instanceof ShortBuffer) {
            return ColorMapData.ColorMapDataType.SHORT;
        } else if (buffer instanceof ByteBuffer) {
            return ColorMapData.ColorMapDataType.BYTE;
        }
        throw new RuntimeException("Could not find ColorMapDataType for "
                + buffer);
    }

    private static Buffer getBuffer(ColorMapDataType dataType, int[] dimensions) {
        int size = 1;
        for (int i : dimensions) {
            size *= i;
        }
        switch (dataType) {
        case BYTE:
        case SIGNED_BYTE:
            return ByteBuffer.allocate(size);
        case SHORT:
        case UNSIGNED_SHORT:
            return ShortBuffer.allocate(size);
        case FLOAT:
            return FloatBuffer.allocate(size);
        case INT:
            return IntBuffer.allocate(size);
        default:
            throw new RuntimeException("Could not find Buffer for " + dataType);
        }
    }
}