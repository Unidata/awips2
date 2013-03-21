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
package com.raytheon.uf.viz.core.data;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Callback interface for initializing a Buffer object from scratch.
 * Implementing classes should create Buffer from scratch when getData is called
 * and NOT keep in memory. NOTE: IT IS HIGHLY RECOMMENDED TO PROPERLY IMPLEMENT
 * hashCode() as well as equals() methods to maximize caching ability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            mschenke     Initial creation
 * Mar 21, 2013 1806       bsteffen    Add ColorMapData constructor that
 *                                     creates buffer from the dataType.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IColorMapDataRetrievalCallback {

    public static enum ColorMapDataType {
        BYTE, SIGNED_BYTE, UNSIGNED_SHORT, SHORT, INT, FLOAT;
    }

    public static class ColorMapData {
        private Buffer buffer;

        private int[] dimensions;

        private ColorMapDataType dataType;

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
                ColorMapDataType dataType) {
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

        public ColorMapDataType getDataType() {
            return dataType;
        }

        private static ColorMapDataType getDataType(Buffer buffer) {
            if (buffer instanceof FloatBuffer) {
                return ColorMapDataType.FLOAT;
            } else if (buffer instanceof IntBuffer) {
                return ColorMapDataType.INT;
            } else if (buffer instanceof ShortBuffer) {
                return ColorMapDataType.SHORT;
            } else if (buffer instanceof ByteBuffer) {
                return ColorMapDataType.BYTE;
            }
            throw new RuntimeException("Could not find ColorMapDataType for "
                    + buffer);
        }

        private static Buffer getBuffer(ColorMapDataType dataType,
                int[] dimensions) {
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
                throw new RuntimeException("Could not find Buffer for "
                        + dataType);
            }

        }
    }

    /**
     * Get the ColorMapData. IMPORTANT NOTE: This method should retrieve the
     * ColorMapData from wherever it lives. ColorMapData objects should not be
     * stored as member variables of the classes implementing this interface.
     * 
     * @return
     * @throws VizException
     */
    public ColorMapData getColorMapData() throws VizException;

}
