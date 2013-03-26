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


import com.raytheon.uf.common.colormap.image.ColorMapData;
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

        /**
         * @param dataType
         * @param dataBounds
         */
        public ColorMapData(ColorMapDataType dataType, int[] dimensions) {
            this.buffer = getBuffer(dataType, dimensions);
            this.dimensions = dimensions;
            this.dataType = dataType;
        }

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
