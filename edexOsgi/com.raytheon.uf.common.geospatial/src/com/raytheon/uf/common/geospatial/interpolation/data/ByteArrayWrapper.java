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
package com.raytheon.uf.common.geospatial.interpolation.data;

import org.geotools.coverage.grid.GeneralGridGeometry;

/**
 * {@link AbstractDataWrapper} implementation for byte array data. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ByteArrayWrapper extends DataWrapper1D {

    // The wrapped byte array data.
    protected final byte[] array;

    /**
     * Wrap a byte array using a specified geometry.
     * @param array Byte array data to be wrapped.
     * @param geometry A {@link GeneralGridGeometry} that will be used to discover
     * the shape of the input data.
     */
    public ByteArrayWrapper(byte[] array, GeneralGridGeometry geometry) {
        super(geometry);
        this.array = array;
    }

    /**
     * Wrap a byte array using given x and y axis dimensions.
     * @param array Byte array data to be wrapped.
     * @param nx Number of elements on the x axis.
     * @param ny Number of elements on the y axis.
     */
    public ByteArrayWrapper(byte[] array, int nx, int ny) {
        super(nx, ny);
        this.array = array;
    }

    /**
     * Create an instance with a byte array using given x and y axis dimensions.
     * @param nx Number of elements on the x axis.
     * @param ny Number of elements on the y axis.
     */
    public ByteArrayWrapper(int nx, int ny) {
        this(new byte[nx * ny], nx, ny);
    }

    /**
     * Create an instance with a byte array using a specified geometry.
     * @param geometry A {@link GeneralGridGeometry} that will be used to discover
     * the shape of the input data.
     */
    public ByteArrayWrapper(GeneralGridGeometry geometry) {
        // assume this is going to be a destination and avoid passing
        // geometry to super to save time on checking for wrapping.
        this(geometry.getGridRange().getSpan(0), geometry.getGridRange()
                .getSpan(1));
    }

    /**
     * Get a reference to the internal wrapped data.
     * @return The internal byte array data.
     */
    public byte[] getArray() {
        return array;
    }

    /**
     * Get the value of the internal data at a specified position.
     * @param index Position within the internal data to get.
     * @return The value of the internal data.
     */
    @Override
    protected double getDataValueInternal(int index) {
        return array[index];
    }

    /**
     * Set the value of the internal data at a specified position.
     * @param dataValue A value to set at the given index.
     * @param index Position within the internal data to set.
     */
    @Override
    public void setDataValueInternal(double dataValue, int index) {
        array[index] = (byte) dataValue;
    }
}
