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
 * Wraps an int array as a {@link DataSource} and {@link DataDestination}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class IntArrayWrapper extends DataWrapper1D {

    protected final int[] array;

    public IntArrayWrapper(int[] array, GeneralGridGeometry geometry) {
        super(geometry);
        this.array = array;
    }

    public IntArrayWrapper(int[] array, int nx, int ny) {
        super(nx, ny);
        this.array = array;
    }

    public IntArrayWrapper(int nx, int ny) {
        this(new int[nx * ny], nx, ny);
    }

    public IntArrayWrapper(GeneralGridGeometry geometry) {
        // assume this is going to be a destination and avoid passing
        // geometry to super to save time on checking for wrapping.
        this(geometry.getGridRange().getSpan(0), geometry.getGridRange()
                .getSpan(1));
    }

    public int[] getArray() {
        return array;
    }

    @Override
    protected double getDataValueInternal(int index) {
        return array[index];
    }

    @Override
    protected void setDataValueInternal(double dataValue, int index) {
        array[index] = (int) dataValue;
    }

}
