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
 * Wraps a short array as a {@link DataSource} and {@link DataDestination}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ShortArrayWrapper extends DataWrapper1D {

    protected final short[] array;

    public ShortArrayWrapper(short[] array, GeneralGridGeometry geometry) {
        super(geometry);
        this.array = array;
    }

    public ShortArrayWrapper(short[] array, int nx, int ny) {
        super(nx, ny);
        this.array = array;
    }

    public ShortArrayWrapper(int nx, int ny) {
        this(new short[nx * ny], nx, ny);
    }

    public ShortArrayWrapper(GeneralGridGeometry geometry) {
        // assume this is going to be a destination and avoid passing
        // geometry to super to save time on checking for wrapping.
        this(geometry.getGridRange().getSpan(0), geometry.getGridRange()
                .getSpan(1));
    }

    public short[] getArray() {
        return array;
    }

    @Override
    protected double getDataValueInternal(int index) {
        return array[index];
    }

    @Override
    public void setDataValueInternal(double dataValue, int index) {
        array[index] = (short) dataValue;
    }

}
