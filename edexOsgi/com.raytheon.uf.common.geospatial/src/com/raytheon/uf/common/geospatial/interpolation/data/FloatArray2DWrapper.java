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
 * 
 * 2-dimensional float array data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class FloatArray2DWrapper extends AbstractDataWrapper {

    protected final float[][] array;

    public FloatArray2DWrapper(float[][] array, GeneralGridGeometry geometry) {
        super(geometry);
        this.array = array;
    }

    public FloatArray2DWrapper(float[][] array, int nx, int ny) {
        super(nx, ny);
        this.array = array;
    }

    public FloatArray2DWrapper(int nx, int ny) {
        this(new float[ny][nx], nx, ny);
    }

    public FloatArray2DWrapper(GeneralGridGeometry geometry) {
        this(geometry.getGridRange().getSpan(0), geometry.getGridRange()
                .getSpan(1));
    }

    public float[][] getArray() {
        return array;
    }

    @Override
    public void setDataValueInternal(double dataValue, int x, int y) {
        array[y][x] = (float) dataValue;
    }

    @Override
    protected double getDataValueInternal(int x, int y) {
        return array[y][x];
    }

}