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
package com.raytheon.uf.common.numeric.array;

import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;

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
public class FloatArray2DWrapper implements DataSource, DataDestination {

    protected final int nx;

    protected final int ny;

    protected final float[][] array;

    public FloatArray2DWrapper(float[][] array, int nx, int ny) {
        this.nx = nx;
        this.ny = ny;
        this.array = array;
    }

    public FloatArray2DWrapper(int nx, int ny) {
        this(new float[ny][nx], nx, ny);
    }

    protected boolean validateRange(int x, int y) {
        if (y < 0 || y > ny - 1) {
            return false;
        } else if (x < 0 || x > nx - 1) {
            return false;
        }
        return true;
    }

    public float[][] getArray() {
        return array;
    }

    @Override
    public void setDataValue(double dataValue, int x, int y) {
        if (validateRange(x, y)) {
            array[y][x] = (float) dataValue;
        }
    }

    @Override
    public double getDataValue(int x, int y) {
        if (validateRange(x, y)) {
            return array[y][x];
        }
        return Double.NaN;
    }

}