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

import java.awt.Rectangle;
import java.lang.reflect.Constructor;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;

/**
 * 
 * Abstract class for any data implementation that can act as both a source and
 * destination.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            bsteffen     Initial creation
 * Nov 19, 2013  2393      bclement     added createNew method
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractDataWrapper implements DataSource,
        DataDestination {

    protected final int nx;

    protected final int ny;

    protected int wrapX = -1;

    protected double minValid = Double.NEGATIVE_INFINITY;

    protected double maxValid = Double.POSITIVE_INFINITY;

    protected double fillValue = Double.NaN;

    public AbstractDataWrapper(GeneralGridGeometry geometry) {
        this.nx = geometry.getGridRange().getSpan(0);
        this.ny = geometry.getGridRange().getSpan(1);
        this.wrapX = GridGeometryWrapChecker.checkForWrapping(geometry);
    }

    public AbstractDataWrapper(int nx, int ny) {
        this.nx = nx;
        this.ny = ny;
    }

    public void setValidRange(double minValid, double maxValid) {
        this.minValid = minValid;
        this.maxValid = maxValid;
    }

    public void setFillValue(double fillValue) {
        this.fillValue = fillValue;
    }

    @Override
    public double getDataValue(int x, int y) {
        if (y < 0 || y > ny - 1) {
            // outside y range
            return Double.NaN;
        } else if (x < 0 || x > nx - 1) {
            // outside x range
            if (wrapX > 0) {
                // attempt to wrap if this is a wrapping grid.
                x = (x + wrapX) % wrapX;
                if (x < 0 || x > nx - 1) {
                    return Double.NaN;
                }
            } else {
                return Double.NaN;
            }
        }
        double val = getDataValueInternal(x, y);
        if (val == fillValue || val < minValid || val > maxValid) {
            // skip outside valid range or fill value
            val = Double.NaN;
        }
        return val;
    }

    @Override
    public void setDataValue(double dataValue, int x, int y) {
        if (Double.isNaN(dataValue)) {
            dataValue = fillValue;
        }
        setDataValueInternal(dataValue, x, y);
    }

    protected abstract double getDataValueInternal(int x, int y);

    protected abstract void setDataValueInternal(double dataValue, int x, int y);

    /**
     * Create a new data wrapper of type c with the provided size
     * 
     * @param c
     *            desired implementation class
     * @param size
     * @return
     * @throws IllegalArgumentException
     */
    public static <T extends AbstractDataWrapper> T createNew(
            Class<? extends T> c, Rectangle size)
            throws IllegalArgumentException {
        try {
            Constructor<? extends T> constructor = c.getConstructor(int.class,
                    int.class);
            return constructor.newInstance(size.width, size.height);
        } catch (Exception e) {
            throw new IllegalArgumentException(
                    "Unable to instatiate instance of class: " + c, e);
        }
    }

}