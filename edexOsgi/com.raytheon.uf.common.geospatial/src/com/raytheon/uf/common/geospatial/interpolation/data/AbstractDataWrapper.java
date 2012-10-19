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
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

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
        checkForWrapping(geometry);
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

    // Attempt to detect the case where a geographic coordinate reference
    // system wraps around the world so that values out of range on the
    // X-axis can be retrieved from the other side of the grid. If this is
    // the case the sourceWrapX value will be set to the number of grid
    // cells that are needed to wrap all the way around the world.
    protected void checkForWrapping(GeneralGridGeometry geometry) {
        try {
            CoordinateReferenceSystem sourceCRS = geometry
                    .getCoordinateReferenceSystem();
            MathTransform grid2crs = geometry
                    .getGridToCRS(PixelInCell.CELL_CENTER);
            MathTransform crs2LatLon = CRS.findMathTransform(sourceCRS,
                    DefaultGeographicCRS.WGS84);
            // Although theoretically any two points would yield the same
            // result, nx is chosen as the x coordinate because it overcomes
            // some of the normalization performed in geotools math transforms.
            // For most math transforms, geotools will normalize the LatLon
            // values into into the range centralMeridian +/- 180. In these
            // cases the 360 degree shift performed later is completely
            // worthless since geotools will normalize the points into the
            // same range regardless of how it is shifted. For a worldwide grid
            // the nx coordinate is guaranteed to be just slightly over the
            // normalized range, so that when the transform normalizes it will
            // use a point that is -360 degrees away from the original point.
            // This means that even though the -360 degree shift we apply is
            // worthless, the normalization process actually applies an
            // equivelant shift that yields the correct result. The end result
            // is that whether the transform normalizes or not there is always a
            // shift of -360 degrees for all worldwide grids.

            // Start with two points in grid space, one on each corner side of
            // the y direction.
            DirectPosition2D corner1 = new DirectPosition2D(nx, 0);
            DirectPosition2D corner2 = new DirectPosition2D(nx, ny - 1);
            // transform the points to crs space.
            grid2crs.transform(corner1, corner1);
            grid2crs.transform(corner2, corner2);
            // and then to latLon space
            crs2LatLon.transform(corner1, corner1);
            crs2LatLon.transform(corner2, corner2);
            // shift the points by 360 degrees because the goal is to know how
            // many grid cells exist in one 360 roll of longitude.
            corner1.x = corner1.x - 360;
            corner2.x = corner2.x - 360;
            // transform back to crs.
            crs2LatLon.inverse().transform(corner1, corner1);
            crs2LatLon.inverse().transform(corner2, corner2);
            // and back to grid space
            grid2crs.inverse().transform(corner1, corner1);
            grid2crs.inverse().transform(corner2, corner2);
            // the difference between the starting x value and the current x
            // value is the number
            int sourceWrapX = (int) (nx - corner1.x);
            // In order to wrap then the transformed point x value should be
            // on the other side of the grid and the y value should not have
            // changed significantly. Additionally the wrapped x value
            // should fall exactly on a grid cell.
            if (corner1.x > nx - 1) {
                return;
            } else if (Math.abs(corner1.y - 0) > 0.0001) {
                return;
            } else if (Math.abs(corner2.y - ny + 1) > 0.0001) {
                return;
            } else if (Math.abs(corner1.x + sourceWrapX - nx) > 0.0001) {
                return;
            } else if (Math.abs(corner2.x + sourceWrapX - nx) > 0.0001) {
                return;
            } else {
                this.wrapX = sourceWrapX;
            }

        } catch (Exception e) {
            // if anything goes wrong in this process just assume we don't
            // wrap the x axis, thats not a big deal and it is normal for
            // non geographic coordinate systems.
            ;
        }
    }
}