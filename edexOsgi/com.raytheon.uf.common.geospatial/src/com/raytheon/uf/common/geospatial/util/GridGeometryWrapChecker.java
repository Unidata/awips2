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
package com.raytheon.uf.common.geospatial.util;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

/**
 * Provide utility methods for determining if a grid geometry is world wide and
 * wrapping so that out of range on the X-axis can be retrieved from the other
 * side of the grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridGeometryWrapChecker {

    private static double TOLERANCE = 0.00001;

    /**
     * 
     * Attempt to detect the case where a geographic coordinate reference system
     * wraps around the world so that values out of range on the X-axis can be
     * retrieved from the other side of the grid.
     * 
     * If the provided grid geometry wraps than the result will be a positive
     * value representing the number of grid cells used to wrap around the
     * world. The return value can be used to extend the range of the grid
     * infinitely in either direction by reusing column values at an interval of
     * the return value. For example if a grid normally has an x range from 0 to
     * 359 and the result of this method is 360 then the range can be extended
     * to 360 by reusing the values from the 0 column, in fact any integer
     * column can be referenced by simply taking columnNumber % 360 to get a
     * value within the original valid range.
     * 
     * If the grid does not wrap around then the result is -1;
     * 
     * @param geometry
     * @return
     */
    public static int checkForWrapping(GeneralGridGeometry geometry) {
        try {
            int nx = geometry.getGridRange().getSpan(0);
            int ny = geometry.getGridRange().getSpan(1);
            CoordinateReferenceSystem sourceCRS = geometry
                    .getCoordinateReferenceSystem();
            MathTransform grid2crs = geometry
                    .getGridToCRS(PixelInCell.CELL_CENTER);
            MathTransform crs2LatLon = CRS.findMathTransform(sourceCRS,
                    DefaultGeographicCRS.WGS84);
            MathTransform latLon2crs = crs2LatLon.inverse();
            MathTransform crs2grid = grid2crs.inverse();
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
            latLon2crs.transform(corner1, corner1);
            latLon2crs.transform(corner2, corner2);
            // and back to grid space
            crs2grid.transform(corner1, corner1);
            crs2grid.transform(corner2, corner2);
            // the difference between the starting x value and the current x
            // value is the number
            int sourceWrapX = (int) (nx - corner1.x);
            // In order to wrap then the transformed point x value should be
            // on the other side of the grid and the y value should not have
            // changed significantly. Additionally the wrapped x value
            // should fall exactly on a grid cell.
            if (corner1.x > nx - 1) {
                return -1;
            } else if (Math.abs(corner1.y - 0) > TOLERANCE) {
                return -1;
            } else if (Math.abs(corner2.y - ny + 1) > TOLERANCE) {
                return -1;
            } else if (Math.abs(corner1.x + sourceWrapX - nx) > TOLERANCE) {
                return -1;
            } else if (Math.abs(corner2.x + sourceWrapX - nx) > TOLERANCE) {
                return -1;
            } else {
                return sourceWrapX;
            }
        } catch (FactoryException e) {
            // Every known crs which can world wrap does not throw factory
            // exceptions when getting transformation so if an exception is
            // thrown assume it is because the grid does not world wrap.
            return -1;
        } catch (TransformException e) {
            // this exception is expected for some grids that do not world
            // wrap, often because while checking transformation is performed
            // for invalid coordinates.
            return -1;
        }
    }

}
