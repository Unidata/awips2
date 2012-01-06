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
package com.raytheon.uf.common.geospatial;

import java.awt.Point;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utilies related to points in space
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PointUtil {

    /**
     * Determines the index in a 2 dimensional grid of the specified lat/lon
     * coordinate
     * 
     * @param coordinateToFind
     *            the coordinate to find in lat/lon
     * @param crs
     *            the crs of the grid
     * @param mapGeometry
     *            the geometry of the grid
     * @return the x,y index of the coordinate in the grid
     * @throws FactoryException
     * @throws NoninvertibleTransformException
     * @throws InvalidGridGeometryException
     * @throws VizException
     */
    public static Point determineIndex(Coordinate coordinateToFind,
            CoordinateReferenceSystem crs, GridGeometry2D mapGeometry)
            throws Exception {
        DirectPosition2D resultPoint = PointUtil.determineExactIndex(
                coordinateToFind, crs, mapGeometry);
        int x = (int) Math.round(resultPoint.x);
        int y = (int) Math.round(resultPoint.y);
        return new Point(x, y);
    }

    public static DirectPosition2D determineExactIndex(
            Coordinate coordinateToFind, CoordinateReferenceSystem crs,
            GridGeometry2D mapGeometry) throws FactoryException,
            InvalidGridGeometryException, TransformException {
        MathTransform latLonToCrs = MapUtil.getTransformFromLatLon(crs);
        MathTransform crsToGrid = mapGeometry.getGridToCRS().inverse();
        float fx = (float) coordinateToFind.x;
        float fy = (float) coordinateToFind.y;
        float[] srcPoints = { fx, fy, fx + 360, fy, fx - 360, fy };
        DirectPosition2D ptPosition = new DirectPosition2D();

        latLonToCrs.transform(srcPoints, 0, srcPoints, 0, 3);

        // use the least positive x value

        crsToGrid.transform(srcPoints, 0, srcPoints, 0, 3);
        DirectPosition2D resultPoint = new DirectPosition2D(srcPoints[0],
                srcPoints[1]);
        for (int i = 0; i < srcPoints.length; i = i + 2) {
            resultPoint.y = srcPoints[i + 1];
            if (srcPoints[i] > 0
                    && (resultPoint.x <= 0 || srcPoints[i] < ptPosition.x)) {
                resultPoint.x = srcPoints[i];
            }
        }

        return resultPoint;
    }

    public static Coordinate determineLatLon(Point pointToFind,
            CoordinateReferenceSystem crs, GridGeometry2D mapGeometry)
            throws Exception {
        MathTransform crsToLatLon = MapUtil.getTransformToLatLon(crs);
        MathTransform gridToCRS = mapGeometry.getGridToCRS();
        DirectPosition2D ptPosition = new DirectPosition2D(pointToFind.x,
                pointToFind.y);
        gridToCRS.transform(ptPosition, ptPosition);
        crsToLatLon.transform(ptPosition, ptPosition);
        return new Coordinate(ptPosition.x, ptPosition.y);
    }

}
