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

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.transform.IdentityTransform;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeneralDerivedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

/**
 * Convenience geospatial math transforms
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 30, 2008            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TransformFactory {

    private static DefaultMathTransformFactory factory = new DefaultMathTransformFactory();

    /**
     * Go from a grid crs projection to another grid's cell coordinates
     * 
     * @param gridGeometrySrc
     * @param gridGeometryDest
     * @return
     * @throws TransformException
     * @throws FactoryException
     */
    public static MathTransform gridCRSToGridCell(
            GeneralGridGeometry gridGeometrySrc,
            GeneralGridGeometry gridGeometryDest, PixelInCell destOrientation)
            throws TransformException, FactoryException {

        if (gridGeometryDest.equals(gridGeometrySrc)) {
            // Is in the same grid geometryTransformFactory
            // grid_crs -> grid_cell_space
            return CRSCache.getInstance().getCoordinateSystemToGrid(
                    gridGeometryDest, destOrientation);
        }

        // An entirely different grid geometry (slower)
        // grid_crs_1 -> grid_crs_2 -> grid_cell_space_2
        MathTransform mt1 = CRSCache.getInstance().findMathTransform(
                gridGeometrySrc.getCoordinateReferenceSystem(),
                gridGeometryDest.getCoordinateReferenceSystem());

        MathTransform mt2 = CRSCache.getInstance().getCoordinateSystemToGrid(
                gridGeometryDest, destOrientation);
        return factory.createConcatenatedTransform(mt1, mt2);
    }

    /**
     * Go from a grid crs coordinate of any projection, to a world pixel
     * coordinate of the map
     * 
     * (A convenience method for gridCRSToGridCell)
     * 
     * 
     * @param gridGeometry
     * @param descriptor
     * @return
     * @throws TransformException
     * @throws FactoryException
     */
    public static MathTransform gridCRSToWorldPixel(
            GeneralGridGeometry gridGeometry, GeneralGridGeometry mapGeometry)
            throws TransformException, FactoryException {
        return gridCRSToGridCell(gridGeometry, mapGeometry,
                PixelInCell.CELL_CENTER);
    }

    /**
     * Go from grid cell to lat lon
     * 
     * @param gridGeometry
     * @param orientation
     * @return
     * @throws TransformException
     * @throws FactoryException
     */
    public static MathTransform gridToLatLon(GeneralGridGeometry gridGeometry,
            PixelInCell orientation) throws TransformException,
            FactoryException {
        MathTransform mt1 = CRSCache.getInstance().getGridToCoordinateSystem(
                gridGeometry, orientation);
        MathTransform mt2 = CRSCache.getInstance().getTransformToLatLon(
                gridGeometry.getCoordinateReferenceSystem());
        return factory.createConcatenatedTransform(mt1, mt2);
    }

    /**
     * Go from lat lon to grid cell
     * 
     * @param gridGeometry
     * @param orientation
     * @return
     * @throws TransformException
     * @throws FactoryException
     */
    public static MathTransform latLonToGrid(GeneralGridGeometry gridGeometry,
            PixelInCell orientation) throws TransformException,
            FactoryException {

        MathTransform mt1 = CRSCache.getInstance().getTransformFromLatLon(
                gridGeometry.getCoordinateReferenceSystem());
        MathTransform mt2 = CRSCache.getInstance().getCoordinateSystemToGrid(
                gridGeometry, orientation);
        return factory.createConcatenatedTransform(mt1, mt2);
    }

    /**
     * Go from one grid cell coordinate system to another
     * 
     * @param gridGeometrySrc
     * @param gridGeometryDest
     * @return
     * @throws TransformException
     * @throws FactoryException
     */
    public static MathTransform gridCellToGridCell(
            GeneralGridGeometry gridGeometrySrc, PixelInCell srcOrientation,
            GeneralGridGeometry gridGeometryDest, PixelInCell destOrientation)
            throws TransformException, FactoryException {

        if (gridGeometrySrc.equals(gridGeometryDest)) {
            return IdentityTransform.create(2);
        }

        MathTransform mt1 = CRSCache.getInstance().getGridToCoordinateSystem(
                gridGeometrySrc, srcOrientation);
        MathTransform mt2 = gridCRSToGridCell(gridGeometrySrc,
                gridGeometryDest, destOrientation);

        return factory.createConcatenatedTransform(mt1, mt2);
    }

    /**
     * Constructs a transform from the "world" CRS of the target geometry to the
     * grid of the targetGeometry. Null is a valid return and indicates there is
     * no "world" CRS to convert from and all conversions should be from
     * targetGeometry CRS to grid
     * 
     * @param targetGeometry
     * @param cellType
     * @return
     * @throws FactoryException
     */
    public static MathTransform worldToGrid(GeneralGridGeometry targetGeometry,
            PixelInCell cellType) throws FactoryException {
        CoordinateReferenceSystem crs = targetGeometry.getEnvelope()
                .getCoordinateReferenceSystem();
        if (crs instanceof GeneralDerivedCRS) {
            GeneralDerivedCRS projCRS = (GeneralDerivedCRS) crs;
            CoordinateReferenceSystem worldCRS = projCRS.getBaseCRS();
            MathTransform worldToCRS = CRSCache.getInstance()
                    .findMathTransform(worldCRS, crs);
            try {
                MathTransform crsToPixel = targetGeometry
                        .getGridToCRS(cellType).inverse();
                return factory.createConcatenatedTransform(worldToCRS,
                        crsToPixel);
            } catch (Exception e) {
                throw new FactoryException(e);
            }
        }
        return null;
    }
}
