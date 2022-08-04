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
package com.raytheon.uf.common.xmrg.hrap;

import java.awt.Point;
import java.awt.Rectangle;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2015  17098     snaples     Adjust Pixel Orientation from Lower Left to Center
 * Mar 09, 2016  19733     snaples     Adject back to Lower Left after changing transforms.
 * Sep 15, 2017  6407      bkowal      Now possible to retrieve the Grid Mapper and Grid Geometry
 *                                     for a HRAP Sub Grid.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class HRAPSubGrid implements ISpatialObject {
    private static final long serialVersionUID = 1L;

    private CoordinateReferenceSystem crs;

    private Rectangle extent;

    private Geometry geometry;

    private HRAP hrap;

    private Point llGridPoint;

    private Point ulGridPoint;

    private Point urGridPoint;

    private Point lrGridPoint;

    private Coordinate llLatLonCoord;

    private Coordinate ulLatLonCoord;

    private Coordinate urLatLonCoord;

    private Coordinate lrLatLonCoord;

    /**
     * @param gridSize
     * @param domainOrigin
     * @param domainExtent
     * @throws VizException
     */
    public HRAPSubGrid(Rectangle extent) throws HrapConversionException {
        hrap = HRAP.getInstance();
        this.extent = extent;
        this.crs = hrap.getCrs();

        this.llGridPoint = new Point((int) extent.getMinX(),
                (int) extent.getMinY());
        this.ulGridPoint = new Point((int) extent.getMinX(),
                (int) extent.getMaxY());
        this.urGridPoint = new Point((int) extent.getMaxX(),
                (int) extent.getMaxY());
        this.lrGridPoint = new Point((int) extent.getMaxX(),
                (int) extent.getMinY());

        // transform the grid corners from grid coordinates to latLon
        this.llLatLonCoord = hrap.gridCoordinateToLatLon(llGridPoint,
                PixelOrientation.LOWER_LEFT);
        this.ulLatLonCoord = hrap.gridCoordinateToLatLon(ulGridPoint,
                PixelOrientation.LOWER_LEFT);
        this.urLatLonCoord = hrap.gridCoordinateToLatLon(urGridPoint,
                PixelOrientation.LOWER_LEFT);
        this.lrLatLonCoord = hrap.gridCoordinateToLatLon(lrGridPoint,
                PixelOrientation.LOWER_LEFT);
        Coordinate[] corners = new Coordinate[] { this.llLatLonCoord,
                this.ulLatLonCoord, this.urLatLonCoord, this.lrLatLonCoord,
                this.llLatLonCoord };

        this.geometry = MapUtil.getPolygon(corners);
    }

    /**
     * @param gridSize
     * @param domainOrigin
     * @param domainExtent
     * @throws VizException
     */
    public HRAPSubGrid(Rectangle extent, int gridFactor) throws HrapConversionException {
        hrap = HRAP.getInstance(gridFactor);
        this.extent = extent;

        this.crs = hrap.getCrs();

        this.llGridPoint = new Point((int) extent.getMinX(),
                (int) extent.getMinY());
        this.ulGridPoint = new Point((int) extent.getMinX(),
                (int) extent.getMaxY());
        this.urGridPoint = new Point((int) extent.getMaxX(),
                (int) extent.getMaxY());
        this.lrGridPoint = new Point((int) extent.getMaxX(),
                (int) extent.getMinY());

        // transform the grid corners from grid coordinates to latLon
        this.llLatLonCoord = hrap.gridCoordinateToLatLon(llGridPoint,
                PixelOrientation.LOWER_LEFT);
        this.ulLatLonCoord = hrap.gridCoordinateToLatLon(ulGridPoint,
                PixelOrientation.LOWER_LEFT);
        this.urLatLonCoord = hrap.gridCoordinateToLatLon(urGridPoint,
                PixelOrientation.LOWER_LEFT);
        this.lrLatLonCoord = hrap.gridCoordinateToLatLon(lrGridPoint,
                PixelOrientation.LOWER_LEFT);
        Coordinate[] corners = new Coordinate[] { this.llLatLonCoord,
                this.ulLatLonCoord, this.urLatLonCoord, this.lrLatLonCoord,
                this.llLatLonCoord };

        this.geometry = MapUtil.getPolygon(corners);
    }

    public GridToEnvelopeMapper getGridMapper() throws HrapConversionException {
        GridToEnvelopeMapper gridMapper = null;
        try {
            // transform the projection corner points to CRS units
            MathTransform mt = MapUtil.getTransformFromLatLon(getCrs());
            double[] output = new double[4];
            mt.transform(
                    new double[] { llLatLonCoord.x, llLatLonCoord.y,
                            urLatLonCoord.x, urLatLonCoord.y },
                    0, output, 0, 2);

            // create a grid geometry for the projection
            GeneralEnvelope userRange = new GeneralEnvelope(2);
            userRange.setCoordinateReferenceSystem(getCrs());
            userRange.setRange(0, Math.min(output[0], output[2]),
                    Math.max(output[0], output[2]));
            userRange.setRange(1, Math.min(output[1], output[3]),
                    Math.max(output[1], output[3]));

            GeneralGridEnvelope gridRange = new GeneralGridEnvelope(
                    new int[] { 0, 0 },
                    new int[] { (urGridPoint.x - llGridPoint.x),
                            (urGridPoint.y - llGridPoint.y) },
                    false);

            gridMapper = new GridToEnvelopeMapper(gridRange, userRange);
            gridMapper.setEnvelope(userRange);
            gridMapper.setGridRange(gridRange);
            gridMapper.setPixelAnchor(PixelInCell.CELL_CORNER);
            gridMapper.setReverseAxis(new boolean[] { false, true });
        } catch (Exception e) {
            throw new HrapConversionException("Unable to create HRAP grid mapper.", e);
        }

        return gridMapper;
    }

    public GridGeometry2D getGridGeometry() throws HrapConversionException {
        return new GridGeometry2D(getGridMapper().getGridRange(),
                getGridMapper().createTransform(), getCrs());
    }

    public Rectangle getExtent() {
        return extent;
    }

    public void setExtent(Rectangle extent) {
        this.extent = extent;
    }

    @Override
    public CoordinateReferenceSystem getCrs() {
        return crs;
    }

    @Override
    public Geometry getGeometry() {
        return geometry;
    }

    @Override
    public Integer getNx() {
        return extent.width;
    }

    @Override
    public Integer getNy() {
        return extent.height;
    }

    public HRAP getHRAP() {
        return hrap;
    }
}