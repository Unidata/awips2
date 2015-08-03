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
package com.raytheon.uf.common.hydro.spatial;

import java.awt.Rectangle;

import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2015  17098     snaples     Adjust Pixel Orientation from Lower Left to Center
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class HRAPSubGrid implements ISpatialObject {
    private static final long serialVersionUID = 1L;

    private CoordinateReferenceSystem crs;

    private Rectangle extent;

    private Geometry geometry;

    private HRAP hrap;

    /**
     * @param gridSize
     * @param domainOrigin
     * @param domainExtent
     * @throws VizException
     */
    public HRAPSubGrid(Rectangle extent) throws Exception {
        hrap = HRAP.getInstance();
        this.extent = extent;
        this.crs = hrap.getCrs();

        // transform the grid corners from grid coordinates to latLon
        Coordinate ll = hrap.gridCoordinateToLatLon(
                new Coordinate(extent.getMinX(), extent.getMinY()),
                PixelOrientation.CENTER);
        Coordinate ul = hrap.gridCoordinateToLatLon(
                new Coordinate(extent.getMinX(), extent.getMaxY()),
                PixelOrientation.CENTER);
        Coordinate ur = hrap.gridCoordinateToLatLon(
                new Coordinate(extent.getMaxX(), extent.getMaxY()),
                PixelOrientation.CENTER);
        Coordinate lr = hrap.gridCoordinateToLatLon(
                new Coordinate(extent.getMaxX(), extent.getMinY()),
                PixelOrientation.CENTER);
        Coordinate[] corners = new Coordinate[] { ll, ul, ur, lr, ll };

        this.geometry = MapUtil.getPolygon(corners);
    }

    /**
     * @param gridSize
     * @param domainOrigin
     * @param domainExtent
     * @throws VizException
     */
    public HRAPSubGrid(Rectangle extent, int gridFactor) throws Exception {
        hrap = HRAP.getInstance(gridFactor);
        this.extent = extent;

        this.crs = hrap.getCrs();

        // transform the grid corners from grid coordinates to latLon
        Coordinate[] corners = new Coordinate[] {
                hrap.gridCoordinateToLatLon(new Coordinate(extent.getMinX(),
                        extent.getMinY()), PixelOrientation.CENTER),
                hrap.gridCoordinateToLatLon(new Coordinate(extent.getMinX(),
                        extent.getMaxY()), PixelOrientation.CENTER),
                hrap.gridCoordinateToLatLon(new Coordinate(extent.getMaxX(),
                        extent.getMaxY()), PixelOrientation.CENTER),
                hrap.gridCoordinateToLatLon(new Coordinate(extent.getMaxX(),
                        extent.getMinY()), PixelOrientation.CENTER),
                hrap.gridCoordinateToLatLon(new Coordinate(extent.getMinX(),
                        extent.getMinY()), PixelOrientation.CENTER) };

        this.geometry = MapUtil.getPolygon(corners);
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

    public static void main(String[] args) {
        HRAPSubGrid subGrid;
        try {
            subGrid = new HRAPSubGrid(new Rectangle(525, 440, 81, 91));
            System.out.println(subGrid.getGeometry());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
