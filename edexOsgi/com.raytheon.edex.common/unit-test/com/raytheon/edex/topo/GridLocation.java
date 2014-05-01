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
package com.raytheon.edex.topo;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

public class GridLocation implements ISpatialObject {

    private CoordinateReferenceSystem crsObject;

    private Polygon geometry;

    private Integer nx;

    private Integer ny;

    private String crsWKT;

    private Coordinate extent;

    private Coordinate origin;

    private ProjectionData projection;

    /**
     * @param data
     * @param gridSize
     * @param domainOrigin
     * @param domainExtent
     */
    public GridLocation(ProjectionData proj, java.awt.Point gridSize,
            Coordinate domainOrigin, Coordinate domainExtent) {
        try {
            this.nx = gridSize.x;
            this.ny = gridSize.y;
            this.projection = proj;
            this.origin = domainOrigin;
            this.extent = domainExtent;
            this.crsObject = proj.getCRS();
            this.crsWKT = this.crsObject.toWKT();

            // transform the projection corner points to CRS units
            MathTransform mt = MapUtil.getTransformFromLatLon(this.crsObject);
            double[] output = new double[4];
            mt.transform(
                    new double[] { proj.getLatLonLL().x, proj.getLatLonLL().y,
                            proj.getLatLonUR().x, proj.getLatLonUR().y }, 0,
                    output, 0, 2);

            // create a grid geometry for the projection
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(this.crsObject);
            ge.setRange(0, Math.min(output[0], output[2]),
                    Math.max(output[0], output[2]));
            ge.setRange(1, Math.min(output[1], output[3]),
                    Math.max(output[1], output[3]));

            GeneralGridEnvelope gr = new GeneralGridEnvelope(new int[] {
                    proj.getGridPointLL().x, proj.getGridPointLL().y },
                    new int[] { proj.getGridPointUR().x + 1,
                            proj.getGridPointUR().y + 1 }, false);

            GridGeometry2D projGeom = new GridGeometry2D(gr, ge);

            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            double[] latLon = new double[8];

            // transform the grid corners from grid coordinates to CRS units
            // need to adjust for the fact that AWIPS considers 1,1 to be in
            // lower left and GeoTools considers 1,1 to be in upper left
            Coordinate ll = new Coordinate(domainOrigin.x,
                    proj.getGridPointUR().y - domainOrigin.y
                            + proj.getGridPointLL().y);
            Coordinate ur = new Coordinate(domainOrigin.x + domainExtent.x,
                    ll.y - domainExtent.y);

            mt.transform(new double[] { ll.x, ll.y, ur.x, ur.y }, 0, output, 0,
                    2);

            mt = projGeom.getGridToCRS(PixelOrientation.UPPER_LEFT);
            output = new double[4];
            mt.transform(new double[] { ll.x, ll.y, ur.x, ur.y }, 0, output, 0,
                    2);

            // construct the grid geometry that covers the GFE grid
            ge.setRange(0, Math.min(output[0], output[2]),
                    Math.max(output[0], output[2]));
            ge.setRange(1, Math.min(output[1], output[3]),
                    Math.max(output[1], output[3]));
            GridGeometry2D gridGeom = new GridGeometry2D(
                    new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                            this.nx, this.ny }, false), ge);

            // set up the transform from grid coordinates to lon/lat
            mt = dmtf.createConcatenatedTransform(
                    gridGeom.getGridToCRS(PixelOrientation.CENTER),
                    MapUtil.getTransformToLatLon(crsObject));

            // transform grid corner points to Lat/Lon
            mt.transform(new double[] { -1.0, this.ny - 1, -1.0, -1.0,
                    this.nx - 1, -1.0, this.nx - 1, this.ny - 1 }, 0, latLon,
                    0, 4);

            Coordinate[] corners = new Coordinate[] {
                    MapUtil.getCoordinate(latLon[0], latLon[1]),
                    MapUtil.getCoordinate(latLon[2], latLon[3]),
                    MapUtil.getCoordinate(latLon[4], latLon[5]),
                    MapUtil.getCoordinate(latLon[6], latLon[7]),
                    MapUtil.getCoordinate(latLon[0], latLon[1]) };

            this.geometry = MapUtil.getPolygon(corners);

            // this.geometry = new Polygon(new LinearRing[] { shell });
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Override
    public CoordinateReferenceSystem getCrs() {
        if (crsObject == null) {
            try {
                crsObject = CRSCache.getInstance()
                        .getCoordinateReferenceSystem(crsWKT);
            } catch (FactoryException e) {
                crsObject = null;
            }
        }
        return crsObject;
    }

    @Override
    public Polygon getGeometry() {
        return geometry;
    }

    @Override
    public Integer getNx() {
        return nx;
    }

    @Override
    public Integer getNy() {
        return ny;
    }
}
