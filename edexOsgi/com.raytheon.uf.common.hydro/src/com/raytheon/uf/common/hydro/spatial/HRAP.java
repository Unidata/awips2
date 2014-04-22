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

import java.awt.Point;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Defines the HRAP spatial coordinate system
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2008            randerso     Initial creation
 * Mar 31, 2014     2689   mpduff      Log input values on conversion failure.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class HRAP {

    private static final long serialVersionUID = 1L;

    private static float COMPARISON_THRESHOLD = 0.005f;

    private static Map<Integer, HRAP> instances = new HashMap<Integer, HRAP>();

    /**
     * This is sort of nasty but it is meant to allow for full grid XMRG files
     * 
     * @return
     */
    public synchronized static HRAP getInstance() {
        return getInstance(1);
    }

    /**
     * Factor to increase grid resolution by
     * 
     * @param gridResolution
     * @return
     */
    public static synchronized HRAP getInstance(int gridResolution) {
        HRAP instance = instances.get(gridResolution);

        if (instance == null) {
            instance = new HRAP(gridResolution);
            instances.put(gridResolution, instance);
        }

        return instance;
    }

    private String projectionID;

    private final Coordinate latLonUL;

    private Coordinate latLonUR;

    private Coordinate latLonLL;

    private final Coordinate latLonLR;

    private Coordinate latLonOrigin;

    private double stdParallelOne;

    private double stdParallelTwo;

    private Point gridPointLL;

    private Point gridPointUR;

    private double latIntersect;

    private double lonCenter;

    private double lonOrigin;

    private CoordinateReferenceSystem crs;

    private final Map<PixelOrientation, MathTransform> toLatLonMap;

    private final Map<PixelOrientation, MathTransform> fromLatLonMap;

    private GridToEnvelopeMapper gridMapper;

    private GridGeometry2D gridGeometry;

    private HRAP(int gridResolution) {
        this.toLatLonMap = new HashMap<PixelOrientation, MathTransform>();
        this.fromLatLonMap = new HashMap<PixelOrientation, MathTransform>();

        this.projectionID = "HRAP";
        // this.latLonUL = new Coordinate(-80.772, 19.798);
        // this.latLonUR = new Coordinate(-60.0, 45.620);
        // this.latLonLL = new Coordinate(-119.036, 23.098);
        // this.latLonLR = new Coordinate(-134.055, 53.480);
        this.latLonLL = new Coordinate(-119.03624346792651, 23.09739145101471);
        this.latLonUL = new Coordinate(-134.05460409907715, 53.48009484523732);
        this.latLonUR = new Coordinate(-60.0, 45.61982902388795);
        this.latLonLR = new Coordinate(-80.77225468204585, 19.797555031799057);
        this.latLonOrigin = new Coordinate(0, 0);
        this.stdParallelOne = 0.0;
        this.stdParallelTwo = 0.0;
        this.gridPointLL = new Point(1, 1);
        this.gridPointUR = new Point(1121 * gridResolution,
                881 * gridResolution);
        this.latIntersect = 0.0;
        this.lonCenter = 0.0;
        this.lonOrigin = -105.0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String result = "{";

        result += latLonLL + ", ";
        result += latLonUR + ", ";
        result += latLonOrigin + ", ";
        result += stdParallelOne + ", ";
        result += stdParallelTwo + ", ";
        result += gridPointLL + ", ";
        result += gridPointUR + ", ";
        result += latIntersect + ", ";
        result += lonCenter + ", ";
        result += lonOrigin;

        result += "}";
        return result;
    }

    /**
     * @return
     */
    public CoordinateReferenceSystem getCrs() {
        if (crs == null) {
            crs = MapUtil.constructNorthPolarStereo(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, 60.0, this.lonOrigin);
        }
        return crs;
    }

    public GridToEnvelopeMapper getGridMapper() throws Exception {
        if (gridMapper == null) {
            try {
                // transform the projection corner points to CRS units
                MathTransform mt = MapUtil.getTransformFromLatLon(getCrs());
                double[] output = new double[4];
                mt.transform(new double[] { getLatLonLL().x, getLatLonLL().y,
                        getLatLonUR().x, getLatLonUR().y }, 0, output, 0, 2);

                // create a grid geometry for the projection
                GeneralEnvelope userRange = new GeneralEnvelope(2);
                userRange.setCoordinateReferenceSystem(getCrs());
                userRange.setRange(0, Math.min(output[0], output[2]),
                        Math.max(output[0], output[2]));
                userRange.setRange(1, Math.min(output[1], output[3]),
                        Math.max(output[1], output[3]));

                GeneralGridEnvelope gridRange = new GeneralGridEnvelope(
                        new int[] { getGridPointLL().x, getGridPointLL().y },
                        new int[] { getGridPointUR().x, getGridPointUR().y },
                        false);

                gridMapper = new GridToEnvelopeMapper(gridRange, userRange);
                gridMapper.setPixelAnchor(PixelInCell.CELL_CORNER);
                gridMapper.setReverseAxis(new boolean[] { false, false });

            } catch (Exception e) {
                throw new Exception("Unable to create HRAP g", e);
            }
        }

        return gridMapper;
    }

    public GridGeometry2D getGridGeometry() throws Exception {
        if (gridGeometry == null) {
            gridGeometry = new GridGeometry2D(getGridMapper().getGridRange(),
                    getGridMapper().createTransform(), getCrs());
        }

        return gridGeometry;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof HRAP)) {
            return false;
        }

        HRAP rhs = (HRAP) obj;
        if (Math.abs(this.latLonLL.y - rhs.latLonLL.y) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonUR.y - rhs.latLonUR.y) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD) {
            return false;
        }

        // specific projection comparisons
        return (Math.abs(this.lonOrigin - rhs.lonOrigin) < COMPARISON_THRESHOLD);
    }

    public String getProjectionID() {
        return projectionID;
    }

    public Coordinate getLatLonLL() {
        return latLonLL;
    }

    public Coordinate getLatLonUR() {
        return latLonUR;
    }

    public Coordinate getLatLonOrigin() {
        return latLonOrigin;
    }

    public double getStdParallelOne() {
        return stdParallelOne;
    }

    public double getStdParallelTwo() {
        return stdParallelTwo;
    }

    public Point getGridPointLL() {
        return gridPointLL;
    }

    public Point getGridPointUR() {
        return gridPointUR;
    }

    public double getLatIntersect() {
        return latIntersect;
    }

    public double getLonCenter() {
        return lonCenter;
    }

    public double getLonOrigin() {
        return lonOrigin;
    }

    public void setProjectionID(String projectionID) {
        this.projectionID = projectionID;
    }

    public void setLatLonLL(Coordinate latLonLL) {
        this.latLonLL = latLonLL;
    }

    public void setLatLonUR(Coordinate latLonUR) {
        this.latLonUR = latLonUR;
    }

    public void setLatLonOrigin(Coordinate latLonOrigin) {
        this.latLonOrigin = latLonOrigin;
    }

    public void setStdParallelOne(double stdParallelOne) {
        this.stdParallelOne = stdParallelOne;
    }

    public void setStdParallelTwo(double stdParallelTwo) {
        this.stdParallelTwo = stdParallelTwo;
    }

    public void setGridPointLL(Point gridPointLL) {
        this.gridPointLL = gridPointLL;
    }

    public void setGridPointUR(Point gridPointUR) {
        this.gridPointUR = gridPointUR;
    }

    public void setLatIntersect(double latIntersect) {
        this.latIntersect = latIntersect;
    }

    public void setLonCenter(double lonCenter) {
        this.lonCenter = lonCenter;
    }

    public void setLonOrigin(double lonOrigin) {
        this.lonOrigin = lonOrigin;
    }

    public HRAPSubGrid getHRAPSubGrid(Rectangle extent) throws Exception {
        HRAPSubGrid subGrid = new HRAPSubGrid(extent);
        return subGrid;
    }

    public HRAPSubGrid getHRAPSubGrid(Rectangle extent, int gribFactor)
            throws Exception {
        HRAPSubGrid subGrid = new HRAPSubGrid(extent, gribFactor);
        return subGrid;
    }

    private MathTransform getTransformToLatLon(PixelOrientation orientation)
            throws Exception {
        MathTransform toLatLon = toLatLonMap.get(orientation);
        if (toLatLon == null) {
            try {
                MathTransform gridToProj = getGridMapper().createTransform();
                MathTransform projToLatLon = MapUtil
                        .getTransformToLatLon(getCrs());
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                toLatLon = dmtf.createConcatenatedTransform(gridToProj,
                        projToLatLon);
            } catch (Exception e) {
                throw new Exception("Error creating HRAP to Lat/Lon transform",
                        e);
            }
            toLatLonMap.put(orientation, toLatLon);
        }
        return toLatLon;
    }

    private MathTransform getTransformFromLatLon(PixelOrientation orientation)
            throws Exception {
        MathTransform fromLatLon = fromLatLonMap.get(orientation);
        if (fromLatLon == null) {
            try {
                MathTransform latLonToProj = MapUtil
                        .getTransformFromLatLon(getCrs());
                MathTransform projToGrid = getGridMapper().createTransform()
                        .inverse();
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                fromLatLon = dmtf.createConcatenatedTransform(latLonToProj,
                        projToGrid);
            } catch (Exception e) {
                throw new Exception("Error creating Lat/Lon to HRAP transform",
                        e);
            }
            fromLatLonMap.put(orientation, fromLatLon);
        }
        return fromLatLon;
    }

    public Integer getNx() {
        return this.gridPointUR.x - gridPointLL.x + 1;
    }

    public Integer getNy() {
        return this.gridPointUR.y - gridPointLL.y + 1;
    }

    public Coordinate gridCoordinateToLatLon(Point point,
            PixelOrientation orientation) throws Exception {
        return gridCoordinateToLatLon(new Coordinate(point.x, point.y),
                orientation);
    }

    public Coordinate gridCoordinateToLatLon(Coordinate gridCoord,
            PixelOrientation orientation) throws Exception {
        // Coordinate adjusted = new Coordinate(gridCoord.x - 1, getNy()
        // - gridCoord.y);
        // return MapUtil.gridCoordinateToLatLon(adjusted, orientation, this);
        Coordinate latLon = new Coordinate();

        try {
            JTS.transform(gridCoord, latLon, getTransformToLatLon(orientation));
        } catch (TransformException e) {
            throw new Exception(
                    "Error creating transform from HRAP to Lat/Lon", e);
        }
        return latLon;
    }

    public Coordinate latLonToGridCoordinate(Coordinate latLon,
            PixelOrientation orientation) throws Exception {
        // Coordinate gridCell = MapUtil.latLonToGridCoordinate(latLon,
        // orientation, this);
        // return new Coordinate(gridCell.x + 1, getNy() - gridCell.y);
        Coordinate gridCell = new Coordinate();

        try {
            JTS.transform(latLon, gridCell, getTransformFromLatLon(orientation));
        } catch (TransformException e) {
            throw new Exception(
                    "Error creating transform from Lat/Lon to HRAP.  Input: x ["
                            + latLon.x + "], y [" + latLon.y + "]", e);
        }
        return gridCell;
    }

    public static void main(String[] args) {
        HRAP hrap = HRAP.getInstance();

        PixelOrientation po = PixelOrientation.CENTER;

        try {

            Coordinate gridCell = new Coordinate();
            ReferencedCoordinate refCoord = new ReferencedCoordinate(gridCell,
                    hrap.getGridGeometry(), Type.GRID_CENTER);

            Coordinate latLon = new Coordinate();

            gridCell.x = 1;
            gridCell.y = 1;
            latLon = hrap.gridCoordinateToLatLon(gridCell, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            latLon = refCoord.asLatLon();
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            gridCell.x = 1;
            gridCell.y = 881;
            latLon = hrap.gridCoordinateToLatLon(gridCell, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            latLon = refCoord.asLatLon();
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            gridCell.x = 1121;
            gridCell.y = 881;
            latLon = hrap.gridCoordinateToLatLon(gridCell, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            latLon = refCoord.asLatLon();
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            gridCell.x = 1121;
            gridCell.y = 1;
            latLon = hrap.gridCoordinateToLatLon(gridCell, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            latLon = refCoord.asLatLon();
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            System.out.println();

            latLon.x = hrap.latLonLL.x;
            latLon.y = hrap.latLonLL.y;
            refCoord = new ReferencedCoordinate(latLon);
            gridCell = hrap.latLonToGridCoordinate(latLon, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            gridCell = refCoord.asGridCell(hrap.getGridGeometry(),
                    PixelInCell.CELL_CENTER);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            latLon.x = hrap.latLonUL.x;
            latLon.y = hrap.latLonUL.y;
            gridCell = hrap.latLonToGridCoordinate(latLon, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            gridCell = refCoord.asGridCell(hrap.getGridGeometry(),
                    PixelInCell.CELL_CENTER);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            latLon.x = hrap.latLonUR.x;
            latLon.y = hrap.latLonUR.y;
            gridCell = hrap.latLonToGridCoordinate(latLon, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            gridCell = refCoord.asGridCell(hrap.getGridGeometry(),
                    PixelInCell.CELL_CENTER);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            latLon.x = hrap.latLonLR.x;
            latLon.y = hrap.latLonLR.y;
            gridCell = hrap.latLonToGridCoordinate(latLon, po);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);
            gridCell = refCoord.asGridCell(hrap.getGridGeometry(),
                    PixelInCell.CELL_CENTER);
            System.out.println("grid: " + gridCell + ", latLon: " + latLon);

            System.out.println("Nx: " + hrap.getNx() + ", Ny: " + hrap.getNy());

        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
