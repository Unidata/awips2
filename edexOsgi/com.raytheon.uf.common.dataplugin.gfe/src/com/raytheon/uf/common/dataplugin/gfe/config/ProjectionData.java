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
package com.raytheon.uf.common.dataplugin.gfe.config;

import java.awt.Point;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.CoordAdapter;
import com.raytheon.uf.common.serialization.adapters.PointAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * ProjectionData class containing all the data any Projection could want.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/13/08     #1030      randerso    Initial port
 * 04/24/08     #1047      randerso    Made all fields private and created getters
 * 04/24/13     #1935      randerso    Fixed date line spanning issue
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ProjectionData implements ISerializableObject {

    private static float COMPARISON_THRESHOLD = 0.005f;

    public static enum ProjectionType {
        NONE, LAMBERT_CONFORMAL, MERCATOR, POLAR_STEREOGRAPHIC, LATLON
    };

    @XmlAttribute
    @DynamicSerializeElement
    private String projectionID;

    @XmlAttribute
    @DynamicSerializeElement
    private ProjectionType projectionType;

    @XmlElement
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    @DynamicSerializeElement
    private Coordinate latLonLL;

    @XmlElement
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    @DynamicSerializeElement
    private Coordinate latLonUR;

    @XmlElement
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    @DynamicSerializeElement
    private Coordinate latLonOrigin;

    @XmlAttribute
    @DynamicSerializeElement
    private double stdParallelOne;

    @XmlAttribute
    @DynamicSerializeElement
    private double stdParallelTwo;

    @XmlElement
    @XmlJavaTypeAdapter(value = PointAdapter.class)
    @DynamicSerializeElement
    private Point gridPointLL;

    @XmlElement
    @XmlJavaTypeAdapter(value = PointAdapter.class)
    @DynamicSerializeElement
    private Point gridPointUR;

    @XmlAttribute
    @DynamicSerializeElement
    private double latIntersect;

    @XmlAttribute
    @DynamicSerializeElement
    private double lonCenter;

    @XmlAttribute
    @DynamicSerializeElement
    private double lonOrigin;

    private CoordinateReferenceSystem crs;

    private GridGeometry2D gridGeometry;

    private MathTransform crsToLatLon;

    private MathTransform latLonToCrs;

    private HashMap<PixelOrientation, MathTransform> gridToLatLon;

    private HashMap<PixelOrientation, MathTransform> latLonToGrid;

    private HashMap<PixelOrientation, MathTransform> gridToCrs;

    private boolean initialized;

    public ProjectionData() {
        latLonLL = new Coordinate();
        latLonUR = new Coordinate();
        latLonOrigin = new Coordinate();
        gridPointLL = new Point();
        gridPointUR = new Point();

        gridToLatLon = new HashMap<PixelOrientation, MathTransform>();
        latLonToGrid = new HashMap<PixelOrientation, MathTransform>();
        gridToCrs = new HashMap<PixelOrientation, MathTransform>();

        initialized = false;
    }

    public ProjectionData(String projID, int projType, Coordinate latLonLL,
            Coordinate latLonUR, Coordinate latLonOrig, float stdPar1,
            float stdPar2, Point gridLL, Point gridUR, float latInt,
            float lonCenter, float lonOrig) {
        this(projID, ProjectionType.values()[projType], latLonLL, latLonUR,
                latLonOrig, stdPar1, stdPar2, gridLL, gridUR, latInt,
                lonCenter, lonOrig);

        init();
    }

    public ProjectionData(String projID, ProjectionType projType,
            Coordinate latLonLL, Coordinate latLonUR, Coordinate latLonOrig,
            float stdPar1, float stdPar2, Point gridLL, Point gridUR,
            float latInt, float lonCenter, float lonOrig) {
        this();

        this.projectionID = projID;
        this.projectionType = projType;
        this.latLonLL = latLonLL;
        this.latLonUR = latLonUR;
        this.latLonOrigin = latLonOrig;
        this.stdParallelOne = stdPar1;
        this.stdParallelTwo = stdPar2;
        this.gridPointLL = gridLL;
        this.gridPointUR = gridUR;
        this.latIntersect = latInt;
        this.lonCenter = lonCenter;
        this.lonOrigin = lonOrig;

        init();
    }

    private void init() {
        if (initialized) {
            return;
        }

        try {
            // transform the projection corner points to CRS units
            MathTransform mt = MapUtil.getTransformFromLatLon(getCrs());
            double[] output = new double[4];
            mt.transform(new double[] { getLatLonLL().x, getLatLonLL().y,
                    getLatLonUR().x, getLatLonUR().y }, 0, output, 0, 2);

            // create a grid geometry for the projection
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(getCrs());
            ge.setRange(0, Math.min(output[0], output[2]),
                    Math.max(output[0], output[2]));
            ge.setRange(1, Math.min(output[1], output[3]),
                    Math.max(output[1], output[3]));

            // NOTE: the LL + 1 is a kludge to work around an apparent geotools
            // issue
            GeneralGridEnvelope gr = new GeneralGridEnvelope(new int[] {
                    getGridPointLL().x + 1, getGridPointLL().y + 1 },
                    new int[] { getGridPointUR().x, getGridPointUR().y }, true);

            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, false });
            mt = mapper.createTransform();

            gridGeometry = new GridGeometry2D(PixelInCell.CELL_CORNER, mt, ge,
                    null);

            crsToLatLon = MapUtil.getTransformToLatLon(getCrs());
            latLonToCrs = MapUtil.getTransformFromLatLon(getCrs());

            initialized = true;

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String result = "{";

        result += projectionType + ", ";
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

    public synchronized CoordinateReferenceSystem getCrs() {
        if (crs == null) {

            // construct the appropriate CRS based on the projection type
            switch (this.projectionType) {
            case LAMBERT_CONFORMAL:
                crs = MapUtil.constructLambertConformal(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        this.stdParallelOne, this.stdParallelTwo,
                        this.latLonOrigin.x);
                break;

            case MERCATOR:
                crs = MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                        MapUtil.AWIPS_EARTH_RADIUS, this.stdParallelOne,
                        this.lonCenter);
                break;

            case POLAR_STEREOGRAPHIC:
                crs = MapUtil.constructNorthPolarStereo(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        60.0, this.lonOrigin);
                break;

            case LATLON:
                crs = MapUtil.constructEquidistantCylindrical(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        this.lonCenter, 0);
                break;

            case NONE:
            default:
                System.out.println("ERROR: unknown projection type: "
                        + this.projectionType);
            }
        }

        return crs;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ProjectionData)) {
            return false;
        }

        ProjectionData rhs = (ProjectionData) obj;
        if (!this.getProjectionType().equals(rhs.getProjectionType())) {
            return false;
        }

        if (Math.abs(this.latLonLL.y - rhs.latLonLL.y) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonUR.y - rhs.latLonUR.y) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD) {
            return false;
        }

        // specific projection comparisons
        switch (this.projectionType) {
        case LAMBERT_CONFORMAL:
            return (Math.abs(this.latLonOrigin.y - rhs.latLonOrigin.y) < COMPARISON_THRESHOLD
                    || Math.abs(this.latLonOrigin.x - rhs.latLonOrigin.x) < COMPARISON_THRESHOLD
                    || Math.abs(this.stdParallelOne - rhs.stdParallelOne) < COMPARISON_THRESHOLD || Math
                    .abs(this.stdParallelTwo - rhs.stdParallelTwo) < COMPARISON_THRESHOLD);
        case POLAR_STEREOGRAPHIC:
            return (Math.abs(this.lonOrigin - rhs.lonOrigin) < COMPARISON_THRESHOLD);
        case MERCATOR:
            return (Math.abs(this.lonCenter - rhs.lonCenter) < COMPARISON_THRESHOLD);
        case LATLON:
            return true;
        default:
            return false;
        }
    }

    public Coordinate gridCoordinateToLatLon(Coordinate gridCoord,
            PixelOrientation orientation) {
        Coordinate latLon = new Coordinate();
        MathTransform mt = gridToLatLon.get(orientation);
        try {
            if (mt == null) {
                init();
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                mt = dmtf.createConcatenatedTransform(
                        gridGeometry.getGridToCRS(orientation), crsToLatLon);
                gridToLatLon.put(orientation, mt);
            }

            double[] output = new double[2];
            mt.transform(new double[] { gridCoord.x, gridCoord.y }, 0, output,
                    0, 1);
            latLon.x = output[0];
            latLon.y = output[1];
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return latLon;
    }

    public Coordinate latLonToGridCoordinate(Coordinate latLon,
            PixelOrientation orientation) {
        Coordinate gridCoord = new Coordinate();
        MathTransform mt = latLonToGrid.get(orientation);
        try {
            if (mt == null) {
                init();
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                mt = dmtf.createConcatenatedTransform(latLonToCrs, gridGeometry
                        .getGridToCRS(orientation).inverse());
                latLonToGrid.put(orientation, mt);
            }

            double[] output = new double[2];
            mt.transform(new double[] { latLon.x, latLon.y }, 0, output, 0, 1);
            gridCoord.x = output[0];
            gridCoord.y = output[1];
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return gridCoord;
    }

    public MathTransform getGridToCrs(PixelOrientation orientation) {
        MathTransform mt = gridToCrs.get(orientation);
        try {
            if (mt == null) {
                init();
                mt = gridGeometry.getGridToCRS(orientation);
                gridToCrs.put(orientation, mt);
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return mt;
    }

    public Coordinate gridCoordinateToCrs(Coordinate gridCoord,
            PixelOrientation orientation) {
        Coordinate crsCoordinate = new Coordinate();
        MathTransform mt = getGridToCrs(orientation);
        try {
            double[] output = new double[2];
            mt.transform(new double[] { gridCoord.x, gridCoord.y }, 0, output,
                    0, 1);
            crsCoordinate.x = output[0];
            crsCoordinate.y = output[1];
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return crsCoordinate;
    }

    public String getProjectionID() {
        return projectionID;
    }

    public ProjectionType getProjectionType() {
        return projectionType;
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

    public void setProjectionType(ProjectionType projectionType) {
        this.projectionType = projectionType;
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

    public Integer getNx() {
        return this.gridPointUR.x - this.gridPointLL.x + 1;
    }

    public Integer getNy() {
        return this.gridPointUR.y - this.gridPointLL.y + 1;
    }

}
