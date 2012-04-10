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

package com.raytheon.uf.viz.core.map;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Implementation of IMapDescriptor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *           
 * Date          Ticket#    Engineer    Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 1/12/09                  randerso    added getMapManager
 * 10/22/09     #3348       bsteffen    Moved getter/setters for numberOfFrames down to AbstractDescriptor
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class MapDescriptor extends AbstractDescriptor implements
        IMapDescriptor, ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapDescriptor.class);

    public static final int LABEL_PADDING = 5;

    // private static final int MOUSE_HEIGHT = 100;

    /**
     * Create a grid geometry for a projection given the crs, lower left, and
     * upper right corners.
     * 
     * @param crs
     *            CoordinateReferenceSystem of the projection
     * @param llCoord
     *            lat/lon of the lower left corner
     * @param urCoord
     *            lat/lon of the upper right corner
     * @return
     * @throws FactoryException
     * @throws TransformException
     */
    public static GridGeometry2D createGridGeometry(
            CoordinateReferenceSystem crs, Coordinate llCoord,
            Coordinate urCoord) throws VizException {
        try {
            MathTransform WGS84toPROJCRS = MapUtil.getTransformFromLatLon(crs);

            GeneralEnvelope envelope = new GeneralEnvelope(2);

            DirectPosition ll = WGS84toPROJCRS.transform(new DirectPosition2D(
                    llCoord.x, llCoord.y), null);

            DirectPosition ur = WGS84toPROJCRS.transform(new DirectPosition2D(
                    urCoord.x, urCoord.y), null);

            envelope.setRange(0,
                    Math.min(ll.getOrdinate(0), ur.getOrdinate(0)),
                    Math.max(ll.getOrdinate(0), ur.getOrdinate(0)));
            envelope.setRange(1,
                    Math.min(ll.getOrdinate(1), ur.getOrdinate(1)),
                    Math.max(ll.getOrdinate(1), ur.getOrdinate(1)));

            envelope.setCoordinateReferenceSystem(crs);

            return constructGridGeometry(envelope);
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    /**
     * Create a grid geometry for a projection given the crs, center, width and
     * height.
     * 
     * @param crs
     *            CoordinateReferenceSystem of the projection
     * @param center
     *            lat/lon of center
     * @param width
     *            in projection units
     * @param height
     *            in projection units
     * @return
     * @throws VizException
     * @throws FactoryException
     * @throws TransformException
     */
    public static GridGeometry2D createGridGeometry(
            CoordinateReferenceSystem crs, Coordinate centerLL, double width,
            double height) throws VizException {
        try {
            MathTransform WGS84toPROJCRS = MapUtil.getTransformFromLatLon(crs);

            GeneralEnvelope envelope = new GeneralEnvelope(2);

            DirectPosition center = WGS84toPROJCRS.transform(
                    new DirectPosition2D(centerLL.x, centerLL.y), null);

            double halfWidth = width / 2;
            envelope.setRange(0, center.getOrdinate(0) - halfWidth,
                    center.getOrdinate(0) + halfWidth);

            double halfHeight = height / 2;
            envelope.setRange(1, center.getOrdinate(1) - halfHeight,
                    center.getOrdinate(1) + halfHeight);

            envelope.setCoordinateReferenceSystem(crs);

            return constructGridGeometry(envelope);
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    private static GridGeometry2D constructGridGeometry(GeneralEnvelope envelope) {
        // scale map so smaller dimension is DEFAULT_WORLD_HEIGHT pixels
        double aspect = envelope.getSpan(0) / envelope.getSpan(1);
        int mapWidth = (aspect > 1.0 ? (int) Math.round(aspect
                * DEFAULT_WORLD_HEIGHT) : DEFAULT_WORLD_HEIGHT);
        int mapHeight = (aspect > 1.0 ? DEFAULT_WORLD_HEIGHT : (int) Math
                .round(DEFAULT_WORLD_HEIGHT / aspect));

        GridGeometry2D gridGeom = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { mapWidth, mapHeight }, false),
                envelope);
        return gridGeom;
    }

    /** The mapping from grid to coordinate */
    protected MathTransform mapToCoordinateTransform;

    /** The mapping from coordinate to grid */
    protected MathTransform coordinateToMapTransform;

    /** The time in ms that the last blink state was used */
    protected long timeLastBlink;

    /** The last blink state */
    protected boolean currentBlinkState;

    /** Text color */
    protected final RGB textColor = new RGB(255, 255, 255);

    /**
     * Approximate mapWidth in meters This is used to compute the display width
     * for progressive disclosure
     */
    protected int mapWidth;

    /** elevation exaggeration */
    protected double elevationExageration = 0;

    private static final NumberFormat LAT_LON_FORMATTER;
    static {
        LAT_LON_FORMATTER = DecimalFormat.getInstance();
        LAT_LON_FORMATTER.setMinimumFractionDigits(2);
        LAT_LON_FORMATTER.setMaximumFractionDigits(2);
    }

    /**
     * Constructor
     * 
     * This creates a default view of the entire world in Lat/Lon projection
     * 
     * @param worldWidth
     * @param worldHeight
     */
    public MapDescriptor() throws VizException {
        this(MapUtil.LATLON_PROJECTION, new Coordinate(-180, -90),
                new Coordinate(180, 90));

    }

    public MapDescriptor(CoordinateReferenceSystem crs, Coordinate llCoord,
            Coordinate urCoord) throws VizException {
        this(createGridGeometry(crs, llCoord, urCoord));
    }

    /**
     * Constructor
     * 
     * @param gridGeometry
     *            the geometry and coverage of the map
     * @param percentOfEarthCoverage
     *            estimate of the percent of the total earth (used for zoom
     *            level calculation)
     * 
     */
    public MapDescriptor(GeneralGridGeometry gridGeometry) throws VizException {
        super(gridGeometry);
        init();
    }

    protected void init() throws VizException {
        try {
            GeneralGridGeometry gridGeometry = getGridGeometry();
            mapToCoordinateTransform = gridGeometry
                    .getGridToCRS(PixelInCell.CELL_CENTER);
            coordinateToMapTransform = mapToCoordinateTransform.inverse();

            CoordinateReferenceSystem crs = gridGeometry
                    .getCoordinateReferenceSystem();

            DirectPosition s1, d1, s2, d2;
            if (crs.getCoordinateSystem().getDimension() == 2) {
                double centerX = (gridGeometry.getGridRange().getLow(0) + gridGeometry
                        .getGridRange().getHigh(0)) / 2;

                double centerY = (gridGeometry.getGridRange().getLow(1) + gridGeometry
                        .getGridRange().getHigh(1)) / 2;

                s1 = new DirectPosition2D(centerX, centerY);
                d1 = new DirectPosition2D(centerX + 1, centerY);
                s2 = new DirectPosition2D(crs);
                d2 = new DirectPosition2D(crs);

                mapToCoordinateTransform.transform(s1, s2);
                mapToCoordinateTransform.transform(d1, d2);

                GeodeticCalculator gc = new GeodeticCalculator(crs);
                gc.setStartingPosition(s2);
                gc.setDestinationPosition(d2);

                double distance = gc.getOrthodromicDistance();
                // System.out.println("Azimuth: " + azimuth + ",
                // Distance: "
                // + distance);
                this.mapWidth = (int) distance
                        * gridGeometry.getGridRange().getHigh(0);

            } else {
                // TODO come up with a better way of calculating this for 3D
                this.mapWidth = 12700000;
            }

            // reproject all resources contained in this descriptor
            ArrayList<ResourcePair> unProjectable = new ArrayList<ResourcePair>();
            for (ResourcePair rp : this.resourceList) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc == null) {
                    continue;
                }
                try {
                    rsc.project(gridGeometry.getCoordinateReferenceSystem());
                } catch (VizException e) {
                    // TODO: what to do here?
                    unProjectable.add(rp);
                    statusHandler.handle(Priority.PROBLEM,
                            "Error projecting resource :: " + rsc.getName(), e);
                }
            }
            this.resourceList.removeAll(unProjectable);

            // System.out.println("mapWidth = " + mapWidth + " meters");
        } catch (Exception e) {
            throw new VizException("Error setting up map transformations", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.map.IMapDescriptor#pixelToWorld(double[],
     * org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public double[] pixelToWorld(final double[] pixel,
            CoordinateReferenceSystem crs) {

        if (crs == MapUtil.LATLON_PROJECTION) {
            return pixelToWorld(pixel);
        } else if (!crs.getName().equals(
                getGridGeometry().getCoordinateReferenceSystem().getName())) {
            return null;
        }

        double[] output2 = new double[3];
        try {
            mapToCoordinateTransform.transform(pixel, 0, output2, 0, 1);
        } catch (TransformException e) {
            return null;
        }

        return output2;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.map.IMapDescriptor#worldToPixel(double[],
     * org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public double[] worldToPixel(double[] pixel, CoordinateReferenceSystem crs) {
        if (crs == MapUtil.LATLON_PROJECTION) {
            return worldToPixel(pixel);
        } else if (!crs.getName().equals(
                getGridGeometry().getCoordinateReferenceSystem().getName())) {
            return null;
        }

        double[] output2 = new double[3];
        try {
            coordinateToMapTransform.transform(pixel, 0, output2, 0, 1);
        } catch (TransformException e) {
            return null;
        }

        return output2;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapDescriptor#pixelToWorld(com.raytheon.viz
     * .core.PixelExtent)
     */
    @Override
    public Envelope pixelToWorld(IExtent extent) {
        double[] ul = pixelToWorld(new double[] { extent.getMinX(),
                extent.getMaxY(), 0 });

        double[] ll = pixelToWorld(new double[] { extent.getMinX(),
                extent.getMinY(), 0 });

        double[] lr = pixelToWorld(new double[] { extent.getMaxX(),
                extent.getMinY(), 0 });

        double[] ur = pixelToWorld(new double[] { extent.getMaxX(),
                extent.getMaxY(), 0 });

        double minY = Math.min(lr[1], ll[1]);
        minY = Math.min(ul[1], minY);
        minY = Math.min(ur[1], minY);

        double maxY = Math.max(ul[1], ur[1]);
        maxY = Math.max(ll[1], maxY);
        maxY = Math.max(ul[1], maxY);

        Envelope lle = new Envelope(Math.min(ul[0], ll[0]), Math.max(lr[0],
                ur[0]), minY, maxY);
        return lle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapDescriptor#pixelToWorld(com.raytheon.viz
     * .core.IExtent, org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public Envelope pixelToWorld(IExtent extent, CoordinateReferenceSystem crs) {
        double[] ul = pixelToWorld(
                new double[] { extent.getMinX(), extent.getMaxY(), 0 }, crs);

        double[] ll = pixelToWorld(
                new double[] { extent.getMinX(), extent.getMinY(), 0 }, crs);

        double[] lr = pixelToWorld(
                new double[] { extent.getMaxX(), extent.getMinY(), 0 }, crs);

        double[] ur = pixelToWorld(
                new double[] { extent.getMaxX(), extent.getMaxY(), 0 }, crs);

        double minY = Math.min(lr[1], ll[1]);
        minY = Math.min(ul[1], minY);
        minY = Math.min(ur[1], minY);

        double maxY = Math.max(ul[1], ur[1]);
        maxY = Math.max(ll[1], maxY);
        maxY = Math.max(ul[1], maxY);

        Envelope lle = new Envelope(Math.min(ul[0], ll[0]), Math.max(lr[0],
                ur[0]), minY, maxY);
        return lle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapDescriptor#worldToPixel(com.vividsolutions
     * .jts.geom.Envelope)
     */
    @Override
    public PixelCoverage worldToPixel(Envelope extent) {

        double[] ul = worldToPixel(new double[] { extent.getMinX(),
                extent.getMaxY(), 0 });

        double[] ur = worldToPixel(new double[] { extent.getMaxX(),
                extent.getMaxY(), 0 });

        double[] lr = worldToPixel(new double[] { extent.getMaxX(),
                extent.getMinY(), 0 });

        double[] ll = worldToPixel(new double[] { extent.getMinX(),
                extent.getMinY(), 0 });

        if (ul == null || ur == null || lr == null || ll == null) {
            return null;
        }

        PixelCoverage pc = new PixelCoverage(
                new Coordinate(ul[0], ul[1], ul[2]), new Coordinate(ur[0],
                        ur[1], ur[2]), new Coordinate(lr[0], lr[1], lr[2]),
                new Coordinate(ll[0], ll[1], ll[2]));

        return pc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapDescriptor#worldToPixel(com.vividsolutions
     * .jts.geom.Envelope,
     * org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public PixelCoverage worldToPixel(Envelope extent,
            CoordinateReferenceSystem crs) {

        double[] ul = worldToPixel(
                new double[] { extent.getMinX(), extent.getMinY(), 0 }, crs);

        double[] ur = worldToPixel(
                new double[] { extent.getMaxX(), extent.getMinY(), 0 }, crs);

        double[] lr = worldToPixel(
                new double[] { extent.getMaxX(), extent.getMaxY(), 0 }, crs);

        double[] ll = worldToPixel(
                new double[] { extent.getMinX(), extent.getMaxY(), 0 }, crs);

        PixelCoverage pc = new PixelCoverage(
                new Coordinate(ul[0], ul[1], ul[2]), new Coordinate(ur[0],
                        ur[1], ul[2]), new Coordinate(lr[0], lr[1], ul[2]),
                new Coordinate(ll[0], ll[1], ul[2]));

        return pc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapDescriptor#setProjection(org.opengis
     * .referencing.crs.CoordinateReferenceSystem,
     * com.vividsolutions.jts.geom.Coordinate,
     * com.vividsolutions.jts.geom.Coordinate)
     */
    public void setProjection(CoordinateReferenceSystem crs, Coordinate ll,
            Coordinate ur) throws FactoryException, TransformException,
            VizException {
        GridGeometry2D gridGeometry = createGridGeometry(crs, ll, ur);
        setGridGeometry(gridGeometry);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapDescriptor#setGridGeometry(org.geotools
     * .coverage.grid.GeneralGridGeometry)
     */
    @Override
    public void setGridGeometry(GeneralGridGeometry gridGeometry)
            throws VizException {
        super.setGridGeometry(gridGeometry);
        init();
    }

    /**
     * Get the current display width
     * 
     * @return the current display width in meters
     */
    @Override
    public int getMapWidth() {
        return this.mapWidth;
    }

}
