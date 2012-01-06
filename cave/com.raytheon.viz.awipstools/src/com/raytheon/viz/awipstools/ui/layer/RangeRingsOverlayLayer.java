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
package com.raytheon.viz.awipstools.ui.layer;

import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource;
import com.raytheon.viz.core.interval.XFormFunctions;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Draws a series of range rings around the center point for a ranged resource
 * (such as radar data.) Also draws cardinal and ordinal spokes with certain
 * labels on the ordinal spokes. <strong>The ranged resource interrogate method
 * must return the center "Latitude" and "Longitude" and "Elevation"as key
 * values. Also, the ranged resource may implement IVertSeqResource for cases
 * where it has multiple levels.</strong>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  12-03-09     #3698      bgonzale    Range rings overlay with labels 
 *                                      for radius in km and miles and
 *                                      labels for elevation in ftMSL and
 *                                      kmAgl.
 * 
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class RangeRingsOverlayLayer implements IRenderable {

    private static double ordinalNE = 45;

    private static double ordinalSE = 135;

    private static double ordinalSW = 225;

    private static double ordinalNW = 315;

    private final UnitConverter convertFromMeterToKilometer = SI.METER
            .getConverterTo(SI.KILOMETER);

    private final UnitConverter convertFromMeterToFoot = SI.METER
            .getConverterTo(NonSI.FOOT);

    private final UnitConverter convertFromMileToKilometer = NonSI.MILE
            .getConverterTo(SI.KILOMETER);

    private final UnitConverter convertFromMileToMeter = NonSI.MILE
            .getConverterTo(SI.METER);

    private final UnitConverter convertFromDegreesToRadians = NonSI.DEGREE_ANGLE
            .getConverterTo(SI.RADIAN);

    private GeodeticCalculator gc;

    private Coordinate center;

    private IRangeableResource rangedResource;

    private boolean hasTilt = false;

    private double tilt;

    private double resourceSourceElevationInMeters;

    private IMapDescriptor descriptor;

    private ColorableCapability colorableCapability;

    private OutlineCapability outlineCapability;

    private Map<WireframeKey, IWireframeShape> wireframeMap;

    public RangeRingsOverlayLayer(IMapDescriptor desc,
            ColorableCapability colorableCapability,
            OutlineCapability outlineCapability) {
        this.descriptor = desc;
        this.colorableCapability = colorableCapability;
        this.outlineCapability = outlineCapability;
        this.gc = new GeodeticCalculator(this.descriptor.getCRS());
        this.wireframeMap = new HashMap<WireframeKey, IWireframeShape>();
    }

    /**
     * @return the rangedResource
     */
    public IRangeableResource getRangedResource() {
        return rangedResource;
    }

    /**
     * @param rangedResource
     *            the rangedResource to set
     * @throws VizException
     */
    public void setRangedResource(IRangeableResource rangedResource)
            throws VizException {
        this.rangedResource = rangedResource;
        this.center = this.rangedResource.getCenter();
        this.resourceSourceElevationInMeters = this.rangedResource
                .getElevation().getUnit().getConverterTo(SI.METER)
                .convert(this.rangedResource.getElevation().doubleValue());
        this.gc.setStartingGeographicPoint(this.center.x, this.center.y);
        updateTilt();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        // calculate ring spacing to use, in miles
        float zoomLevel = (float) paintProps.getZoomLevel();
        IExtent pixelExtent = paintProps.getView().getExtent();

        float metersZoomConstant = 500000;
        int mapWidth = descriptor.getMapWidth();
        float dataZoom = (float) ((mapWidth * zoomLevel) / metersZoomConstant);
        double dRing = getStandardRadius(dataZoom);

        IWireframeShape shape = getRangeRingsWireframe(dRing, pixelExtent,
                target);

        target.drawWireframeShape(shape, colorableCapability.getColor(),
                outlineCapability.getOutlineWidth(),
                outlineCapability.getLineStyle());
    }

    private IWireframeShape getRangeRingsWireframe(double dRing,
            IExtent pixelExtent, IGraphicsTarget target) throws VizException {
        WireframeKey key = new WireframeKey(tilt, dRing);
        IWireframeShape shape = wireframeMap.get(key);

        if (shape == null) {
            // get the maximum radius from the radar on the display
            double maxRadiusMiles = getMaxRingRadiusMiles(pixelExtent);

            shape = createRangeOverlay(target, dRing, maxRadiusMiles);
            wireframeMap.put(key, shape);
        }
        return shape;
    }

    void updateTilt() {
        IRangeableResource rsc = rangedResource;
        if (rsc instanceof IResourceGroup) {
            rsc = (IRangeableResource) ((IResourceGroup) rsc).getResourceList()
                    .get(0);
        }
        this.tilt = rsc.getTilt();
        if (tilt >= 0.0) {
            this.hasTilt = true;
        } else {
            this.hasTilt = false;
        }
    }

    /**
     * @param dataZoom
     * @return
     */
    private double getStandardRadius(float dataZoom) {
        double dRing = XFormFunctions.newDataIntervalFromZoom(50f,
                1f / dataZoom, false, null, 30);

        // dRing constraints
        if (dRing < 2) {
            dRing = 2;
        } else if (dRing > 50 && dRing <= 100) {
            dRing = 50;
        } else if (dRing > 100) {
            dRing = 100;
        }

        // round radius to standardized values
        // 2, 5, 10, 20, 50 only
        if (dRing < 2.5) {
            dRing = 2;
        } else if (dRing < 7.5) {
            dRing = 5;
        } else if (dRing < 15) {
            dRing = 10;
        } else if (dRing < 35) {
            dRing = 20;
        } else {
            dRing = 50;
        }
        return dRing;
    }

    private IWireframeShape createRangeOverlay(IGraphicsTarget target,
            double dRing, double maxRangeInMiles) throws VizException {
        IWireframeShape shape = target.createWireframeShape(true, descriptor);

        gc.setStartingGeographicPoint(center.x, center.y);
        gc.setDirection(90, convertFromMileToMeter.convert(30));
        double[] p1 = descriptor.worldToPixel(new double[] {
                gc.getStartingGeographicPoint().getX(),
                gc.getStartingGeographicPoint().getY() });
        int maxRingCount = (int) (maxRangeInMiles / dRing);
        double radiusMeters = convertFromMileToMeter.convert(dRing);

        // draw radial lines
        for (double az = -180; az < 180; az += 22.5) {
            double farRadius = maxRingCount * radiusMeters;
            gc.setDirection(az, farRadius);
            double ring = (((int) az) % 90 == 0 ? 0.0 : radiusMeters);
            double[][] spokeLine = {
                    target.getPointOnCircle(p1[0], p1[1], 0.0,
                            getPixelRelativeSize(ring), az),
                    target.getPointOnCircle(p1[0], p1[1], 0.0,
                            getPixelRelativeSize(farRadius), az) };
            shape.addLineSegment(spokeLine);
        }

        // draw and label rings
        for (double radiusInMiles = dRing; radiusInMiles <= maxRangeInMiles; radiusInMiles += dRing) {
            double radiusInMeters = convertFromMileToMeter
                    .convert(radiusInMiles);
            addRingToShape(target, shape, radiusInMeters);
            labelOverlayOrdinals(shape, radiusInMiles, radiusInMeters);
        }
        return shape;
    }

    private void addRingToShape(IGraphicsTarget target, IWireframeShape shape,
            double radiusInMeters) throws VizException {
        gc.setStartingGeographicPoint(center.x, center.y);
        double[] p1 = descriptor.worldToPixel(new double[] {
                gc.getStartingGeographicPoint().getX(),
                gc.getStartingGeographicPoint().getY() });
        for (double j = 0; j <= 359; j++) {
            double[][] pointOnCircle = {
                    target.getPointOnCircle(p1[0], p1[1], 0.0,
                            getPixelRelativeSize(radiusInMeters), j),
                    target.getPointOnCircle(p1[0], p1[1], 0.0,
                            getPixelRelativeSize(radiusInMeters), j + 1) };
            shape.addLineSegment(pointOnCircle);
        }
    }

    /**
     * Labels the wireframe overlay at the ordinals for the given radius in
     * meters.
     * 
     * @throws VizException
     */
    private void labelOverlayOrdinals(IWireframeShape shape,
            double radiusInMiles, double radiusInMeters) throws VizException {
        StringBuilder sb = new StringBuilder();
        double radiusInKilometers = convertFromMileToKilometer
                .convert(radiusInMiles);

        if (hasTilt) {
            double tiltInRadians = convertFromDegreesToRadians.convert(tilt);
            // From OB9 source. calculate the height of radar beam along the
            // surface distance.
            double beamElevMeters = tiltInRadians * radiusInMeters
                    + radiusInMeters * radiusInMeters / 1.7e7;

            // label north-east with height in feet MSL
            sb.setLength(0);
            double ftMSL = convertFromMeterToFoot
                    .convert(resourceSourceElevationInMeters + beamElevMeters);
            sb.append(String.format("%d", Math.round(ftMSL))).append("ftMSL");
            labelOnShape(ordinalNE, radiusInMeters, sb.toString(), shape);

            // label south-east with distance in miles
            sb.setLength(0);
            sb.append(String.format("%.0f", radiusInMiles)).append("Mi");
            labelOnShape(ordinalSE, radiusInMeters, sb.toString(), shape);

            // label south-west with height in kilometers AGL
            sb.setLength(0);

            double kmAgl = convertFromMeterToKilometer.convert(beamElevMeters);
            sb.append(String.format("%.1f", kmAgl)).append("kmAgl");
            labelOnShape(ordinalSW, radiusInMeters, sb.toString(), shape);

            // label north-west with distance in kilometers
            sb.setLength(0);
            sb.append(String.format("%.0f", radiusInKilometers)).append("km");
            labelOnShape(ordinalNW, radiusInMeters, sb.toString(), shape);
        } else {
            // label south-east and north-west with distance in miles
            sb.setLength(0);
            sb.append(radiusInMiles).append("Mi");
            labelOnShape(ordinalSE, radiusInMeters, sb.toString(), shape);
            labelOnShape(ordinalNW, radiusInMeters, sb.toString(), shape);

            // label north-east and south-west with distance in kilometers
            sb.setLength(0);
            sb.append(radiusInKilometers).append("km");
            labelOnShape(ordinalNE, radiusInMeters, sb.toString(), shape);
            labelOnShape(ordinalSW, radiusInMeters, sb.toString(), shape);
        }
    }

    /**
     * Writes a shape label on the point of circle of the given radius at the
     * given angle of rotation.
     * 
     * @param angle
     * @param radiusInMeters
     * @param label
     * @param target
     * @throws VizException
     */
    private void labelOnShape(double angle, double radiusInMeters,
            String label, IWireframeShape shape) throws VizException {
        // convert angle from 0..360 to -180..180
        angle = angle > 180 ? angle - 360.0 : angle;
        gc.setDirection(angle, radiusInMeters);
        double[] p = descriptor.worldToPixel(new double[] {
                gc.getDestinationGeographicPoint().getX(),
                gc.getDestinationGeographicPoint().getY() });
        shape.addLabel(label, p);
    }

    /*
     * Distance in miles from resource center to farthest display corner.
     */
    private double getMaxRingRadiusMiles(IExtent extent) {
        // upper left
        Envelope worldExtent = descriptor.pixelToWorld(extent);
        gc.setDestinationGeographicPoint(worldExtent.getMinX(),
                worldExtent.getMaxY());
        double maxDistance = gc.getOrthodromicDistance();

        // upper right
        gc.setDestinationGeographicPoint(worldExtent.getMaxX(),
                worldExtent.getMaxY());
        double tmp = gc.getOrthodromicDistance();
        maxDistance = Math.max(maxDistance, tmp);

        // lower left
        gc.setDestinationGeographicPoint(worldExtent.getMinX(),
                worldExtent.getMinY());
        tmp = gc.getOrthodromicDistance();
        maxDistance = Math.max(maxDistance, tmp);

        // lower right
        gc.setDestinationGeographicPoint(worldExtent.getMaxX(),
                worldExtent.getMinY());
        tmp = gc.getOrthodromicDistance();
        maxDistance = Math.max(maxDistance, tmp);
        maxDistance = gc.getEllipsoid().getAxisUnit()
                .getConverterTo(NonSI.MILE).convert(maxDistance);

        // limit miles of maxDistance.
        maxDistance = Math.min(maxDistance, 300);

        return maxDistance;
    }

    /**
     * For some calculations we need the pixel relative size to pass the
     * drawCircle for instance. This is a convenience method for calculating
     * this.
     * 
     * @param meters
     * @return
     */
    private double getPixelRelativeSize(double meters) {

        gc.setStartingGeographicPoint(center.x, center.y);
        gc.setDirection(90, meters);

        double[] p1 = descriptor.worldToPixel(new double[] {
                gc.getStartingGeographicPoint().getX(),
                gc.getStartingGeographicPoint().getY() });
        double[] p2 = descriptor.worldToPixel(new double[] {
                gc.getDestinationGeographicPoint().getX(),
                gc.getDestinationGeographicPoint().getY() });
        return Math.abs(p1[0] - p2[0]);
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        for (IWireframeShape shape : this.wireframeMap.values()) {
            shape.dispose();
        }
    }

    /**
     * Key class for retrieving wireframes from the map.
     * 
     * @author bgonzale
     * @version 1.0
     */
    public static class WireframeKey {
        final int hashCode;

        public WireframeKey(double tilt, double ringSize) {
            String key = new StringBuilder().append(tilt).append(ringSize)
                    .toString();
            this.hashCode = key.hashCode();
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            return hashCode == obj.hashCode();
        }
    }
}
