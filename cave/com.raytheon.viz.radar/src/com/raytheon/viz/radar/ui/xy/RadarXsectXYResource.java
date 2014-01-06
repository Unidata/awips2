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
package com.raytheon.viz.radar.ui.xy;

import java.util.Iterator;

import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2010            bsteffen     Initial creation
 * Dec 11, 2013 DR 16795   D. Friedman  Transform pixel coordinate in inspect
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class RadarXsectXYResource extends RadarXYResource implements
        IInsetMapResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarXsectXYResource.class);

    private LineString baseLine;

    private IWireframeShape line;

    /**
     * @param resourceData
     * @param loadProperties
     * @param interrogator
     * @param textContributor
     * @throws VizException
     */
    public RadarXsectXYResource(RadarResourceData resourceData,
            LoadProperties loadProperties, IRadarInterrogator interrogator)
            throws VizException {
        super(resourceData, loadProperties, interrogator);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.ui.xy.RadarXYResource#populatePlotObjects(com.
     * raytheon.edex.plugin.radar.RadarRecord,
     * com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void populatePlotObjects(RadarRecord radarRecord,
            IGraphicsTarget target) throws VizException {
        super.populatePlotObjects(radarRecord, target);
        initBaseLine(radarRecord);
        // Remove old x axis labels
        Iterator<Coordinate> it = screenStringMap.keySet().iterator();
        while (it.hasNext()) {
            double y = it.next().y;
            if (y > 470 && y < 490) {
                it.remove();
            }
        }
        // Make new x axis labels
        double az1 = radarRecord.getProductDependentValue(3) * 0.1;
        double ran1 = radarRecord.getProductDependentValue(4) * 0.1;
        double az2 = radarRecord.getProductDependentValue(5) * 0.1;
        double ran2 = radarRecord.getProductDependentValue(6) * 0.1;
        screenStringMap.put(new Coordinate(51, 484),
                String.format("%.0f/%.0f", az1, ran1));
        screenStringMap.put(new Coordinate(484, 484),
                String.format("%.0f/%.0f", az2, ran2));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource#getInsetMapLocation()
     */
    @Override
    public Geometry getInsetMapLocation() {
        if (baseLine == null) {
            initBaseLine(getRadarRecord(displayedDate));
        }
        return baseLine;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource#paintInsetMap(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.uf.viz.core.map.MapDescriptor)
     */
    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {
        if (baseLine == null) {
            initBaseLine(getRadarRecord(displayedDate));
        }
        if (baseLine != null) {
            if (line == null) {
                line = target.createWireframeShape(false, insetMapDescriptor);
                line.addLineSegment(baseLine.getCoordinates());
                line.compile();
            }
            target.drawWireframeShape(line,
                    getCapability(ColorableCapability.class).getColor(), 2.0f);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.ui.xy.RadarXYResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        line.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.RadarImageResource#buildCoverage(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.viz.radar.RadarTimeRecord.RadarTiltRecord)
     */
    @Override
    public PixelCoverage buildCoverage(IGraphicsTarget target,
            VizRadarRecord rec) throws VizException {
        int iStart = rec.getIstart();
        int jStart = rec.getJstart();

        double width = 460;
        double height = 430;

        width *= scalar;
        height *= scalar;

        double upper = (yOffsetNWP + jStart) * scalar;
        double lower = upper + height;
        double left = (xOffsetNWP + iStart) * scalar;
        double right = left + width;

        return new PixelCoverage(new Coordinate(right, lower), new Coordinate(
                left, lower), new Coordinate(left, upper), new Coordinate(
                right, upper));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.AbstractRadarResource#inspect(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        DrawableImage image = images.get(displayedDate);
        try {
            Coordinate c = latLon.asLatLon();
            double[] worldCoord = descriptor.pixelToWorld(new double[] {
                    c.x, c.y });
            IExtent extent = image.getCoverage().getExtent();
            // Convert the screen coordinate to a coordinate within the image.
            // 0,0 is the upper left and 1,1 is the lower right of the iamge.
            double xRat = (worldCoord[0] - extent.getMinX()) / extent.getWidth();
            double yRat = (worldCoord[1] - extent.getMinY()) / extent.getHeight();
            return super.inspect(new ReferencedCoordinate(new Coordinate(xRat,
                    yRat)));
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    private void initBaseLine(RadarRecord radarRecord) {
        if (radarRecord == null) {
            baseLine = null;
            return;
        }
        double az1 = radarRecord.getProductDependentValue(3);
        double ran1 = radarRecord.getProductDependentValue(4);
        double az2 = radarRecord.getProductDependentValue(5);
        double ran2 = radarRecord.getProductDependentValue(6);

        // I think ran comes in at 10 * Degrees
        az1 = Math.toRadians(az1 * 0.1);
        az2 = Math.toRadians(az2 * 0.1);
        // I think ran comes in at 10 * Nautical Miles
        ran1 = ran1 / 5.4 * radarRecord.getGateResolution();
        ran2 = ran2 / 5.4 * radarRecord.getGateResolution();

        DirectPosition2D start = new DirectPosition2D(ran1 * Math.sin(az1),
                ran1 * Math.cos(az1));
        DirectPosition2D end = new DirectPosition2D(ran2 * Math.sin(az2), ran2
                * Math.cos(az2));
        try {
            MathTransform toLL = MapUtil.getTransformToLatLon(radarRecord
                    .getCRS());
            toLL.transform(start, start);
            toLL.transform(end, end);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            baseLine = null;
            return;
        }
        if (start.equals(end)) {
            baseLine = null;
            return;
        }
        Coordinate[] coords = new Coordinate[2];
        coords[0] = new Coordinate(start.x, start.y);
        coords[1] = new Coordinate(end.x, end.y);
        // try {
        // double m = JTS.orthodromicDistance(coords[0], coords[1], MapUtil
        // .getLatLonProjection());
        // double nmi = SI.METER.getConverterTo(NonSI.NAUTICAL_MILE)
        // .convert(m);
        // System.out.println("Len = " + nmi);
        // } catch (Exception e) {
        // e.printStackTrace();
        // }

        baseLine = (new GeometryFactory()).createLineString(coords);
    }

}
