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
package com.raytheon.uf.common.dataplugin.radar.dataaccess;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket.HdaHailPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket.SCITDataCell;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket.SpecialGraphicPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket.StormIDPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket.TVSPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.Pair;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.MultiPoint;
import org.locationtech.jts.geom.Point;

/**
 * Methods that create IGeometryData for radar products
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2016 2671       tgurney     Initial creation
 * Aug 31, 2016 2671       tgurney     Add makeMesocycloneGeom
 * Sep 08, 2016 2671       tgurney     Add makeStormTrackGeom
 * Sep 27, 2016 2671       tgurney     Add makeHailIndexGeom
 * Sep 28, 2016 2671       tgurney     Add makeTVSGeom
 *
 * </pre>
 *
 * @author tgurney
 */

public class RadarGeometryDataUtil {

    /**
     * Data representing storm track for a single storm.
     */
    private static class StormTrackData {
        /**
         * Index of the point corresponding to the storm's current location. All
         * points before that are past, and all points after that are forecast.
         */
        public Integer currentLocIndex;

        public MultiPoint points;

        public String stormId = "";
    }

    private static final GeometryFactory geomFactory = new GeometryFactory();

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarGeometryDataUtil.class);

    private RadarGeometryDataUtil() {
        // static interface only
    }

    public static DefaultGeometryData[] makeMeltingLayerGeom(
            RadarRecord radarRecord) {
        Map<Integer, Coordinate[]> coords = RadarRecordUtil
                .buildMeltingLayerCoordinates(radarRecord);
        List<LinearRing> rings = new ArrayList<>();
        for (Integer num : coords.keySet()) {
            rings.add(geomFactory.createLinearRing(coords.get(num)));
        }
        GeometryCollection geomCollection = geomFactory
                .createGeometryCollection(rings.toArray(new Geometry[0]));
        DefaultGeometryData rval = new DefaultGeometryData();
        rval.setGeometry(geomCollection);
        return new DefaultGeometryData[] { rval };
    }

    /**
     * @return (circleId, point) for specific location, or null if the data for
     *         that location is nonexistent or invalid.
     */
    private static Pair<String, SpecialGraphicPoint> getMesocyclonePoint(
            RadarDataKey currLoc, RadarRecord radarRecord) {
        Integer productCode = radarRecord.getProductCode();
        RadarDataPoint currStorm = radarRecord.getSymbologyData().get(currLoc);

        HashMap<Integer, SymbologyPoint> displayPointData = currStorm
                .getDisplayPointData().get(productCode);
        if (displayPointData == null) {
            statusHandler.warn("Mesocyclone at " + currLoc + " has no "
                    + "points. Expected 1");
            return null;
        }

        Collection<SymbologyPacket> textPackets = currStorm
                .getDisplayPacketData().get(productCode).values();
        if (textPackets.size() != 1) {
            statusHandler.warn("Mesocyclone at " + currLoc + " has "
                    + textPackets.size() + " text packets. Expected 1");
            return null;
        }

        SymbologyPacket textPacket = textPackets.iterator().next();
        if (!(textPacket instanceof TextSymbolPacket)) {
            statusHandler.warn("Mesocyclone at " + currLoc + ": Expected "
                    + "a " + TextSymbolPacket.class.getSimpleName() + ", got a "
                    + textPacket.getClass().getName());
            return null;
        }

        Collection<SymbologyPoint> currPoints = displayPointData.values();
        if (currPoints.size() != 1) {
            statusHandler.warn("Mesocyclone at " + currLoc + " has "
                    + currPoints.size() + " points. Expected 1");
            return null;
        }

        SymbologyPoint currPoint = currPoints.iterator().next();
        if (!(currPoint instanceof SpecialGraphicPoint)) {
            statusHandler.warn("Mesocyclone at " + currLoc + ": Expected "
                    + "a " + SpecialGraphicPoint.class.getSimpleName()
                    + ", got a " + currPoint.getClass().getName());
            return null;
        }

        String circleId = ((TextSymbolPacket) textPacket).getTheText().trim();
        SpecialGraphicPoint point = (SpecialGraphicPoint) currPoint;
        return new Pair<>(circleId, point);
    }

    /**
     * @return One geometry data per mesocyclone. The geometry itself is a
     *         single point. Radius is stored in the "radiusKm" attribute. Other
     *         tabular data are stored in the 'tableData' attribute.
     */
    public static DefaultGeometryData[] makeMesocycloneGeom(
            RadarRecord radarRecord) {
        List<DefaultGeometryData> geomDatas = new ArrayList<>();

        for (RadarDataKey currLoc : radarRecord.getSymbologyData().keySet()) {
            DefaultGeometryData geomData = new DefaultGeometryData();
            Pair<String, SpecialGraphicPoint> mesoPoint = getMesocyclonePoint(
                    currLoc, radarRecord);
            if (mesoPoint == null) {
                // failed validation
                continue;
            }
            String circleId = mesoPoint.getFirst();
            SpecialGraphicPoint graphicPoint = mesoPoint.getSecond();

            // Make coordinate from graphic point
            GeneralGridGeometry gg = RadarRecordUtil
                    .getRadarGraphicsGridGeometry(radarRecord);
            Coordinate coord = null;
            try {
                coord = new ReferencedCoordinate(
                        RadarRecordUtil.rectifyCoordinate(
                                new Coordinate(graphicPoint.i, graphicPoint.j)),
                        gg, Type.GRID_CENTER).asLatLon();
            } catch (TransformException | FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                continue;
            }

            // Get tabular data
            Map<MapValues, String> dataMap = radarRecord.getMapProductVals()
                    .get(MapValues.MESO_TYPE).get(circleId);
            if (dataMap == null) {
                statusHandler.warn("Mesocyclone at " + currLoc
                        + ": No tabular data found");
                continue;
            }
            Map<String, String> newDataMap = new HashMap<>();
            for (Entry<MapValues, String> mesoData : dataMap.entrySet()) {
                // Remove MESO_ prefix
                newDataMap.put(mesoData.getKey().toString().substring(5),
                        mesoData.getValue());
            }
            // Radius is in 1/4 km; convert to km
            double radius = graphicPoint.getPointFeatureAttr() / 4.0;
            newDataMap.put("RADIUS_KM", Double.toString(radius));
            geomData.addAttribute("tableData", newDataMap);
            geomData.setGeometry(geomFactory.createPoint(coord));
            geomDatas.add(geomData);
        }
        return geomDatas.toArray(new DefaultGeometryData[0]);
    }

    /**
     * @return One geometry data per storm. The geometry itself is a multipoint.
     *         Tabular data is stored in 'tableData' attribute. The current
     *         location of the storm is the point with index specified by
     *         "currentLocIndex" attribute. All points before that index are
     *         past and all points after it are forecast.
     */
    public static DefaultGeometryData[] makeStormTrackGeom(
            RadarRecord radarRecord) {
        List<DefaultGeometryData> geomDatas = new ArrayList<>();

        for (RadarDataKey currLoc : radarRecord.getSymbologyData().keySet()) {

            DefaultGeometryData geomData = new DefaultGeometryData();
            // Get storm ID and all points
            RadarDataPoint currStorm = radarRecord.getSymbologyData()
                    .get(currLoc);
            StormTrackData stormTrackData = getStormTrackPoints(currStorm,
                    RadarRecordUtil.getRadarGraphicsGridGeometry(radarRecord));
            String stormId = stormTrackData.stormId;
            if ("".equals(stormId)) {
                statusHandler
                        .warn("Storm track at " + currLoc + " has no storm ID");
                continue;
            }

            // Get tabular data
            Map<MapValues, String> dataMap = radarRecord.getMapProductVals()
                    .get(MapValues.STI_TYPE).get(stormId);
            if (dataMap == null) {
                statusHandler.warn("Storm track " + stormId + " at " + currLoc
                        + ": No tabular data found");
                continue;
            }
            Map<String, String> newDataMap = new HashMap<>();
            for (Entry<MapValues, String> stiData : dataMap.entrySet()) {
                // Remove STI_ prefix
                newDataMap.put(stiData.getKey().toString().substring(4),
                        stiData.getValue());
            }
            geomData.addAttribute("currentLocIndex",
                    stormTrackData.currentLocIndex.toString());
            newDataMap.put("STORM_ID", stormId);
            geomData.addAttribute("tableData", newDataMap);
            geomData.setGeometry(stormTrackData.points);
            geomDatas.add(geomData);
        }

        return geomDatas.toArray(new DefaultGeometryData[0]);
    }

    public static DefaultGeometryData[] makeHailIndexGeom(
            RadarRecord radarRecord) {
        List<DefaultGeometryData> geomDatas = new ArrayList<>();

        for (RadarDataKey currLoc : radarRecord.getSymbologyData().keySet()) {

            HashMap<Integer, HashMap<Integer, SymbologyPoint>> displayPointData = radarRecord
                    .getSymbologyData().get(currLoc).getDisplayPointData();

            HashMap<Integer, SymbologyPoint> pointMap = displayPointData
                    .get(radarRecord.getProductCode());
            if (pointMap == null) {
                statusHandler.warn(
                        "Hail index at " + currLoc + " has no " + "points.");
                continue;
            }

            // Get coordinate and storm id
            GeneralGridGeometry gg = RadarRecordUtil
                    .getRadarGraphicsGridGeometry(radarRecord);
            DefaultGeometryData geomData = new DefaultGeometryData();
            String stormId = "";
            Coordinate coord = null;
            for (SymbologyPoint point : pointMap.values()) {
                if (point instanceof HdaHailPoint) {
                    try {
                        coord = new ReferencedCoordinate(
                                RadarRecordUtil.rectifyCoordinate(
                                        new Coordinate(((HdaHailPoint) point).i,
                                                ((HdaHailPoint) point).j)),
                                gg, Type.GRID_CENTER).asLatLon();
                    } catch (TransformException | FactoryException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                        break;
                    }
                } else if (point instanceof StormIDPoint) {
                    stormId = ((StormIDPoint) point).getStormID();
                }
            }
            if (coord == null) {
                continue;
            }

            // Get tabular data
            Map<MapValues, String> dataMap = radarRecord.getMapProductVals()
                    .get(MapValues.HAIL_TYPE).get(stormId);
            if (dataMap == null) {
                statusHandler.warn("Hail index " + stormId + " at " + currLoc
                        + ": No tabular data found");
                continue;
            }
            Map<String, String> newDataMap = new HashMap<>();
            for (Entry<MapValues, String> stiData : dataMap.entrySet()) {
                // Remove HI_ prefix
                newDataMap.put(stiData.getKey().toString().substring(3),
                        stiData.getValue());
            }
            newDataMap.put("STORM_ID", stormId);
            geomData.addAttribute("tableData", newDataMap);
            geomData.setGeometry(geomFactory.createPoint(coord));
            geomDatas.add(geomData);
        }

        return geomDatas.toArray(new DefaultGeometryData[0]);
    }

    /**
     * @return (stormId, point) for specific location, or null if the data for
     *         that location is nonexistent or invalid.
     */
    private static Pair<String, TVSPoint> getTVSPoint(RadarDataKey currLoc,
            RadarRecord radarRecord) {
        Integer productCode = radarRecord.getProductCode();
        RadarDataPoint currStorm = radarRecord.getSymbologyData().get(currLoc);

        HashMap<Integer, SymbologyPoint> displayPointData = currStorm
                .getDisplayPointData().get(productCode);
        if (displayPointData == null) {
            statusHandler.warn(
                    "TVS at " + currLoc + " has no " + "points. Expected 1");
            return null;
        }

        Collection<SymbologyPacket> displayPackets = currStorm
                .getDisplayPacketData().get(productCode).values();
        if (displayPackets.size() != 1) {
            statusHandler.warn("TVS at " + currLoc + " has "
                    + displayPackets.size() + " text packets. Expected 1");
            return null;
        }

        SymbologyPacket stormIdPacket = displayPackets.iterator().next();
        if (!(stormIdPacket instanceof StormIDPacket)) {
            statusHandler.warn("TVS at " + currLoc + ": Expected " + "a "
                    + StormIDPacket.class.getSimpleName() + ", got a "
                    + stormIdPacket.getClass().getName());
            return null;
        }

        Collection<SymbologyPoint> currPoints = displayPointData.values();
        if (currPoints.size() != 1) {
            statusHandler.warn("TVS at " + currLoc + " has " + currPoints.size()
                    + " points. Expected 1");
            return null;
        }

        SymbologyPoint currPoint = currPoints.iterator().next();
        if (!(currPoint instanceof TVSPoint)) {
            statusHandler.warn("TVS at " + currLoc + ": Expected " + "a "
                    + TVSPoint.class.getSimpleName() + ", got a "
                    + currPoint.getClass().getName());
            return null;
        }

        StormIDPoint[] stormIdPoints = ((StormIDPacket) stormIdPacket)
                .getPoints();
        if (stormIdPoints.length != 1) {
            statusHandler.warn("TVS at " + currLoc + " has "
                    + stormIdPoints.length + " storm ID points. Expected 1");
            return null;
        }
        TVSPoint point = (TVSPoint) currPoint;
        String stormId = stormIdPoints[0].getStormID();
        return new Pair<>(stormId, point);
    }

    public static DefaultGeometryData[] makeTVSGeom(RadarRecord radarRecord) {
        List<DefaultGeometryData> geomDatas = new ArrayList<>();
        GeneralGridGeometry gg = RadarRecordUtil
                .getRadarGraphicsGridGeometry(radarRecord);

        for (RadarDataKey currLoc : radarRecord.getSymbologyData().keySet()) {

            DefaultGeometryData geomData = new DefaultGeometryData();
            // Get point and storm ID
            Pair<String, TVSPoint> tvsData = getTVSPoint(currLoc, radarRecord);
            if (tvsData == null) {
                // failed validation
                continue;
            }
            String stormId = tvsData.getFirst();
            TVSPoint point = tvsData.getSecond();
            Coordinate coord;
            try {
                coord = new ReferencedCoordinate(
                        RadarRecordUtil.rectifyCoordinate(
                                new Coordinate(point.i, point.j)),
                        gg, Type.GRID_CENTER).asLatLon();
            } catch (TransformException | FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                continue;
            }

            // Get tabular data
            Map<MapValues, String> dataMap = radarRecord.getMapProductVals()
                    .get(MapValues.TVS_TYPE).values().iterator().next();
            if (dataMap == null) {
                statusHandler.warn("TVS " + stormId + " at " + currLoc
                        + ": No tabular data found");
                continue;
            }
            Map<String, String> newDataMap = new HashMap<>();
            for (Entry<MapValues, String> stiData : dataMap.entrySet()) {
                // Remove TVS_ prefix
                newDataMap.put(stiData.getKey().toString().substring(4),
                        stiData.getValue());
            }
            newDataMap.put("STORM_ID", stormId);
            newDataMap.put("ELEVATED", point.elevated ? "Y" : "N");

            geomData.addAttribute("tableData", newDataMap);
            geomData.setGeometry(geomFactory.createPoint(coord));
            geomDatas.add(geomData);
        }
        return geomDatas.toArray(new DefaultGeometryData[0]);
    }

    private static StormTrackData getStormTrackPoints(RadarDataPoint stiPoint,
            GeneralGridGeometry gridGeometry) {
        List<Point> points = new ArrayList<>();
        SCITDataPacket forecastPacket = null;
        SCITDataPacket pastPacket = null;
        TextSymbolPacket currentPacket = null;
        StormIDPoint stormId = null;
        StormTrackData stormTrackData = new StormTrackData();

        HashMap<Integer, HashMap<Integer, SymbologyPacket>> displayPacketData = stiPoint
                .getDisplayPacketData();

        for (Integer type : displayPacketData.keySet()) {
            for (SymbologyPacket packet : displayPacketData.get(type)
                    .values()) {
                if (packet instanceof SCITDataPacket) {
                    for (SCITDataCell currCell : ((SCITDataPacket) packet)
                            .getPoints()) {
                        if (currCell.getText().contains("!")) {
                            // '!' indicates past
                            pastPacket = (SCITDataPacket) packet;
                            continue;
                        } else if (currCell.getText().contains("#")) {
                            // '#' indicates forecast
                            forecastPacket = (SCITDataPacket) packet;
                            continue;
                        }
                    }
                } else if (packet instanceof TextSymbolPacket) {
                    if (((TextSymbolPacket) packet).getTheText()
                            .contains("\"")) {
                        // '"' indicates current storm location
                        currentPacket = (TextSymbolPacket) packet;
                    }
                } else if (packet instanceof StormIDPacket) {
                    // Should only have one point
                    stormId = ((StormIDPacket) packet).getPoints()[0];
                }
            }
        }

        if (pastPacket != null) {
            points.addAll(createSCITDataCell(pastPacket, gridGeometry));
        }
        if (currentPacket != null) {
            points.addAll(createSCITDataCell(currentPacket, gridGeometry));
            stormTrackData.currentLocIndex = points.size() - 1;
        }
        if (forecastPacket != null) {
            points.addAll(createSCITDataCell(forecastPacket, gridGeometry));
        }
        stormTrackData.points = geomFactory
                .createMultiPoint(points.toArray(new Point[0]));
        if (stormId != null) {
            stormTrackData.stormId = stormId.getStormID();
        }
        return stormTrackData;
    }

    /**
     * @return List of points from a single SymbologyPacket
     */
    private static List<Point> createSCITDataCell(SymbologyPacket packet,
            GeneralGridGeometry gridGeometry) {
        List<Point> points = new ArrayList<>();
        List<SCITDataCell> cells;
        if (packet instanceof TextSymbolPacket) {
            TextSymbolPacket pkt = (TextSymbolPacket) packet;
            cells = new ArrayList<>();
            cells.add(new SCITDataCell());
            cells.get(0).setText(pkt.getTheText());
            cells.get(0).setI(pkt.getI());
            cells.get(0).setJ(pkt.getJ());
        } else {
            SCITDataPacket pkt = (SCITDataPacket) packet;
            cells = pkt.getPoints();
        }
        if (cells == null) {
            return points;
        }

        for (SCITDataCell point : cells) {
            Coordinate coord = null;
            try {
                coord = new ReferencedCoordinate(
                        RadarRecordUtil.rectifyCoordinate(
                                new Coordinate(point.i, point.j)),
                        gridGeometry, Type.GRID_CENTER).asLatLon();
            } catch (TransformException | FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                continue;
            }
            points.add(geomFactory.createPoint(coord));
        }
        return points;
    }
}
