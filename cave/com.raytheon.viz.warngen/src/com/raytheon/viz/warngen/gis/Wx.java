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
package com.raytheon.viz.warngen.gis;

import java.awt.geom.Point2D;
import java.io.File;
import java.text.ParsePosition;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.format.MeasurementParseException;
import javax.measure.quantity.Speed;

import org.apache.commons.lang3.Validate;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.geotools.api.referencing.FactoryException;
import org.geotools.api.referencing.crs.CoordinateReferenceSystem;
import org.geotools.api.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.config.GeospatialConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PathcastConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration.SearchMethod;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialFactory;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil;
import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.DestinationGeodeticCalculator;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackDisplay;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.warngen.PreferenceUtil;
import com.raytheon.viz.warngen.WarngenException;
import com.raytheon.viz.warngen.config.AbstractDbSourceDataAdaptor;
import com.raytheon.viz.warngen.config.DataAdaptorFactory;
import com.raytheon.viz.warngen.util.Abbreviation;
import com.raytheon.viz.warngen.util.AdjustAngle;
import com.raytheon.viz.warngen.util.DateUtil;

import si.uom.SI;
import tech.units.indriya.format.SimpleUnitFormat;

/**
 *
 * Wx
 *
 * <pre>
 *
 *    SOFTWARE HISTORY
 *
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 1, 2007             chammack    Initial Creation.
 *    Mar 01, 2012 DR13596    Qinglu Lin  Call GisUtil.restoreAlaskaLon()
 *                                        in getClosestPoints().
 *    Apr 18, 2012 DR14733    Qinglu Lin  David's fix is used, which creates another
 *                                        ClosestPoint object in the for loop
 *                                        that loops over availablePoints.
 *    May 21, 2012 DR14480    Qinglu Lin  Added code to prevent duplicate cities
 *                                        in pathcast.
 *    Oct 05, 2012 DR15429    Qinglu Lin  Updated code to keep duplicate names of cities
 *                                        which are at different locations in pathcast.
 *    Oct 17, 2012            jsanchez    Moved the path cast data collecting to a separate class.
 *    Jan 31, 2013 1557       jsanchez    Used allowDuplicates flag to collect points with duplicate names.
 *    Feb 12, 2013 1600       jsanchez    Used adjustAngle method from AbstractStormTrackResource.
 *    Mar  5, 2013 1600       jsanchez    Used AdjustAngle instead of AbstractStormTrackResource to handle angle adjusting.
 *    Mar 25, 2013 1605       jsanchez    Checks if a storm location is over an urban bound area.
 *    Apr 24, 2013 1943       jsanchez    Calculated partOfArea for a storm location over an urban bound area.
 *    May  2, 2013 1963       jsanchez    Referenced calculateLocationPortion from GisUtil.
 *    Jun 20, 2013 16224      Qinglu Lin  Updated pathcast() by removing restriction of "i < maxCount" at line 478,
 *                                        and added findPointsToBeRemoved(), computeAngle(), and remove pathcast's
 *                                        points that are in the past.
 *    Jun 24, 2013 DR 16317   D. Friedman Handle "motionless" track.
 *    Jun 25, 2013 16224      Qinglu Lin  Resolved the issue with "Date start" for pathcast in CON.
 *    Dec  4, 2013 2604       jsanchez    Refactored GisUtil.
 *    Apr 29, 2014 3033       jsanchez    Updated method to retrieve files in localization.
 *    Jun 17, 2014 DR 17390   Qinglu Lin  Updated getClosestPoints().
 *    May  7, 2015 ASM #17438 D. Friedman Clean up debug and performance logging.
 *    May 11, 2016 5622       jschmid     Allow use of simulated time using TimeUtil.newCalendar().
 *    Apr 15, 2019 7596       lsingh      Updated units framework to JSR-363.
 *                                        Handled unit conversion
 *    Jul 29, 2020 ASM #21988 dhaines     Added fix for DR21988 - Time zone can be incorrect for cities in pathcast for line
 *                                        of storms along time zone boundary.
 *    Feb 22, 2021 8258       mapeters    Thread closest points calculations
 *    Nov 29, 2021 ASM #22724 dhaines     Changes for DR 22724 - Some Cities Can't be Added to Pathcasts
 * </pre>
 *
 * @author chammack
 */
public class Wx {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Wx.class);

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("WG");

    private static final ExecutorService executor = Executors
            .newFixedThreadPool(4);

    private long wwaStopTime;

    private long wwaStartTime;

    private StormTrackState stormTrackState;

    private WarngenConfiguration config;

    private Geometry stormLocation;

    private Geometry warningPolygon;

    /**
     * Constructor
     *
     * @param eventCenterLocation
     * @param motionSpeedInMetersPerSec
     * @param motionDir
     * @param obsTime
     * @param wwaStartTime
     * @param wwaStopTime
     * @param interval
     * @throws VizException
     */
    public Wx(WarngenConfiguration config, StormTrackState stormTrackState,
            Coordinate[] stormLocations, long wwaStartTime, long wwaStopTime,
            Geometry polygon) throws VizException {

        // --- Begin argument validation ---
        Validate.isTrue(stormTrackState.speed >= 0,
                "Motion speed must be positive");
        Validate.isTrue(wwaStartTime > 0, "WWA start time must be specified");
        Validate.isTrue(wwaStopTime > 0, "WWA stop time must be specified");
        // --- End argument validation ---

        try {
            this.config = config;
            if (stormLocations.length == 1) {
                stormLocation = new GeometryFactory()
                        .createPoint(stormLocations[0]);
            } else {
                stormLocation = new GeometryFactory()
                        .createLineString(stormLocations);
            }
            this.stormTrackState = stormTrackState;
            this.wwaStopTime = wwaStopTime;
            this.wwaStartTime = wwaStartTime;
            this.warningPolygon = polygon;
        } catch (Exception e) {
            throw new VizException("Error setting up universe", e);
        }
    }

    /**
     * returns coordinate based on supplied starting coordinate, angle, and distance
     * @param c
     * @param gc
     * @param angle
     * @param distance
     * @return
     */
    private Coordinate computePoint(Coordinate c, DestinationGeodeticCalculator gc, double angle, double distance) {
        gc.setStartingGeographicPoint(c.x, c.y);
        gc.setDirection(StormTrackDisplay
                .adjustAngle(angle), distance);
        Point2D p = gc.getDestinationGeographicPoint();

        return new Coordinate(p.getX(), p.getY());
    }

    /**
     * This method returns the pathcast given the proper configuration
     * parameters.
     *
     * @param pathcastConfiguration
     * @param geospatialConfig
     * @return the Pathcast
     * @throws WarngenException
     */
    public PathCast[] pathcast(String localizedSite) throws WarngenException {
        if (config.getPathcastConfig() == null) {
            // Can't pathcast if no config
            return null;
        }
        long t0 = System.currentTimeMillis();
        GeospatialConfiguration geospatialConfig = config.getGeospatialConfig();
        PathcastConfiguration pathcastConfiguration = config
                .getPathcastConfig();
        UnitConverter distanceToMeters = config.getUnitDistance()
                .getConverterTo(SI.METRE);

        int maxCount = pathcastConfiguration.getMaxResults();
        int maxGroup = pathcastConfiguration.getMaxGroup();
        String areaField = pathcastConfiguration.getAreaField();
        String areaSource = geospatialConfig.getAreaSource();
        String parentAreaField = pathcastConfiguration.getParentAreaField();
        String areaNotationField = pathcastConfiguration.getAreaNotationField();
        String areaNotationAbbrevField = pathcastConfiguration
                .getAreaNotationTranslationFile();
        String timezoneField = geospatialConfig.getTimezoneField();
        String pointSource = pathcastConfiguration.getPointSource();
        String pointField = pathcastConfiguration.getPointField().toLowerCase();
        Map<String, RequestConstraint> pointFilter = pathcastConfiguration
                .getFilter();
        if (pointFilter != null) {
            // Process substitutes for filter
            for (RequestConstraint rc : pointFilter.values()) {
                rc.setConstraintValue(PreferenceUtil
                        .substitute(rc.getConstraintValue(), localizedSite));
            }
        }
        int delta = pathcastConfiguration.getDelta() * 60 * 1000;
        int intervalInMillis = pathcastConfiguration.getInterval() * 60 * 1000;

        List<String> fields = pathcastConfiguration.getSortBy() != null
                ? Arrays.asList(pathcastConfiguration.getSortBy())
                : new ArrayList<>();
                
        double thresholdInMeters;
        Coordinate[] stormLocations = stormLocation.getCoordinates();
        boolean lineOfStorms = stormLocations.length > 1;
        
        // --- Begin argument checking ---
        if (!lineOfStorms) {
            thresholdInMeters = distanceToMeters.convert(pathcastConfiguration.getDistanceThreshold());
            Validate.isTrue(
                    thresholdInMeters > 0,
                    "Distance threshold must be greater than zero for a single storm. Check .xml if distanceThreshold is set in pathcastConfig.\n");
        } else {
            thresholdInMeters = distanceToMeters.convert(pathcastConfiguration.getDistanceThresholdLOS());
            Validate.isTrue(
                    thresholdInMeters >= 0,
                    "Distance threshold must be greater than or equal to zero for a line of storms. Check .xml if distanceThresholdLOS is set in pathcastConfig.\n");
        }
        
        Validate.isTrue(
        		maxCount > 0,
                "Max count must be greater than zero. Check .xml if maxCount is set in pathcastConfig.\n");
        Validate.isTrue(
        		maxGroup > 0,
                "Max group must be greater than zero. Check .xml if maxGroup is set in pathcastConfig.\n");
        Validate.notNull(
        		areaField,
                "An area field must be provided. Check .xml if areaField is set in pathcastConfig.\n");
        Validate.notNull(
        		pointField,
                "A point field must be provided. Check .xml if pointField is set in pathcastConfig.\n");
        Validate.isTrue(
                (!(areaNotationAbbrevField != null
                        && areaNotationField == null)),
                "Area notation field must be provided if translation is specified. Check .xml if areaNotationField and areaNotationTranslationFile are set in pathcastConfig.\n");
        Validate.notNull(
        		areaSource,
                "Area source must be provided for pointcast to operate. Check .xml if areaSource is set in geosptatialConfig.\n");
        Validate.notNull(
        		pointSource,
                "Point source must be provided for pointcast to operate. Check .xml if pointSource is set in geospatialConfig.\n");
        // --- End argument checking ---

        GeometryFactory gf = new GeometryFactory();

        try {
            Abbreviation areaTypeAbbrev = null;
            String trxFileStr = pathcastConfiguration
                    .getAreaNotationTranslationFile();
            if (trxFileStr != null) {
                File trxFile = WarnFileUtil
                        .findFileInLocalizationIncludingBackupSite(
                                areaNotationAbbrevField, localizedSite, null)
                        .getFile();
                if (!trxFile.exists()) {
                    throw new WarngenException(
                            "Translation file does not exist: " + trxFileStr);
                }
                areaTypeAbbrev = new Abbreviation(trxFile);
            }

            MathTransform latLonToLocal = null;
            CoordinateReferenceSystem crs = null;
            Geometry bufferedPathCastArea = null;
            List<PathCast> pathCasts = new ArrayList<>();

            if (stormTrackState.isNonstationary()) {
                Date stormTime = TimeUtil.newCalendar().getTime();
                Date start = DateUtil.roundDate(
                        new Date(stormTime.getTime() + delta),
                        pathcastConfiguration.getInterval());
                long instant = start.getTime();
                DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();

                double deltaDistance = stormTrackState.speed * ((start.getTime() - stormTime.getTime()) / 1000);
                double intervalDistance = stormTrackState.speed * (intervalInMillis / 1000);
                double warningDuration = wwaStopTime - start.getTime();
                double distanceOfExpiration = stormTrackState.speed * (warningDuration / 1000);
                int pcIndex = 0;

                // Compute where stormLocations would be given the
                // speed/angle of the state
                while (instant <= wwaStopTime) {
                    PathCast cast = new PathCast();
                    cast.time = new Date(instant);
                    cast.index = pcIndex;
                    pathCasts.add(cast);
                    long time = (cast.time.getTime() - stormTime.getTime()) / 1000;
                    double distance = stormTrackState.speed * time;

                    // offset the pathcasts by half the distance to err on the side of warning cities sooner than later
                    distance = distance - (intervalDistance / 2);
                    if (!lineOfStorms) {
                        // single storm
                        Coordinate loc = stormLocations[0];
                        Coordinate c1 = computePoint(loc, gc, stormTrackState.angle, distance);
                        Coordinate c2 = computePoint(c1, gc, stormTrackState.angle, intervalDistance);
                        Coordinate[] pcSegmentCoords = {c1, c2};
                        Geometry pcSegmentGeom = gf.createLineString(pcSegmentCoords);

                        Coordinate c = pcSegmentGeom.getCentroid().getCoordinate();
                        crs = MapUtil.constructStereographic(
                                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                                c.y, c.x);
                        latLonToLocal = MapUtil.getTransformFromLatLon(crs);
                        pcSegmentGeom = JTS.transform(pcSegmentGeom, latLonToLocal);
                        pcSegmentGeom = JTS.transform(pcSegmentGeom.buffer(thresholdInMeters),latLonToLocal.inverse());

                        if (pcIndex == 0) {
                            // create the bufferedPathCastArea on first pass.
                            Coordinate startBound1 = computePoint(c1, gc, (stormTrackState.angle + 90), thresholdInMeters);
                            Coordinate startBound2 = computePoint(c1, gc, (stormTrackState.angle - 90), thresholdInMeters);
                            Coordinate endVectorCoord = computePoint(loc, gc, stormTrackState.angle, distanceOfExpiration + 
                                    intervalDistance);
                            Coordinate endBound1 = computePoint(endVectorCoord, gc, (stormTrackState.angle + 90), 
                                    thresholdInMeters);
                            Coordinate endBound2 = computePoint(endVectorCoord, gc, (stormTrackState.angle - 90), 
                                    thresholdInMeters);
                            bufferedPathCastArea = gf.createPolygon(new Coordinate[] {startBound1, endBound1, endBound2, 
                                    startBound2, startBound1});
                        }

                        cast.pcGeom = pcSegmentGeom;
                    } else {
                        // line of storms
                        if (pcIndex == 0) {
                            // create the bufferedPathCastArea on first pass.
                            Coordinate[] bufferedPCAreaStartBound = new Coordinate[stormLocations.length];
                            for (int i = 0; i < bufferedPCAreaStartBound.length; i++) {
                                /*
                                 * Explanation of distance calculation here: we want to include cities before the delta distance 
                                 * within half the interval distance. Since we already add a buffer to this geometry equal to 
                                 * the distance threshold later, we want to add the distance threshold again to get the correct 
                                 * distance for the starting boundary of the valid area of the warning
                                 */
                                 bufferedPCAreaStartBound[i] = computePoint(stormLocations[i], gc, stormTrackState.angle, 
                                        (deltaDistance - (intervalDistance / 2)) + thresholdInMeters);
                            }
                            Coordinate[] bufferedPCAreaCoords = new Coordinate[(stormLocations.length * 2) + 1]; 
                            int index = 0;
                            for (int i = 0; i < bufferedPCAreaStartBound.length; i++) {
                                bufferedPCAreaCoords[index] = bufferedPCAreaStartBound[i];
                                index++;
                            }
                            // need to reverse the order of this array so they are added in the proper order to bufferedPCAreaCoords
                            // to create a polygon (coordinates should form a linear ring)
                            Coordinate[] stormLocationsReversed = stormLocations.clone();
                            Collections.reverse(Arrays.asList(stormLocationsReversed));
                            for (int i = 0; i < stormLocationsReversed.length; i++) {
                                /*
                                 * Explanation of distance calculation here: we want to include cities on the pathcast that fall 
                                 * within the distance the storm is projected to travel within the warning duration, plus a 
                                 * buffer equal to half the interval distance.  Since we already add a buffer to this geometry 
                                 * equal to the distance threshold later, we want to subtract the distance threshold again to get 
                                 * the correct distance for the ending boundary of the valid area of the warning. 
                                 */
                                bufferedPCAreaCoords[index] = computePoint(stormLocationsReversed[i], gc, stormTrackState.angle, 
                                    (distanceOfExpiration + (intervalDistance / 2)) - thresholdInMeters);
                                index++;
                            }
                            bufferedPCAreaCoords[bufferedPCAreaCoords.length - 1] = bufferedPCAreaStartBound[0];
                            bufferedPathCastArea = gf.createPolygon(bufferedPCAreaCoords);
                            Coordinate c = bufferedPathCastArea.getCentroid().getCoordinate();
                            crs = MapUtil.constructStereographic(
                                    MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                                    c.y, c.x);
                            latLonToLocal = MapUtil.getTransformFromLatLon(crs);
                            bufferedPathCastArea = JTS.transform(bufferedPathCastArea, latLonToLocal);
                            bufferedPathCastArea = JTS.transform(
                                    bufferedPathCastArea.buffer(thresholdInMeters),
                                    latLonToLocal.inverse());
                        }

                        Coordinate[] losPCBound1 = new Coordinate[stormLocations.length];
                        for (int i = 0; i < losPCBound1.length; i++) {
                            losPCBound1[i] = computePoint(stormLocation.getCoordinates()[i], gc, stormTrackState.angle, distance);
                        }
                        Coordinate[] losPCBound2 = new Coordinate[stormLocations.length];
                        for (int i = 0; i < losPCBound2.length; i++) {
                            losPCBound2[i] = computePoint(losPCBound1[i], gc, stormTrackState.angle, intervalDistance);
                        }

                        // create pathcast area from the two coordinate sets
                        Coordinate[] losPCAreaCoords = new Coordinate[losPCBound1.length + losPCBound2.length + 1];
                        int index = 0;
                        for (Coordinate c : losPCBound1) {
                            losPCAreaCoords[index] = c;
                            index++;
                        }
                        // need to reverse the order of this  so they are added in the proper order to losPCAreaCoords
                        // to create a polygon (coordinates should form a linear ring)
                        Collections.reverse(Arrays.asList(losPCBound2));
                        for (Coordinate c : losPCBound2) {
                            losPCAreaCoords[index] = c;
                            index++;
                        }
                        losPCAreaCoords[index] = losPCBound1[0];
                        Geometry pcGeom = gf.createPolygon(losPCAreaCoords);

                        Coordinate c = pcGeom.getCentroid().getCoordinate();
                        crs = MapUtil.constructStereographic(
                                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                                c.y, c.x);
                        latLonToLocal = MapUtil.getTransformFromLatLon(crs);
                        pcGeom = JTS.transform(pcGeom, latLonToLocal);
                        pcGeom = JTS.transform(
                                pcGeom.buffer(thresholdInMeters),
                                latLonToLocal.inverse());

                        cast.pcGeom = pcGeom;
                    }
                    instant += intervalInMillis;
                    pcIndex++;
                }
            } else {
                PathCast singleTime = new PathCast();
                singleTime.time = new Date(wwaStartTime);
                pathCasts.add(singleTime);
                bufferedPathCastArea = warningPolygon;
            }

            if (pathcastConfiguration.isWithinPolygon()) {
                // Means that all points returned must be within the polygon
                bufferedPathCastArea = warningPolygon.intersection(bufferedPathCastArea);
            }

            Set<String> ptFields = new HashSet<>();
            ptFields.add(pointField);
            for (String field : fields) {
                if (!"distance".equalsIgnoreCase(field)
                        && !"area".equalsIgnoreCase(field)
                        && !"parentArea".equalsIgnoreCase(field)) {
                    ptFields.add(field.toLowerCase());
                }
            }

            AbstractDbSourceDataAdaptor pathcastDataAdaptor = null;
            if (pointSource != null) {
                pathcastDataAdaptor = DataAdaptorFactory
                        .createPathcastDataAdaptor(pathcastConfiguration,
                                distanceToMeters, bufferedPathCastArea,
                                localizedSite);
            }

            SpatialQueryResult[] areaFeatures = null;
            if (areaSource != null) {
                areaFeatures = SpatialQueryFactory.create().query(areaSource,
                        new String[] { areaField, parentAreaField,
                                areaNotationField },
                        bufferedPathCastArea, null, false,
                        SearchMode.INTERSECTS);
            }

            Map<PathCast, List<ClosestPoint>> pcPoints = new HashMap<>();
            for (PathCast pc : pathCasts) {
                pc.pcGeom = pc.getPcGeom().intersection(bufferedPathCastArea);
                Geometry pcGeom = pc.getPcGeom();

                Point centroid = pcGeom != null ? pcGeom.getCentroid()
                        : warningPolygon.getCentroid();

                SpatialQueryResult myArea = null;

                if (areaFeatures != null) {
                    // Find area and parent area
                    for (SpatialQueryResult areaRslt : areaFeatures) {
                        if (areaRslt.geometry.contains(centroid)) {
                            myArea = areaRslt;
                            break;
                        }
                    }
                }

                // Set area info
                if (myArea != null) {
                    pc.area = myArea.attributes.get(areaField).toString();
                    pc.parentArea = myArea.attributes.get(parentAreaField)
                            .toString();
                    if (areaTypeAbbrev != null) {
                        String tmp = myArea.attributes.get(areaNotationField)
                                .toString();
                        pc.areaNotation = areaTypeAbbrev.translate(tmp);
                    } else if (areaNotationField != null) {
                        pc.areaNotation = myArea.attributes
                                .get(areaNotationField).toString();
                    }
                    pc.area.trim();
                }

                List<ClosestPoint> points = null;
                if (pathcastDataAdaptor != null) {
                    points = pathcastDataAdaptor.getPathcastData(
                            pathcastConfiguration, distanceToMeters,
                            latLonToLocal, areaFeatures, pc);
                } else {
                    points = new ArrayList<>(0);
                }
                pcPoints.put(pc, points);
            }

            // Figure out which points should go with which pathcast. Starts
            // with first pathcast and goes through each point within maxCount,
            // check for same point in other pathcast objects. If same point
            // exists, remove from which ever pathcast is later
            Set<Coordinate> closestPtCoords = new HashSet<>(30);
            List<ClosestPoint> tmpPoints = new ArrayList<>(maxCount);
            Queue<PathCast> tmp = new ArrayDeque<>(pathCasts);
            while (!tmp.isEmpty()) {
                PathCast pc = tmp.remove();
                List<ClosestPoint> points = pcPoints.get(pc);
                for (int i = 0; i < points.size(); ++i) {
                    ClosestPoint cp = points.get(i);
                    for (PathCast pc2 : tmp) {
                        if (pc2 != pc) {
                            List<ClosestPoint> points2 = pcPoints.get(pc2);
                            ClosestPoint found = find(cp, points2,
                            Integer.MAX_VALUE);
                            if (found != null) {
                                // We found a point within maxCount in this list.
                                if (pc.index > pc2.index) {
                                    // This point is earlier in the other pathcast
                                    points.remove(i);
                                    --i;
                                    break;
                                } else {
                                    // Remove from other pathcast, we are earlier
                                    points2.remove(found);
                                }
                            }
                        }
                    }
                }

                tmpPoints.clear();
                for (int i = 0; i < points.size() && i < maxCount; ++i) {
                    ClosestPoint point = points.get(i);
                    Coordinate coord = point.getPoint();
                    if (!closestPtCoords.contains(coord)) {
                        // To prevent duplicate cities in pathcast,
                        // only unused point is added to tmpPoints
                        tmpPoints.add(point);
                        closestPtCoords.add(coord);
                    }
                }
                if (!tmpPoints.isEmpty()) {
                    pc.points = tmpPoints
                            .toArray(new ClosestPoint[tmpPoints.size()]);
                } else {
                    pathCasts.remove(pc);
                }

                /*
                 * time zones are limited, use data for whole CWA and further
                 * intersection later
                 */
                GeospatialData[] timeZones = GeospatialFactory.getTimezones();

                /*
                 * loop through ClosestPoints on the pathcast and assign their
                 * time zones
                 */
                for (ClosestPoint point : tmpPoints) {
                    Coordinate coordinate = point.getPoint();
                    Geometry geomPoint = gf.createPoint(coordinate);
                    GeospatialData timezone = null;
                    if (timeZones != null) {
                        // Find time zone
                        if (timeZones.length == 1) {
                            timezone = timeZones[0];
                        } else {
                            for (GeospatialData tzResult : timeZones) {
                                if (tzResult.prepGeom.contains(geomPoint)) {
                                    timezone = tzResult;
                                    break;
                                }
                            }
                        }
                    }
                    if (timezone != null) {
                        String tz = String.valueOf(
                                timezone.attributes.get(timezoneField));
                        point.setTimeZone(tz);
                    }
                }
            }
            while (pathCasts.size() > maxGroup) {
                pathCasts.remove(pathCasts.size() - 1);
            }

            /*
             * Assign a time zone to all remaining PathCast objects using the
             * time zones of the ClosestPoint objects within it.
             */
            List<PathCast> finalizedPathCasts = new ArrayList<>();
            for (PathCast pc : pathCasts) {
                Map<String, List<ClosestPoint>> timeZoneToPoints = new HashMap<>();
                for (ClosestPoint tmpPoint : pc.points) {
                    String tz = tmpPoint.getTimeZone();
                    List<ClosestPoint> cps = timeZoneToPoints.get(tz);
                    if (cps == null) {
                        cps = new ArrayList<>();
                        timeZoneToPoints.put(tz, cps);
                    }
                    cps.add(tmpPoint);
                }
                for (Entry<String, List<ClosestPoint>> tzToPt : timeZoneToPoints
                        .entrySet()) {
                    PathCast newPc = new PathCast(pc);
                    newPc.timeZone = tzToPt.getKey();
                    newPc.points = tzToPt.getValue().toArray(
                            new ClosestPoint[tzToPt.getValue().size()]);
                    finalizedPathCasts.add(newPc);
                }
            }

            perfLog.logDuration("Get pathcast",
                    System.currentTimeMillis() - t0);
            return finalizedPathCasts
                    .toArray(new PathCast[finalizedPathCasts.size()]);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "WarnGen critical error: Pathcast Query Failed", e);
        }
        return null;
    }

    private ClosestPoint find(ClosestPoint searchFor,
            List<ClosestPoint> searchIn, int maxCount) {
        ClosestPoint found = null;
        for (int i = 0; i < searchIn.size() && i < maxCount; ++i) {
            ClosestPoint check = searchIn.get(i);
            if (searchFor.name.equals(check.name)
                    && searchFor.point.equals(check.point)) {
                found = check;
                break;
            }
        }
        return found;
    }

    public Map<String, Object> getClosestPoints(String localizedSite) {
        long t0 = System.currentTimeMillis();
        Map<String, Object> pointsMap = new HashMap<>();
        // TODO: Group by variable name, validate groups, get data for each
        // group
        Map<String, List<PointSourceConfiguration>> grouped = new HashMap<>();
        for (PointSourceConfiguration ptConfig : config.getPointSources()) {
            List<PointSourceConfiguration> group = grouped
                    .get(ptConfig.getVariable());
            if (group == null) {
                group = new ArrayList<>();
                grouped.put(ptConfig.getVariable(), group);
            }
            group.add(ptConfig);
        }

        List<Future<Pair<String, Object>>> futures = new ArrayList<>();
        for (Entry<String, List<PointSourceConfiguration>> entry : grouped
                .entrySet()) {
            String variable = entry.getKey();
            PointSourceConfiguration[] sources = entry.getValue()
                    .toArray(new PointSourceConfiguration[0]);
            try {
                // Validate SearchMethod all same:
                String message = null;
                SearchMethod method = sources[0].getSearchMethod();
                boolean isWithinPolygon = sources[0].isWithinPolygon();
                String[] sortBy = sources[0].getSortBy();
                for (int i = 1; i < sources.length && message == null; ++i) {
                    PointSourceConfiguration check = sources[i];
                    if (check.getSearchMethod() != method) {
                        message = "SearchMethods do not match";
                    } else if (check.isWithinPolygon() != isWithinPolygon) {
                        message = "isWithinPolygon flags do not match";
                    } else if (!Arrays.equals(sortBy, check.getSortBy())) {
                        message = "sortBy fields do not match";
                    }
                }

                if (message == null) {
                    // No inconsistencies, get points for sources
                    Future<Pair<String, Object>> future = executor
                            .submit(() -> {
                                return new Pair<>(variable, getClosestPoints(
                                        localizedSite, sources));
                            });
                    futures.add(future);
                } else {
                    statusHandler.handle(Priority.PROBLEM,
                            "Skipping sources for variable '" + variable
                                    + "' because " + message);
                }
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving closest points for variable '"
                                + variable + "'",
                        t);
            }
        }

        for (Future<Pair<String, Object>> future : futures) {
            try {
                Pair<String, Object> variableAndPoints = future.get();
                pointsMap.put(variableAndPoints.getFirst(),
                        variableAndPoints.getSecond());
            } catch (InterruptedException | ExecutionException e) {
                statusHandler.error("Error retrieving closest points", e);
            }
        }

        perfLog.logDuration("Get closestPoints",
                System.currentTimeMillis() - t0);
        return pointsMap;
    }

    /**
     * Get the closest points from the point configurations passed in merging
     * results from the multiple sources. Validation should occur before using
     * this method. pointConfigs must be > 0, each point configuration passed in
     * should have the same SearchMethod, isWithinPolygon, and sort by fields
     * should all be the same for each source
     *
     * @param localizedSite
     * @param pointConfigs
     * @return
     */
    private Object getClosestPoints(String localizedSite,
            PointSourceConfiguration... pointConfigs) throws Exception {
        UnitConverter distanceToMeters = config.getUnitDistance()
                .getConverterTo(SI.METRE);
        UnitConverter metersToDistance = distanceToMeters.inverse();

        // Get the maximum distance threshold
        double maxThreshold = 0.0;
        // Get max result count
        int maxCount = 0;
        for (PointSourceConfiguration pointConfig : pointConfigs) {
            double threshold = pointConfig.getDistanceThreshold();
            if (maxThreshold < threshold) {
                maxThreshold = threshold;
            }

            int count = pointConfig.getMaxResults();
            if (maxCount < count) {
                maxCount = count;
            }
        }

        // Convert max distance threshold to meters
        double thresholdInMeters = distanceToMeters.convert(maxThreshold);

        // Get search method, should already have been validated to be same
        // amoung sources
        SearchMethod methodOfSearch = pointConfigs[0].getSearchMethod();

        // Geometry factory for creating JTS geometries
        GeometryFactory gf = new GeometryFactory();

        // All configs should have the same "isWithinPolygon" flag
        boolean isWithinPolygon = pointConfigs[0].isWithinPolygon();
        boolean allowDuplicates = pointConfigs[0].isAllowDuplicates();

        // Sort by fields should have been validated to be same as well
        List<String> fields = pointConfigs[0].getSortBy() != null
                ? Arrays.asList(pointConfigs[0].getSortBy())
                : new ArrayList<>(0);

        Geometry searchArea = null;
        double bufferVal = thresholdInMeters;
        int dimensions = 1;
        if (!config.isTrackEnabled() || isWithinPolygon) {
            searchArea = warningPolygon;
            bufferVal = 0;
        } else {
            if (methodOfSearch == SearchMethod.POINTS) {
                searchArea = stormLocation;
                if (searchArea.getNumPoints() > 1) {
                    dimensions = 2;
                }
            } else {
                // Create polygon that is all coordinates from wwaStartTime
                // to wwaStopTime
                Coordinate[] stormCoords = stormLocation.getCoordinates();
                Coordinate[] endStormCoords = new Coordinate[stormCoords.length];
                List<Coordinate> allCoords = new ArrayList<>(
                        stormCoords.length + endStormCoords.length);
                allCoords.addAll(Arrays.asList(stormCoords));
                long time = (wwaStopTime - wwaStartTime) / 1000;
                DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();
                for (int i = stormCoords.length - 1; i >= 0; --i) {
                    Coordinate loc = stormCoords[i];
                    gc.setStartingGeographicPoint(loc.x, loc.y);
                    double distance = stormTrackState.speed * time;
                    gc.setDirection(StormTrackDisplay
                            .adjustAngle(stormTrackState.angle), distance);
                    Point2D p = gc.getDestinationGeographicPoint();
                    endStormCoords[stormCoords.length - i - 1] = new Coordinate(
                            p.getX(), p.getY());
                }
                allCoords.addAll(Arrays.asList(endStormCoords));
                if (allCoords.size() == 2) {
                    allCoords.add(new Coordinate(
                            allCoords.get(allCoords.size() - 1)));
                }
                allCoords.add(new Coordinate(allCoords.get(0)));

                searchArea = gf.createPolygon(
                        gf.createLinearRing(allCoords
                                .toArray(new Coordinate[allCoords.size()])),
                        null);
            }
        }

        Coordinate c = searchArea.getCentroid().getCoordinate();
        c = GisUtil.restoreAlaskaLon(c);
        CoordinateReferenceSystem crs = MapUtil.constructStereographic(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, c.y,
                c.x);
        MathTransform latLonToLocal = MapUtil.getTransformFromLatLon(crs);
        Geometry bufferedSearchArea = JTS.transform(
                JTS.transform(searchArea, latLonToLocal).buffer(bufferVal),
                latLonToLocal.inverse());

        List<ClosestPoint> availablePoints = new ArrayList<>();
        for (PointSourceConfiguration pointConfig : pointConfigs) {
            long t0 = System.currentTimeMillis();
            AbstractDbSourceDataAdaptor adaptor = DataAdaptorFactory
                    .createDataAdaptor(pointConfig, bufferedSearchArea,
                            localizedSite);
            if (adaptor != null) {
                availablePoints.addAll(
                        adaptor.getData(config, pointConfig, localizedSite));
            }
            long t1 = System.currentTimeMillis();
            perfLog.logDuration("getClosestPoints.dbQuery for point source "
                    + pointConfig.getPointSource(), t1 - t0);
        }

        // Convert searchArea to a local projection
        Geometry localSearchArea = JTS.transform(searchArea, latLonToLocal);

        Coordinate[] localCoords = localSearchArea.getCoordinates();
        Coordinate[] coords = searchArea.getCoordinates();
        List<List<ClosestPoint>> points = new ArrayList<>(coords.length);
        GeodeticCalculator gc = new GeodeticCalculator();
        Map<String, ClosestPoint> nameMap = new HashMap<>(
                (int) (availablePoints.size() * 1.3));
        List<ClosestPoint> pointsWithinDistance = new ArrayList<>();
        for (int i = 0; i < coords.length; ++i) {
            Coordinate coord = localCoords[i];
            Geometry localDistanceGeom = dimensions == 1 ? localSearchArea
                    : gf.createPoint(coord);
            Geometry distanceGeom = dimensions == 1 ? searchArea
                    : gf.createPoint(coords[i]);
            pointsWithinDistance.clear();
            nameMap.clear();

            for (ClosestPoint cp : availablePoints) {
                Geometry localPt = JTS.transform(gf.createPoint(cp.point),
                        latLonToLocal);

                double distance = localDistanceGeom.distance(localPt);
                // Tests if storm location is over an urban bound area even if
                // it may be outside the warning polygon
                if (cp.prepGeom != null && config.isTrackEnabled()
                        && !isWithinPolygon) {
                    // When isWithinPolygon is true, partOfArea
                    // has already been set in DbAreaSoureDataAdapter
                    Point reference = gf.createPoint(coords[i]);
                    if (cp.prepGeom.intersects(reference)) {
                        cp.partOfArea = GisUtil
                                .asStringList(GisUtil.calculateLocationPortion(
                                        cp.prepGeom.getGeometry(), reference,
                                        false, true));
                        distance = 0;
                    }
                }
                if (distance <= thresholdInMeters) {
                    if (allowDuplicates) {
                        // collect all points that are within the threshold
                        ClosestPoint cp2 = createClosestPoint(cp, distance,
                                metersToDistance, distanceGeom, gc);
                        pointsWithinDistance.add(cp2);
                    } else {
                        // check map of currently added points for closer point
                        // with the same name
                        ClosestPoint existingPt = nameMap.get(cp.name);
                        if (existingPt == null
                                || distance < existingPt.distance) {
                            ClosestPoint cp2 = createClosestPoint(cp, distance,
                                    metersToDistance, distanceGeom, gc);
                            nameMap.put(cp2.name, cp2);
                        }
                    }
                }
            }

            List<ClosestPoint> pts = null;
            if (allowDuplicates) {
                pts = new ArrayList<>(pointsWithinDistance);
            } else {
                pts = new ArrayList<>(nameMap.values());
            }
            if (!fields.isEmpty()) {
                // Sort the points based on sortBy fields
                Collections.sort(pts, new ClosestPointComparator(fields));
            }
            points.add(pts);
            // If we are one dimensional, no need to continue
            if (dimensions == 1) {
                break;
            }
        }

        // Filter to maxCount (Somewhat duplicate logic as pathcast)
        if (points.size() == 1) {
            // optimized for single instance
            List<ClosestPoint> pts = points.get(0);
            if (pts.size() > maxCount) {
                // need to reduce points
                pts.subList(maxCount, pts.size()).clear();
            }
        } else if (points.size() > 1) {
            Queue<List<ClosestPoint>> tmp = new ArrayDeque<>(points);
            while (!tmp.isEmpty()) {
                List<ClosestPoint> pts = tmp.remove();
                int maxIndex = Math.min(pts.size(), maxCount);
                for (int i = 0; i < maxIndex; ++i) {
                    // For each point, look for duplicate points in another
                    ClosestPoint cp = pts.get(i);
                    for (List<ClosestPoint> pts2 : tmp) {
                        if (pts2 != pts) {
                            ClosestPoint found = find(cp, pts2, maxCount);
                            if (found != null) {
                                // We found a point within maxCount in this
                                // list.
                                if (found.distance < cp.distance) {
                                    // This point is closer to the other
                                    pts.remove(i);
                                    --i;
                                    // changed size of pts, may need to change
                                    // maxIndex
                                    if (pts.size() < maxIndex) {
                                        maxIndex--;
                                    }
                                    break;
                                } else {
                                    // Remove from other pathcast, we are
                                    // closer
                                    pts2.remove(found);
                                }
                            }
                        }
                    }
                }

                if (pts.size() > maxIndex) {
                    // need to reduce points
                    pts.subList(maxIndex, pts.size()).clear();
                }
            }
        }
        if (points.size() == 1) {
            List<ClosestPoint> rval = points.get(0);
            return rval.toArray(new ClosestPoint[rval.size()]);
        } else if (points.size() > 1) {
            ClosestPoint[][] rvals = new ClosestPoint[points.size()][];
            for (int i = 0; i < points.size(); ++i) {
                List<ClosestPoint> rval = points.get(i);
                rvals[i] = rval.toArray(new ClosestPoint[rval.size()]);
            }
            return rvals;
        }

        return null;
    }

    /**
     * Helper method to create a ClosestPoint object.
     *
     * @param cp
     * @param distance
     *            between the cp to the
     * @param metersToDistance
     *            Unit converter to calculate the rounded distance.
     * @param distanceGeom
     *            Geometry search area.
     * @param gc
     *            Geodetic Calculator to determine the azimuth
     * @return ClosestPoint object set with roundedDistance, azimuth, etc.
     */
    private ClosestPoint createClosestPoint(ClosestPoint cp, double distance,
            UnitConverter metersToDistance, Geometry distanceGeom,
            GeodeticCalculator gc) {
        // Set the distances
        ClosestPoint cp2 = new ClosestPoint(cp);
        cp2.distance = distance;
        cp2.roundedDistance = (int) metersToDistance.convert(distance);
        gc.setStartingGeographicPoint(cp2.point.x, cp2.point.y);
        Coordinate cen = GisUtil
                .restoreAlaskaLon(distanceGeom.getCentroid().getCoordinate());
        gc.setDestinationGeographicPoint(cen.x, cen.y);
        cp2.azimuth = gc.getAzimuth();
        cp2.oppositeAzimuth = AdjustAngle.to360Degrees(cp2.azimuth + 180);
        cp2.roundedAzimuth = GeoUtil.roundAzimuth(cp2.azimuth);
        cp2.oppositeRoundedAzimuth = AdjustAngle
                .to360Degrees(cp2.roundedAzimuth + 180);

        return cp2;
    }

    public double getMovementSpeed() {
        return getMovementSpeed(null);
    }

    public double getMovementSpeed(String unit) {
        Unit<Speed> convertTo = config.getUnitSpeed();
        if (unit != null) {
            try {
                convertTo = SimpleUnitFormat
                        .getInstance(SimpleUnitFormat.Flavor.ASCII)
                        .parseProductUnit(unit, new ParsePosition(0))
                        .asType(Speed.class);
            } catch (MeasurementParseException e) {
                statusHandler.handle(Priority.DEBUG,
                        "Unable to parse movement speed unit: " + unit, e);
            }
        }
        UnitConverter converter = SI.METRE_PER_SECOND.getConverterTo(convertTo);
        return converter.convert(stormTrackState.speed);
    }

    /**
     * @return movement direction rounded
     */
    public double getMovementDirectionRounded() {
        return GeoUtil.roundAzimuth(stormTrackState.angle);
    }

    /**
     * @return movement direction
     */
    public double getMovementDirection() {
        double motion = stormTrackState.angle;
        if (motion < 0) {
            motion += 360;
        }

        return motion;
    }

    /**
     * @return the end time
     */
    public Date getEndTime() {
        return new Date(this.wwaStopTime);
    }

    /**
     * @return the start time
     */
    public Date getStartTime() {
        return new Date(this.wwaStartTime);
    }

    /**
     * @return the obs time
     */
    public Date getObsTime() {
        return new Date(this.wwaStartTime);
    }
}

