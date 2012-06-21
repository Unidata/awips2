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
import java.text.ParseException;
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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.apache.commons.lang.Validate;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.config.GeospatialConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PathcastConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration.SearchMethod;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.util.FileUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.DestinationGeodeticCalculator;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackDisplay;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.warngen.PreferenceUtil;
import com.raytheon.viz.warngen.WarngenException;
import com.raytheon.viz.warngen.config.DataAdaptorFactory;
import com.raytheon.viz.warngen.util.Abbreviation;
import com.raytheon.viz.warnings.DateUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

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
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class Wx {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Wx.class);

    private static final String transformedKey = "com.raytheon.transformed";

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
                .getConverterTo(SI.METER);
        UnitConverter metersToDistance = distanceToMeters.inverse();

        int maxCount = pathcastConfiguration.getMaxResults();
        int maxGroup = pathcastConfiguration.getMaxGroup();
        double thresholdInMeters = distanceToMeters
                .convert(pathcastConfiguration.getDistanceThreshold());
        String areaField = pathcastConfiguration.getAreaField();
        String areaSource = geospatialConfig.getAreaSource();
        String parentAreaField = pathcastConfiguration.getParentAreaField();
        String areaNotationField = pathcastConfiguration.getAreaNotationField();
        String areaNotationAbbrevField = pathcastConfiguration
                .getAreaNotationTranslationFile();
        String timezoneTable = geospatialConfig.getTimezoneSource();
        String timezoneField = geospatialConfig.getTimezoneField();
        String pointSource = pathcastConfiguration.getPointSource();
        String pointField = pathcastConfiguration.getPointField().toLowerCase();
        Map<String, RequestConstraint> pointFilter = pathcastConfiguration
                .getFilter();
        if (pointFilter != null) {
            // Process substitutes for filter
            for (RequestConstraint rc : pointFilter.values()) {
                rc.setConstraintValue(PreferenceUtil.substitute(
                        rc.getConstraintValue(), localizedSite));
            }
        }
        int delta = pathcastConfiguration.getDelta() * 60 * 1000;
        int intervalInMillis = pathcastConfiguration.getInterval() * 60 * 1000;

        List<String> fields = pathcastConfiguration.getSortBy() != null ? Arrays
                .asList(pathcastConfiguration.getSortBy())
                : new ArrayList<String>();

        Validate.isTrue(
                maxCount > 0,
                "Max count must be greater than zero. Check .xml if maxCount is set in pathcastConfig.\n");
        Validate.isTrue(
                maxGroup > 0,
                "Max group must be greater than zero. Check .xml if maxGroup is set in pathcastConfig.\n");
        Validate.isTrue(
                thresholdInMeters > 0,
                "Near threshold must be greater than zero. Check .xml if nearThreshold is set in pathcastConfig.\n");
        Validate.notNull(
                areaField,
                "An area field must be provided. Check .xml if areaField is set in pathcastConfig.\n");
        Validate.notNull(
                pointField,
                "A point field must be provided. Check .xml if pointField is set in pathcastConfig.\n");
        Validate.isTrue(
                (!(areaNotationAbbrevField != null && areaNotationField == null)),
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
                File trxFile = FileUtil.getFile(areaNotationAbbrevField,
                        localizedSite);
                if (!trxFile.exists()) {
                    throw new WarngenException(
                            "Translation file does not exist: " + trxFileStr);
                }
                areaTypeAbbrev = new Abbreviation(trxFile);
            }

            MathTransform latLonToLocal = null;
            CoordinateReferenceSystem crs = null;
            Geometry bufferedPathCastArea = null;
            List<PathCast> pathcasts = new ArrayList<PathCast>();
            Map<PathCast, Coordinate[]> pathCastCoords = new HashMap<PathCast, Coordinate[]>();
            if (stormTrackState.trackVisible) {
                List<Coordinate> coordinates = new ArrayList<Coordinate>();
                Date start = DateUtil.roundDate(new Date(wwaStartTime + delta),
                        pathcastConfiguration.getInterval());
                Date stormTime = new Date(wwaStartTime);
                while (start.getTime() <= wwaStopTime) {
                    PathCast cast = new PathCast();
                    cast.time = new Date(start.getTime());
                    start = new Date(start.getTime() + intervalInMillis);
                    pathcasts.add(cast);

                    // Compute where stormLocations would be given the
                    // speed/angle of the state
                    Coordinate[] stormLocations = stormLocation
                            .getCoordinates();
                    Coordinate[] pathCoords = new Coordinate[stormLocations.length];
                    for (int i = 0; i < pathCoords.length; ++i) {
                        Coordinate loc = stormLocations[i];
                        DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();
                        gc.setStartingGeographicPoint(loc.x, loc.y);
                        long time = (cast.time.getTime() - stormTime.getTime()) / 1000;
                        double distance = stormTrackState.speed * time;
                        gc.setDirection(StormTrackDisplay
                                .adjustAngle(stormTrackState.angle), distance);
                        Point2D p = gc.getDestinationGeographicPoint();
                        Coordinate c = new Coordinate(p.getX(), p.getY());
                        pathCoords[i] = c;
                        coordinates.add(new Coordinate(c));
                    }
                    pathCastCoords.put(cast, pathCoords);
                }

                if (coordinates.size() < 2) {
                    coordinates.add(new Coordinate(coordinates.get(0)));
                }

                // Take all the points in the path cast for each interval and
                // transform the points into stereographic over the centroid.
                // Then we will buffer the geometry to the distance threshold
                // and convert back into latLon for our bufferedGeom
                Geometry geom = gf.createLineString(coordinates
                        .toArray(new Coordinate[coordinates.size()]));
                Coordinate c = geom.getCentroid().getCoordinate();
                crs = MapUtil.constructStereographic(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        c.y, c.x);
                try {
                    latLonToLocal = MapUtil.getTransformFromLatLon(crs);
                    geom = JTS.transform(geom, latLonToLocal);
                    geom = JTS.transform(
                            geom.convexHull().buffer(thresholdInMeters),
                            latLonToLocal.inverse());
                } catch (FactoryException e) {
                    throw new VizException(
                            "Error creating polygon, could not get math tranform",
                            e);
                }

                if (pathcastConfiguration.isWithinPolygon()) {
                    // Means that all points returned must be within the polygon
                    bufferedPathCastArea = warningPolygon;
                } else {
                    bufferedPathCastArea = geom;
                }
            } else {
                PathCast singleTime = new PathCast();
                singleTime.time = new Date(wwaStartTime);
                pathcasts.add(singleTime);
                bufferedPathCastArea = warningPolygon;
            }

            Set<String> ptFields = new HashSet<String>();
            ptFields.add(pointField);
            for (String field : fields) {
                if (!field.equalsIgnoreCase("distance")
                        && !field.equalsIgnoreCase("area")
                        && !field.equalsIgnoreCase("parentArea")) {
                    ptFields.add(field.toLowerCase());
                }
            }

            SpatialQueryResult[] ptFeatures = null;
            if (pointSource != null) {
                ptFeatures = SpatialQueryFactory.create().query(pointSource,
                        ptFields.toArray(new String[ptFields.size()]),
                        bufferedPathCastArea, pointFilter,
                        SearchMode.INTERSECTS);
                if (latLonToLocal != null) {
                    for (SpatialQueryResult rslt : ptFeatures) {
                        rslt.attributes.put(transformedKey,
                                JTS.transform(rslt.geometry, latLonToLocal));
                    }
                }
            }

            SpatialQueryResult[] areaFeatures = null;
            if (areaSource != null) {
                areaFeatures = SpatialQueryFactory.create().query(
                        areaSource,
                        new String[] { areaField, parentAreaField,
                                areaNotationField }, bufferedPathCastArea,
                        null, false, SearchMode.INTERSECTS);
            }

            SpatialQueryResult[] timeZoneFeatures = SpatialQueryFactory
                    .create().query(timezoneTable,
                            new String[] { timezoneField },
                            bufferedPathCastArea, null, false,
                            SearchMode.INTERSECTS);

            Map<PathCast, List<ClosestPoint>> pcPoints = new HashMap<PathCast, List<ClosestPoint>>();
            for (PathCast pc : pathcasts) {
                Geometry pcGeom = null;
                Coordinate[] coords = pathCastCoords.get(pc);
                if (coords != null) {
                    if (coords.length == 1) {
                        pcGeom = gf.createPoint(coords[0]);
                    } else {
                        pcGeom = gf.createLineString(coords);
                    }
                }
                Point centroid = pcGeom != null ? pcGeom.getCentroid()
                        : warningPolygon.getCentroid();

                SpatialQueryResult myArea = null, myTz = null;

                if (areaFeatures != null) {
                    // Find area and parent area
                    for (SpatialQueryResult areaRslt : areaFeatures) {
                        if (areaRslt.geometry.contains(centroid)) {
                            myArea = areaRslt;
                            break;
                        }
                    }
                }

                if (timeZoneFeatures != null) {
                    // Find time zone
                    if (timeZoneFeatures.length == 1) {
                        myTz = timeZoneFeatures[0];
                    } else {
                        for (SpatialQueryResult tzResult : timeZoneFeatures) {
                            if (tzResult.geometry.contains(centroid)) {
                                myTz = tzResult;
                                break;
                            }
                        }
                    }
                }

                // Set area info and time zone info
                if (myArea != null) {
                    pc.area = myArea.attributes.get(areaField).toString();
                    pc.parentArea = myArea.attributes.get(parentAreaField)
                            .toString();
                    if (areaTypeAbbrev != null) {
                        String tmp = myArea.attributes.get(areaNotationField)
                                .toString();
                        pc.areaNotation = areaTypeAbbrev.translate(tmp);
                    } else if (areaNotationField != null) {
                        pc.areaNotation = myArea.attributes.get(
                                areaNotationField).toString();
                    }
                    pc.area.trim();
                }
                if (myTz != null) {
                    pc.timeZone = String.valueOf(myTz.attributes
                            .get(timezoneField));
                }

                Geometry localPCGeom = null;
                if (pcGeom != null) {
                    localPCGeom = JTS.transform(pcGeom, latLonToLocal);
                }

                // Find closest points
                List<ClosestPoint> points = new ArrayList<ClosestPoint>();
                for (SpatialQueryResult pointRslt : ptFeatures) {
                    Geometry localPt = (Geometry) pointRslt.attributes
                            .get(transformedKey);
                    double minDist = Double.MAX_VALUE;
                    Coordinate closestCoord = null;
                    if (localPCGeom != null) {
                        Coordinate[] localPts = localPCGeom.getCoordinates();
                        Coordinate[] latLonPts = pcGeom.getCoordinates();
                        for (int i = 0; i < localPts.length; ++i) {
                            Coordinate loc = localPts[i];
                            double distance = loc.distance(localPt
                                    .getCoordinate());
                            if (distance <= thresholdInMeters
                                    && distance < minDist) {
                                minDist = distance;
                                closestCoord = latLonPts[i];
                            }
                        }
                    } else {
                        closestCoord = centroid.getCoordinate();
                        minDist = 0;
                    }

                    if (closestCoord != null) {
                        ClosestPoint cp = new ClosestPoint();
                        cp.point = pointRslt.geometry.getCoordinate();
                        cp.name = String.valueOf(pointRslt.attributes
                                .get(pointField));
                        cp.distance = minDist;
                        cp.roundedDistance = (int) metersToDistance
                                .convert(minDist);
                        GeodeticCalculator gc = new GeodeticCalculator();
                        gc.setStartingGeographicPoint(cp.point.x, cp.point.y);
                        gc.setDestinationGeographicPoint(closestCoord.x,
                                closestCoord.y);
                        cp.azimuth = gc.getAzimuth();
                        cp.oppositeAzimuth = ClosestPoint
                                .adjustAngle(cp.azimuth + 180);
                        cp.roundedAzimuth = GeoUtil.roundAzimuth(cp.azimuth);
                        cp.oppositeRoundedAzimuth = ClosestPoint
                                .adjustAngle(cp.roundedAzimuth + 180);

                        boolean found = false;
                        for (SpatialQueryResult areaRslt : areaFeatures) {
                            if (areaRslt.geometry.contains(pointRslt.geometry)) {
                                cp.area = String.valueOf(areaRslt.attributes
                                        .get(areaField));
                                cp.parentArea = String
                                        .valueOf(areaRslt.attributes
                                                .get(parentAreaField));
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            cp.area = pc.area;
                            cp.parentArea = pc.parentArea;
                        }

                        if (ptFields.contains("population")) {
                            try {
                                cp.population = Integer.valueOf(String
                                        .valueOf(pointRslt.attributes
                                                .get("population")));
                            } catch (Exception e) {
                                cp.population = 0;
                            }
                        }
                        if (ptFields.contains("warngenlev")) {
                            try {
                                cp.warngenlev = Integer.valueOf(String
                                        .valueOf(pointRslt.attributes
                                                .get("warngenlev")));
                            } catch (Exception e) {
                                cp.warngenlev = 3;
                            }
                        }
                        points.add(cp);
                    }
                }

                if (fields.isEmpty() == false) {
                    // Sort the points based on sortBy fields
                    Collections
                            .sort(points, new ClosestPointComparator(fields));
                }
                pcPoints.put(pc, points);
            }
            // Figure out which points should go with which pathcast. Starts
            // with first pathcast and goes through each point within maxCount,
            // check for same point in other pathcast objects. If same point
            // exists, remove from which ever pathcast is furthest away
            Queue<PathCast> tmp = new ArrayDeque<PathCast>(pathcasts);
            while (tmp.isEmpty() == false) {
                PathCast pc = tmp.remove();
                List<ClosestPoint> points = pcPoints.get(pc);
                for (int i = 0; i < points.size() && i < maxCount; ++i) {
                    ClosestPoint cp = points.get(i);
                    for (PathCast pc2 : tmp) {
                        if (pc2 != pc) {
                            List<ClosestPoint> points2 = pcPoints.get(pc2);
                            ClosestPoint found = find(cp, points2, maxCount);
                            if (found != null) {
                                // We found a point within maxCount in this
                                // list.
                                if (found.distance < cp.distance) {
                                    // This point is closer to the other
                                    // pathcast
                                    points.remove(i--);
                                    break;
                                } else {
                                    // Remove from other pathcast, we are closer
                                    points2.remove(found);
                                }
                            }
                        }
                    }
                }

                List<ClosestPoint> tmpPoints = new ArrayList<ClosestPoint>(
                        maxCount);
                for (int i = 0; i < points.size() && i < maxCount; ++i) {
                    tmpPoints.add(points.get(i));
                }
                if (tmpPoints.size() > 0) {
                    pc.points = tmpPoints.toArray(new ClosestPoint[tmpPoints
                            .size()]);
                } else {
                    pathcasts.remove(pc);
                }
            }

            while (pathcasts.size() > maxGroup) {
                pathcasts.remove(pathcasts.size() - 1);
            }

            System.out.println("Time to get pathcast = "
                    + (System.currentTimeMillis() - t0) + "ms");
            return pathcasts.toArray(new PathCast[pathcasts.size()]);
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

    public Map<String, Object> getClosetsPoints(String localizedSite) {
        long t0 = System.currentTimeMillis();
        Map<String, Object> pointsMap = new HashMap<String, Object>();
        // TODO: Group by variable name, validate groups, get data for each
        // group
        Map<String, List<PointSourceConfiguration>> grouped = new HashMap<String, List<PointSourceConfiguration>>();
        for (PointSourceConfiguration ptConfig : config.getPointSources()) {
            List<PointSourceConfiguration> group = grouped.get(ptConfig
                    .getVariable());
            if (group == null) {
                group = new ArrayList<PointSourceConfiguration>();
                grouped.put(ptConfig.getVariable(), group);
            }
            group.add(ptConfig);
        }

        for (Entry<String, List<PointSourceConfiguration>> entry : grouped
                .entrySet()) {
            String variable = entry.getKey();
            PointSourceConfiguration[] sources = entry.getValue().toArray(
                    new PointSourceConfiguration[0]);
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
                    } else if (Arrays.equals(sortBy, check.getSortBy()) == false) {
                        message = "sortBy fields do not match";
                    }
                }

                if (message == null) {
                    // No inconsistencies, get points for sources
                    pointsMap.put(variable,
                            getClosestPoints(localizedSite, sources));
                } else {
                    statusHandler.handle(Priority.PROBLEM,
                            "Skipping sources for variable '" + variable
                                    + "' because " + message);
                }
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving closest points for variable '"
                                + variable + "'", t);
            }
        }
        System.out.println("Time to get closestPoints = "
                + (System.currentTimeMillis() - t0) + "ms");
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
                .getConverterTo(SI.METER);
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

        // Sort by fields should have been validated to be same as well
        List<String> fields = pointConfigs[0].getSortBy() != null ? Arrays
                .asList(pointConfigs[0].getSortBy()) : new ArrayList<String>();

        Geometry searchArea = null;
        double bufferVal = thresholdInMeters;
        int dimensions = 1;
        if (config.isTrackEnabled() == false || isWithinPolygon) {
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
                List<Coordinate> allCoords = new ArrayList<Coordinate>(
                        stormCoords.length + endStormCoords.length);
                allCoords.addAll(Arrays.asList(stormCoords));
                long time = (wwaStopTime - wwaStartTime) / 1000;
                for (int i = stormCoords.length - 1; i >= 0; --i) {
                    Coordinate loc = stormCoords[i];
                    DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();
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

                searchArea = gf.createPolygon(gf.createLinearRing(allCoords
                        .toArray(new Coordinate[allCoords.size()])), null);
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

        List<ClosestPoint> availablePoints = new ArrayList<ClosestPoint>();
        for (PointSourceConfiguration pointConfig : pointConfigs) {
            availablePoints.addAll(DataAdaptorFactory.createPointSource(
                    pointConfig).getData(config, pointConfig,
                    bufferedSearchArea, localizedSite));
        }

        // Convert searchArea to a local projection
        Geometry localSearchArea = JTS.transform(searchArea, latLonToLocal);

        List<List<ClosestPoint>> points = new ArrayList<List<ClosestPoint>>();
        Coordinate[] localCoords = localSearchArea.getCoordinates();
        Coordinate[] coords = searchArea.getCoordinates();
        for (int i = 0; i < coords.length; ++i) {
            Coordinate coord = localCoords[i];
            Geometry localDistanceGeom = dimensions == 1 ? localSearchArea : gf
                    .createPoint(coord);
            Geometry distanceGeom = dimensions == 1 ? searchArea : gf
                    .createPoint(coords[i]);
            List<ClosestPoint> pts = new ArrayList<ClosestPoint>();
            Map<String, ClosestPoint> nameMap = new HashMap<String, ClosestPoint>();

            for (ClosestPoint cp : availablePoints) {
                Geometry localPt = JTS.transform(gf.createPoint(cp.point),
                        latLonToLocal);

                double distance = localDistanceGeom.distance(localPt);
                if (distance <= thresholdInMeters) {
                    // check map of currently added points for closer point with
                    // the same name
                    ClosestPoint existingPt = nameMap.get(cp.name);
                    if (existingPt == null || distance < existingPt.distance) {
                        // Set the distances
                    	ClosestPoint cp2 = new ClosestPoint(cp);
                    	cp2.distance = distance;
                        cp2.roundedDistance = (int) metersToDistance
                                .convert(distance);
                        // No point by name or we are better at this location
                        if (existingPt != null) {
                            // There was an existing point, remove it
                            pts.remove(existingPt);
                        }
                        GeodeticCalculator gc = new GeodeticCalculator();
                        gc.setStartingGeographicPoint(cp2.point.x, cp2.point.y);
                        Coordinate cen = distanceGeom.getCentroid()
                                .getCoordinate();
                        cen = GisUtil.restoreAlaskaLon(cen);
                        gc.setDestinationGeographicPoint(cen.x, cen.y);
                        cp2.azimuth = gc.getAzimuth();
                        cp2.oppositeAzimuth = ClosestPoint
                                .adjustAngle(cp2.azimuth + 180);
                        cp2.roundedAzimuth = GeoUtil.roundAzimuth(cp2.azimuth);
                        cp2.oppositeRoundedAzimuth = ClosestPoint
                                .adjustAngle(cp2.roundedAzimuth + 180);
                        nameMap.put(cp2.name, cp2);
                        pts.add(cp2);
                    }
                }
            }

            if (fields.isEmpty() == false) {
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
        Queue<List<ClosestPoint>> tmp = new ArrayDeque<List<ClosestPoint>>(
                points);
        while (tmp.isEmpty() == false) {
            List<ClosestPoint> pts = tmp.remove();
            for (int i = 0; i < pts.size() && i < maxCount; ++i) {
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
                                pts.remove(i--);
                                break;
                            } else {
                                // Remove from other pathcast, we are closer
                                pts2.remove(found);
                            }
                        }
                    }
                }
            }

            List<ClosestPoint> tmpPoints = new ArrayList<ClosestPoint>(maxCount);
            for (int i = 0; i < pts.size() && i < maxCount; ++i) {
                tmpPoints.add(pts.get(i));
            }
            pts.clear();
            pts.addAll(tmpPoints);
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

    public double getMovementSpeed() {
        return getMovementSpeed(null);
    }

    public double getMovementSpeed(String unit) {
        Unit<?> convertTo = config.getUnitSpeed();
        if (unit != null) {
            try {
                convertTo = UnitFormat.getUCUMInstance().parseProductUnit(unit,
                        new ParsePosition(0));
            } catch (ParseException e) {

            }
        }
        UnitConverter converter = SI.METERS_PER_SECOND
                .getConverterTo(convertTo);
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
