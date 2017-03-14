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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.Validate;

import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration.AreaType;
import com.raytheon.uf.common.dataplugin.warning.config.GeospatialConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil.Direction;
import com.raytheon.uf.common.dataplugin.warning.portions.PortionsUtil;
import com.raytheon.uf.common.dataplugin.warning.util.CountyUserData;
import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.warngen.gui.WarngenLayer;
import com.raytheon.viz.warngen.util.Abbreviation;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.Polygonal;
import com.vividsolutions.jts.geom.TopologyException;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
//import com.raytheon.viz.warngen.gis.GisUtil.Direction;

/**
 * Area
 * 
 * Finds areas affected by area warnings
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 15, 2007 #601        chammack    Initial Creation.
 *    Mar 28, 2012 #14691      Qinglu lin  Created AffectedAreas' partOfParentRegion based on 
 *                                         FE_AREA stored in GeospatialData's attributes map, 
 *                                         instead of calculating them.
 *    Apr 11, 2012 #14691      Qinglu lin  Extra code were added to handle marine warnings as
 *                                         MarineZones shapefiles have no FE_AREA.
 *    Apr 13, 2012 #14691      Qinglu lin  Added code for two more fe_area: er and nr.
 *    May  4, 2012 #14887      Qinglu lin  Changed 0.25 to 0.60 for DEFAULT_PORTION_TOLERANCE; 
 *                                         added code to pass a Envelope calculatePortion().
 *    Nov  9, 2012 DR 15430    D. Friedman Extracted method converFeAreaToPartList.
 *    Apr 29, 2013  1955       jsanchez    Ignored comparing the geometry's user data when finding intersected areas.
 *    May  2, 2013  1963       jsanchez    Updated method to determine partOfArea.
 *    Aug 19, 2013  2177       jsanchez    Used portionsUtil to calculate area portion descriptions.
 *    Dec  4, 2013  2604       jsanchez    Refactored GisUtil and PortionsUtil.
 *    Apr 29, 2014  3033       jsanchez    Updated method to retrieve files in localization.
 *    May 16, 2014 DR 17365    D. Friedman Reduce precision of warning area to avoid topology errors.
 *    Jun 30, 2014 DR 17447    Qinglu lin  Updated findAffectedAreas().
 *    Jul 22, 2014  3419       jsanchez    Cleaned up converFeAreaToPartList.
 *    Sep 14, 2014 ASM #641    dhuffman    Filtered out cases where Areas do not match Zones by using
 *                                         refactored WarngenLayer::filterArea.
 *    Mar  9, 2014 ASM #17190  D. Friedman Use fipsField and areaField for unique area ID.
 *    May  7, 2015 ASM #17438  D. Friedman Clean up debug and performance logging.
 *    Dec 15, 2015 ASM #17933 mgamazaychikov Update calculation of partOfParentRegion.
 *    May 12, 2016 ASM #18789  D. Friedman Improve findInsectingAreas performance.
 *    Jul 21, 2016 DR 18159    Qinglu Lin  Updated findAffectedAreas() and findInsectingAreas(), added
 *                                         createFipsGeometryMap) and createKey().
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class Area {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Area.class);

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("WG:");

    /**
     * If an area greater than this percentage of the area is covered, no
     * direction is included
     */
    public static final double DEFAULT_PORTION_TOLERANCE = 0.60;

    private static final List<String> SPECIAL_CASE_FE_AREAS = Arrays
            .asList(new String[] { "PA", "MI", "PD", "UP", "BB", "ER", "EU",
                    "SR", "NR", "WU", "DS" });

    private Map<String, Geometry> fipsCountynameGeometry = new HashMap<>();

    private Set<String> countiesOfSplitArea = new HashSet<>();

    private static final int DEFAULT_SUBDIVISION_TRESHOLD = 100;

    private static final int SIMPLE_FEATURE_GEOM_COUNT_THRESHOLD = 2;

    private static final int MAX_SUBDIVISION_DEPTH = 24;

    private static final String SUBDIVISION_CONFIG_FILE = "subdiv.txt";

    private static ExecutorService intersectionExecutor;

    private PortionsUtil portionsUtil;

    public Area(PortionsUtil portionsUtil) {
        this.portionsUtil = portionsUtil;
    }

    public AffectedAreas[] findAffectedAreas(WarngenConfiguration config,
            Geometry polygon, Geometry warningArea, String localizedSite,
            WarngenLayer warngenLayer) throws VizException {

        // --- Begin argument checking ---
        Validate.notNull(config.getGeospatialConfig().getAreaSource(),
                "Area source must be provided for findAffectedAreas to operate");
        Validate.notNull(polygon, "Area geometry must not be null");

        // Get spatial query result for entries in our area from existing data;
        List<Geometry> geoms = new ArrayList<Geometry>();
        GeometryUtil.buildGeometryList(geoms, warningArea);

        GeospatialConfiguration geospatialConfig = config.getGeospatialConfig();
        AreaSourceConfiguration areaConfig = config.getHatchedAreaSource();

        if (fipsCountynameGeometry.size() == 0) {
            createFipsGeometryMap(config, localizedSite, warngenLayer);
        }

        return findAffectedAreas(areaConfig, geospatialConfig, polygon,
                localizedSite, geoms, warngenLayer);
    }

    private AffectedAreas[] findAffectedAreas(
            AreaSourceConfiguration areaConfig,
            GeospatialConfiguration geospatialConfig, Geometry polygon,
            String localizedSite, List<Geometry> geoms,
            WarngenLayer warngenLayer) throws VizException {
        String areaSource = areaConfig.getAreaSource();
        String areaField = areaConfig.getAreaField();
        String fipsField = areaConfig.getFipsField();
        String areaNotationField = areaConfig.getAreaNotationField();
        String pointField = areaConfig.getPointField();
        String pointSource = geospatialConfig.getPointSource();
        Map<String, RequestConstraint> pointFilter = areaConfig
                .getPointFilter();
        String parentAreaField = areaConfig.getParentAreaField();
        String timezonePathcastField = geospatialConfig.getTimezoneField();
        ArrayList<String> fields = new ArrayList<String>();
        /* fields is not used in querying to the database */
        if (areaConfig.getSortBy() != null) {
            for (String field : areaConfig.getSortBy()) {
                fields.add(field);
            }
        }

        Map<String, GeospatialData> countyMap = new HashMap<String, GeospatialData>();
        Map<String, Geometry> mergedIntersect = new HashMap<String, Geometry>();
        for (Geometry g : geoms) {
            CountyUserData data = (CountyUserData) g.getUserData();
            if (data != null) {
                String mapKey = createKey(data.entry.attributes, fipsField,
                        areaField);
                if (countyMap.containsKey(mapKey) == false) {
                    countyMap.put(mapKey, data.entry);
                }
                if (countiesOfSplitArea.contains(mapKey)) {
                    // for county with split areas, store merged areas in mergedIntersect.
                    if (! mergedIntersect.containsKey(mapKey)) {
                        mergedIntersect.put(mapKey, g);
                    } else {
                        Geometry g1 = mergedIntersect.get(mapKey);
                        CountyUserData cud = (CountyUserData) g1.getUserData();
                        cud.gid = GeometryUtil.getPrefix(cud);
                        Geometry g2 = GeometryUtil.union(g1, g);
                        g2.setUserData(cud);
                        mergedIntersect.put(mapKey, g2);
                    }
                }
            }
        }

        // Query for points within polygon
        SpatialQueryResult[] ptFeatures = null;
        long t0 = System.currentTimeMillis();
        if (pointField != null) {
            try {
                ptFeatures = SpatialQueryFactory.create().query(pointSource,
                        new String[] { pointField }, polygon, pointFilter,
                        SearchMode.INTERSECTS);
            } catch (SpatialException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        perfLog.logDuration("affected areas '" + areaConfig.getVariable()
                + "' spatial query", System.currentTimeMillis() - t0);

        Abbreviation abbreviation = null;

        if (areaConfig.getAreaNotationTranslationFile() != null) {
            try {
                abbreviation = new Abbreviation(WarnFileUtil
                        .findFileInLocalizationIncludingBackupSite(
                                areaConfig.getAreaNotationTranslationFile(),
                                localizedSite, null).getFile());
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.ERROR, "Unable to find "
                        + areaConfig.getAreaNotationTranslationFile() + "", e);
            }
        }

        List<String> uniqueAreaIDs = new ArrayList<String>();
        List<AffectedAreas> areas = new ArrayList<AffectedAreas>();
        long t0f = System.currentTimeMillis();
        for (GeospatialData regionFeature : countyMap.values()) {
            Geometry regionGeom = regionFeature.geometry;
            PreparedGeometry preparedRegionGeom = regionFeature.prepGeom;
            AffectedAreas area = new AffectedAreas();
            area.name = regionFeature.attributes.get(areaField).toString();
            area.fips = regionFeature.attributes.get(fipsField).toString();
            area.stateabbr = regionFeature.attributes.get(areaNotationField)
                    .toString();
            area.size = regionGeom.getArea();

            Object tzData = regionFeature.attributes.get(timezonePathcastField);

            if (tzData != null) {
                area.timezone = String.valueOf(tzData);
            } else {
                area.timezone = "P";
            }

            if (abbreviation != null) {
                area.areaNotation = abbreviation.translate(String
                        .valueOf(regionFeature.attributes
                                .get(areaNotationField)));
                area.areasNotation = abbreviation.translatePlural(String
                        .valueOf(regionFeature.attributes
                                .get(areaNotationField)));
            }
            Geometry intersection;
            String mapKey = createKey(regionFeature.attributes, fipsField,
                    areaField);
            if (countiesOfSplitArea.contains(mapKey)) {
                // replace regionGeom with that of county with split areas
                regionGeom = (Geometry) fipsCountynameGeometry.get(mapKey);
                // get intersection of county with split areas
                intersection = mergedIntersect.get(mapKey);
            } else {
                String gid = String.valueOf(regionFeature.attributes
                        .get(WarngenLayer.GID));
                List<Geometry> intersections = new ArrayList<Geometry>();
                for (Geometry g : geoms) {
                    if (GeometryUtil.getPrefix(g.getUserData())
                            .equalsIgnoreCase(gid)) {
                        intersections.add(g);
                    }
                }
                intersection = regionGeom.getFactory()
                        .createGeometryCollection(
                                intersections
                                .toArray(new Geometry[intersections
                                                      .size()]));
            }
            double areaIntersection = intersection.getArea();

            double tolerCheck = regionGeom.getArea()
                    * DEFAULT_PORTION_TOLERANCE;
            if (areaIntersection < tolerCheck) {
                try {
                    String entityID = area.stateabbr + areaSource.charAt(0)
                            + area.fips.substring(2);
                    area.partOfArea = GisUtil.asStringList(portionsUtil
                            .getPortions(entityID, regionGeom, intersection,
                                    true));
                } catch (Exception e) {
                    statusHandler.error("Unable to calculate part of area for "
                            + area.name, e);
                }
            }

            // Search the parent region
            GeospatialData parentRegion = regionFeature.parent;
            if (parentRegion != null) {
                area.parentRegion = String.valueOf(parentRegion.attributes
                        .get(parentAreaField));
                String feArea = (String) regionFeature.attributes
                        .get("FE_AREA");
                area.partOfParentRegion = Arrays.asList(feArea);
            }

            // Search against point matches
            if (ptFeatures != null) {
                List<String> pointList = new ArrayList<String>();
                for (SpatialQueryResult ptRslt : ptFeatures) {
                    if (preparedRegionGeom.contains(ptRslt.geometry)) {
                        pointList.add(String.valueOf(ptRslt.attributes
                                .get(pointField)));
                    }
                }

                area.points = pointList.toArray(new String[pointList.size()]);
            }
            /*
             * Usually, the fipsField value is a unique identifier, but in some
             * cases there are multiple areas that have the same value. These
             * areas have different names, so we make that part of the unique
             * ID.
             */
            String areaID = area.fips + ':' + area.name;
            if (uniqueAreaIDs.contains(areaID) == false) {
                uniqueAreaIDs.add(areaID);
                areas.add(area);
            }
        }
        perfLog.logDuration("affected areas '" + areaConfig.getVariable()
                + "' features", System.currentTimeMillis() - t0f);

        // Perform Sort
        if (fields.size() > 0) {
            AffectedAreasComparator comparator = new AffectedAreasComparator(
                    fields);
            Collections.sort(areas, comparator);
        }
        return areas.toArray(new AffectedAreas[areas.size()]);
    }

    /**
     * Determines the affected areas that intersect the warnArea. This method
     * should be used if the intersected areas are of a different area source
     * compared to the hatched area source. Otherwise, the information in the
     * warnArea can just be re-used in the template. If the area source of the
     * intersect and the hatched are the same, then the configuration and
     * template files are configured inefficiently.
     * 
     * @param config
     * @param warnPolygon
     * @param warnArea
     * @param localizedSite
     * @param warngenLayer
     * @return
     * @throws VizException
     */
    public Map<String, Object> findInsectingAreas(WarngenConfiguration config,
            Geometry warnPolygon, Geometry warnArea, String localizedSite,
            WarngenLayer warngenLayer) throws VizException {
        Map<String, Object> areasMap = new HashMap<String, Object>();
        try {
            Geometry precisionReducedArea = PolygonUtil
                    .reducePrecision(warnArea);
            if (precisionReducedArea.isValid()) {
                warnArea = precisionReducedArea;
            }
        } catch (Exception e) {
            // ignore
        }

        if (fipsCountynameGeometry.size() == 0) {
            createFipsGeometryMap(config, localizedSite, warngenLayer);
        }

        Map<String, Geometry> intersectMap = new HashMap<String, Geometry>();
        Map<String, Double> areaOfGeomMap = new HashMap<>();
        String hatchedAreaSource = config.getHatchedAreaSource()
                .getAreaSource();

        boolean subdivide = true;
        try {
            String propertiesText = WarnFileUtil.convertFileContentsToString(
                    SUBDIVISION_CONFIG_FILE, LocalizationManager.getInstance()
                            .getCurrentSite(), warngenLayer.getLocalizedSite());
            if (propertiesText != null) {
                Properties props = new Properties();
                props.load(new StringReader(propertiesText));
                subdivide = Boolean.parseBoolean(props.getProperty("enabled", "true"));
            }
        } catch (FileNotFoundException e) {
            // ignore
        } catch (IOException e) {
            statusHandler.handle(Priority.WARN, "Could not load subdivision configuration file", e);
        }
        if (!subdivide) {
            statusHandler.debug("findIntersectingAreas: subdivision is disabled");
        }

        long t0 = System.currentTimeMillis();
        for (AreaSourceConfiguration asc : config.getAreaSources()) {
            boolean ignoreUserData = asc.getAreaSource().equals(
                    hatchedAreaSource) == false;
            if (asc.getType() == AreaType.INTERSECT) {
                List<Geometry> geoms = new ArrayList<Geometry>();
                if (subdivide && ignoreUserData) {
                    synchronized (Area.class) {
                        if (intersectionExecutor == null) {
                            int nThreads = Math.max(2,
                                    Runtime.getRuntime().availableProcessors() / 2);
                            ThreadPoolExecutor executor = new ThreadPoolExecutor(
                                    nThreads, nThreads, 60, TimeUnit.SECONDS,
                                    new LinkedBlockingQueue<Runnable>());
                            executor.allowCoreThreadTimeOut(true);
                            intersectionExecutor = executor;
                        }
                    }
                    Geometry waPoly = toPolygonal(warnArea);
                    GeospatialData[] features = warngenLayer.getGeodataFeatures(
                            asc.getAreaSource(), localizedSite);
                    List<Callable<Geometry>> callables = new ArrayList<>(features.length);
                    for (GeospatialData f : features) {
                        callables.add(new FeatureIntersection(waPoly, f));
                    }
                    try {
                        List<Future<Geometry>> futures = intersectionExecutor.invokeAll(callables);
                        int fi = 0;
                        for (Future<Geometry> future: futures) {
                            Geometry intersect = future.get();
                            if (intersect != null && !intersect.isEmpty()
                                    && warngenLayer.filterArea(features[fi], intersect, asc)) {
                                geoms.add(intersect);
                            }
                            fi++;
                        }
                    } catch (ExecutionException | InterruptedException e) {
                        throw new VizException("Error finding intersecting areas", e);
                    }
                } else {
                    for (GeospatialData f : warngenLayer.getGeodataFeatures(
                            asc.getAreaSource(), localizedSite)) {
                        Geometry intersect = GeometryUtil.intersection(
                                warnArea, f.prepGeom, ignoreUserData);
                        if (!intersect.isEmpty()) {
                            CountyUserData data = (CountyUserData) f.getGeometry()
                                    .getUserData();
                            double areaOfGeom = (Double) f.attributes
                                    .get(WarngenLayer.AREA);
                            String key = createKey(data.entry.attributes,
                                    asc.getFipsField(), asc.getAreaField());
                            if (intersectMap.containsKey(key)) {
                                Geometry intersect1 = intersectMap.get(key);
                                CountyUserData cud = (CountyUserData) intersect1
                                        .getUserData();
                                cud.gid = GeometryUtil.getPrefix(cud);
                                intersect = GeometryUtil.union(intersect1,
                                        intersect);
                                intersect.setUserData(cud);
                                areaOfGeom = areaOfGeomMap.get(key);
                            }
                            intersectMap.put(key, intersect);
                            areaOfGeomMap.put(key, areaOfGeom);
                        }
                    }

                    for (String key : intersectMap.keySet()) {
                        Geometry geometry = fipsCountynameGeometry.get(key);
                        Geometry intersect = intersectMap.get(key);
                        double areaOfGeom = areaOfGeomMap.get(key);

                        if (!intersect.isEmpty()
                                && warngenLayer.filterArea(intersect, geometry,
                                        areaOfGeom, asc)) {
                            geoms.add(intersect);
                        }
                    }
                }

                AffectedAreas[] affectedAreas = findAffectedAreas(asc,
                        config.getGeospatialConfig(), warnPolygon,
                        localizedSite, geoms, warngenLayer);

                areasMap.put(asc.getVariable(), affectedAreas);
            }
        }
        perfLog.logDuration("findIntersectingAreas", System.currentTimeMillis() - t0);

        return areasMap;
    }

    /*
     * Convert input to Polygon or Multipolygon. This will discard any
     * non-polygon elements.
     */
    private static Geometry toPolygonal(Geometry input) {
        Geometry result;
        if (input instanceof Polygonal) {
            result = input;
        } else {
            List<Polygon> pa = new ArrayList<>(input.getNumGeometries() + 63);
            toPolygonalInner(input, pa);
            result = input.getFactory().createMultiPolygon(pa.toArray(new Polygon[pa.size()]));
        }
        return result;
    }

    private static void toPolygonalInner(Geometry input, List<Polygon> pa) {
        int n = input.getNumGeometries();
        for (int i = 0; i < n; ++i) {
            Geometry g = input.getGeometryN(i);
            if (g instanceof Polygon) {
                pa.add((Polygon) g);
            } else if (g instanceof GeometryCollection) {
                toPolygonalInner(g, pa);
            }
        }
    }

    private static class FeatureIntersection implements Callable<Geometry> {
        private Geometry waPoly;
        private GeospatialData f;

        public FeatureIntersection(Geometry waPoly, GeospatialData f) {
            this.waPoly = waPoly;
            this.f = f;
        }

        @Override
        public Geometry call() throws Exception {
            Geometry intersect = null;
            if (f.prepGeom.intersects(waPoly)) {
                try {
                    Geometry fgPoly = toPolygonal(f.geometry);
                    List<Geometry> out = new ArrayList<Geometry>(64);
                    subdivIntersect(waPoly, fgPoly, true, out);
                    intersect = waPoly.getFactory().createGeometryCollection(
                            out.toArray(new Geometry[out.size()]));
                    // subdivIntersect loses user data to set it again.
                    intersect.setUserData(f.geometry.getUserData());
                } catch (TopologyException e) {
                    intersect = GeometryUtil.intersection(waPoly,
                            f.prepGeom, true);
                }
            }
            return intersect;
        }
    }

    private static void subdivIntersect(Geometry warnArea, Geometry featureGeom,
            boolean orient, List<Geometry> out) {
        Envelope env = warnArea.getEnvelopeInternal().intersection(
                featureGeom.getEnvelopeInternal());
        if (env.isNull()) {
            return;
        }
        Coordinate[] c = new Coordinate[5];
        c[0] = new Coordinate(env.getMinX(), env.getMinY());
        c[1] = new Coordinate(env.getMaxX(), env.getMinY());
        c[2] = new Coordinate(env.getMaxX(), env.getMaxY());
        c[3] = new Coordinate(env.getMinX(), env.getMaxY());
        c[4] = c[0];
        subdivIntersectInner(c, warnArea, featureGeom, orient, 1, out);
    }

    private static void subdivIntersectInner(Coordinate[] env, Geometry warnArea,
            Geometry featureGeom, boolean orientation, int depth,
            List<Geometry> out) {
        if (warnArea.getNumGeometries() * featureGeom.getNumGeometries() <= DEFAULT_SUBDIVISION_TRESHOLD
                || depth >= MAX_SUBDIVISION_DEPTH) {
            out.add(batchIntersect(warnArea, featureGeom));
        } else if (featureGeom.getNumGeometries() <= SIMPLE_FEATURE_GEOM_COUNT_THRESHOLD) {
            try {
                Polygon clipPoly = warnArea.getFactory().createPolygon(env);
                Geometry clippedWarnArea = clip(clipPoly, warnArea);
                /*
                 * Not clipping feature geometry because it is already known to
                 * have a small geometry count.
                 */
                out.add(batchIntersect(clippedWarnArea, featureGeom));
            } catch (TopologyException e) {
                // Additional fallback without clipping.
                statusHandler.handle(Priority.DEBUG,
                        "Clipped intersection failed.  Will attempt fallback.", e);
                out.add(GeometryUtil.intersection(warnArea, featureGeom, true));
            }
        } else {
            GeometryFactory gf = warnArea.getFactory();
            Coordinate[] c = new Coordinate[5];
            List<Geometry> subOut = new ArrayList<>();
            for (int side = 0; side < 2; ++side) {
                if (side == 0) {
                    if (orientation) {
                        // horizontal split
                        c[0] = env[0];
                        c[1] = new Coordinate((env[0].x + env[1].x) / 2, env[0].y);
                        c[2] = new Coordinate(c[1].x, env[2].y);
                        c[3] = env[3];
                        c[4] = c[0];
                    } else {
                        // vertical split
                        c[0] = env[0];
                        c[1] = env[1];
                        c[2] = new Coordinate(c[1].x, (env[0].y + env[3].y) / 2);
                        c[3] = new Coordinate(c[0].x, c[2].y);
                        c[4] = c[0];
                    }
                } else {
                    if (orientation) {
                        c[0] = c[1];
                        c[3] = c[2];
                        c[1] = env[1];
                        c[2] = env[2];
                        c[4] = c[0];
                    } else {
                        c[0] = c[3];
                        c[1] = c[2];
                        c[2] = env[2];
                        c[3] = env[3];
                        c[4] = c[0];
                    }
                }

                Polygon clipPoly = gf.createPolygon(c);
                try {
                    Geometry subWarnArea = clip(clipPoly, warnArea);
                    Geometry subFeatureGeom = clip(clipPoly, featureGeom);
                    subdivIntersectInner(c, subWarnArea, subFeatureGeom,
                            !orientation, depth + 1, subOut);
                } catch (TopologyException e) {
                    // Additional fallback without clipping.
                    statusHandler.handle(Priority.DEBUG,
                            "Subdivided intersection failed.  Will attempt fallback.", e);
                    out.add(GeometryUtil.intersection(warnArea, featureGeom, true));
                    return;
                }
            }
            out.addAll(subOut);
        }
    }

    /**
     * Calculate the intersection of p and g by operating on each element of g.
     * This is necessary to prevent "side location conflict" errors. By using
     * envelopes to either filter out elements or bypass
     * Geometry.intersection(), it also is much faster than p.intersection(g)
     * would be.
     *
     * @param p
     * @param g must be Polygonal
     * @return
     */
    private static Geometry clip(Polygon p, Geometry g) {
        Envelope pe = p.getEnvelopeInternal();
        List<Polygon> out = new ArrayList<>(g.getNumGeometries() * 11 / 10);
        int n = g.getNumGeometries();
        for (int i = 0; i < n; ++i) {
            Geometry gi = g.getGeometryN(i);
            Envelope ge = gi.getEnvelopeInternal();
            if (pe.contains(ge)) {
                out.add((Polygon) gi);
            } else if (pe.intersects(ge)) {
                Geometry clipped = p.intersection(gi);
                int m = clipped.getNumGeometries();
                for (int j = 0; j < m; ++j) {
                    Geometry gj = clipped.getGeometryN(j);
                    if (!gj.isEmpty() && gj instanceof Polygon) {
                        out.add((Polygon) gj);
                    }
                }
            }
            // else, discard gi
        }
        return g.getFactory().createMultiPolygon(out.toArray(new Polygon[out.size()]));
    }

    private static Geometry batchIntersect(Geometry warnArea, Geometry featureGeom) {
        return GeometryUtil.intersection(warnArea, featureGeom, true);
    }

    public static List<String> converFeAreaToPartList(String feArea) {
        final List<String> partList = new ArrayList<String>();
        if (feArea != null) {
            feArea = feArea.toUpperCase();
            if (SPECIAL_CASE_FE_AREAS.contains(feArea)) {
                partList.add(feArea);
            } else {
                for (int i = 0; i < feArea.length(); i++) {
                    char c = feArea.charAt(i);
                    Direction direction = null;
                    switch (c) {

                    case 'C':
                        direction = Direction.CENTRAL;
                        break;
                    case 'W':
                        direction = Direction.WEST;
                        break;
                    case 'N':
                        direction = Direction.NORTH;
                        break;
                    case 'E':
                        direction = Direction.EAST;
                        break;
                    case 'S':
                        direction = Direction.SOUTH;
                        break;
                    default:
                        break;
                    }

                    if (direction != null) {
                        partList.add(direction.toString());
                    }
                }
            }
        }
        return partList;
    }

    public void createFipsGeometryMap(WarngenConfiguration config,
            String localizedSite, WarngenLayer warngenLayer) {
        for (AreaSourceConfiguration asc : config.getAreaSources()) {
            for (GeospatialData f : warngenLayer.getGeodataFeatures(
                    asc.getAreaSource(), localizedSite)) {
                Geometry g = f.getGeometry();
                CountyUserData data = (CountyUserData) g.getUserData();
                if (data != null) {
                    Geometry geom = (Geometry) data.entry.getGeometry();
                    String mapKey = createKey(data.entry.attributes,
                            asc.getFipsField(), asc.getAreaField());
                    if (fipsCountynameGeometry.containsKey(mapKey)) {
                        Geometry geom1 = fipsCountynameGeometry.get(mapKey);
                        CountyUserData cud;
                        cud = ((CountyUserData) geom1.getUserData()).copyEntry();
                        cud.gid = GeometryUtil.getPrefix(cud);
                        geom = GeometryUtil.union(geom1, geom);
                        geom.setUserData(cud);
                        countiesOfSplitArea.add(mapKey);
                    }
                    fipsCountynameGeometry.put(mapKey, geom);
                }
            }
        }
        Iterator<String> iter = countiesOfSplitArea.iterator();
        while (iter.hasNext()) {
            // replace LAT/LON with centroid of combined areas
            String mapKey = iter.next();
            Geometry regionGeom = (Geometry) fipsCountynameGeometry.get(mapKey);
            CountyUserData cud = (CountyUserData) regionGeom.getUserData();
            cud.entry.attributes.put("LAT", regionGeom.getCentroid()
                    .getY());
            cud.entry.attributes.put("LON", regionGeom.getCentroid()
                    .getX());
            regionGeom.setUserData(cud);
        }
    }

    private String createKey(Map<String, Object> attributes, String fipsField,
            String areaField) {
        return attributes.get(fipsField) + "_" + attributes.get(areaField);
    }
}
