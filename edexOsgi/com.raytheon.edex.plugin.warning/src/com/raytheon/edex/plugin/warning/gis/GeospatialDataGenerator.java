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
package com.raytheon.edex.plugin.warning.gis;

import java.io.File;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.warning.WarningConstants;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.DialogConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.GeospatialConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GenerateGeospatialDataResult;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialDataSet;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialFactory;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialMetadata;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialTime;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialTimeSet;
import com.raytheon.uf.common.dataplugin.warning.util.StringUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.simplify.TopologyPreservingSimplifier;

/**
 * Saves off geospatial data from the maps database using the warngen
 * configurations to improve performance of various operations in warngen.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2011            rjpeter     Initial creation
 * Mar 29, 2012  #14691    Qinglu Lin  Added returned value of getFeArea() of 
 *                                     AreaConfiguration to areaFields List.
 * May  7, 2013  15690     Qinglu Lin  Added convertToMultiPolygon() and updated queryGeospatialData().
 * Oct 22, 2013  2361      njensen     Use JAXBManager for XML
 * Feb 07, 2014  16090  mgamazaychikov Changed visibility of some methods
 * Mar 19, 2014  2726      rjpeter     Made singleton instance.
 * Apr 29, 2014  3033      jsanchez    Properly handled site and back up site files.
 * Jul 15, 2014  3352      rferrel     Better logging and threading added.
 * Aug 21, 2014  3353      rferrel     Added getGeospatialTimeset and cluster locking of METADATA_FILE.
 *                                      generateGeoSpatialList now sends GenerateGeospatialDataResult.
 * Jun 26, 2015  17212     Qinglu Lin  Removed features whose geometry is empty in queryGeospatialData(),
 *                                     caught exception in updateFeatures() & topologySimplifyQueryResults(),
 *                                     and added composeMessage().
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GeospatialDataGenerator {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeospatialDataGenerator.class);

    private final SingleTypeJAXBManager<GeospatialTimeSet> jaxb = SingleTypeJAXBManager
            .createWithoutException(GeospatialTimeSet.class);

    private final String updaterEndpoint;

    /**
     * Pool to service the callable Geometry.
     */
    private final ExecutorService pool;

    /**
     * Property in the warning.properties to determine the maximum number of
     * geometry threads.
     */
    private final String THREAD_COUNT_PROPERTY = "geospatial.geometry.threads";

    /** Default thread count must be a valid positive number string. */
    private final String DEFAULT_THREAD_COUNT = "5";

    /** Cluster task name. */
    private final static String CLUSTER_NAME = "WarngenGeometryGenerator";

    /** Time out lock after one minute. */
    private final static long TIME_OUT = TimeUtil.MILLIS_PER_MINUTE;

    /** Task to update the lock time for the locked plugin cluster task. */
    private static final class LockUpdateTask extends TimerTask {
        /** The locked cluster task's details. */
        private final String details;

        public LockUpdateTask(String details) {
            this.details = details;
        }

        @Override
        public void run() {
            long currentTime = System.currentTimeMillis();
            ClusterLockUtils.updateLockTime(CLUSTER_NAME, details, currentTime);
        }
    }

    /**
     * Common format for cluster tasks details entry.
     * 
     * @param site
     * @param fileName
     * @return details
     */
    private static String getDetails(String site, String fileName) {
        return String.format("%s%s%s", site, File.separator, fileName);
    }

    /**
     * Lock cluster task for the site's metadata file and obtain the file's
     * geospatial time set.
     * 
     * @param site
     * @return geospatialTimeSet
     */
    public static GeospatialTimeSet getGeospatialTimeset(String site) {
        String metadataDetails = getDetails(site,
                GeospatialFactory.METADATA_FILE
                        .substring(GeospatialFactory.METADATA_FILE
                                .lastIndexOf(File.separator) + 1));

        ClusterTask ct = null;
        try {
            do {
                ct = ClusterLockUtils.lock(CLUSTER_NAME, metadataDetails,
                        TIME_OUT, true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));
            return GeospatialFactory.getGeospatialTimeSet(site);
        } finally {
            if (ct != null) {
                ClusterLockUtils.unlock(ct, false);
            }
        }
    }

    /**
     * The constructor.
     */
    public GeospatialDataGenerator(String updaterEndpoint) {
        this.updaterEndpoint = updaterEndpoint;
        this.pool = initPool(THREAD_COUNT_PROPERTY, DEFAULT_THREAD_COUNT);
    }

    /**
     * Parse property for thread count and return a fixed thread pool.
     * 
     * @param propKey
     * @param defaultStr
     * @return pool
     */
    private ExecutorService initPool(String propKey, String defaultStr) {
        String maxThreadStr = System.getProperty(propKey, defaultStr);
        int maxThreads = -1;
        try {
            maxThreads = Integer.parseInt(maxThreadStr);
            if (maxThreads <= 0) {
                maxThreads = Integer.parseInt(defaultStr);
            }
        } catch (NumberFormatException ex) {
            maxThreads = Integer.parseInt(defaultStr);
        }

        return Executors.newFixedThreadPool(maxThreads);

    }

    public void generateUniqueGeospatialMetadataGeometries() {
        String mySite = SiteUtil.getSite();
        DialogConfiguration dialogConfig = null;

        try {
            dialogConfig = DialogConfiguration.loadDialogConfig(mySite);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Error loading warngen config.xml", e);
            return;
        }
        List<String> sites = getBackupSites(dialogConfig);
        sites.add(0, mySite);
        List<String> templates = getTemplates(dialogConfig);
        Set<GeospatialMetadata> metaDataSet = getMetaDataSet(sites, templates);

        for (final String site : sites) {
            long start = System.currentTimeMillis();
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO,
                        "Checking warngen geometries for site: " + site);
            }

            for (final GeospatialMetadata md : metaDataSet) {
                try {
                    generateGeoSpatialList(site, md);
                } catch (SpatialException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                long time = System.currentTimeMillis() - start;
                statusHandler.handle(Priority.INFO, String.format(
                        "Checking warngen geometries for site: %s, took: %s",
                        site, TimeUtil.prettyDuration(time)));
            }
        }
    }

    public Set<GeospatialMetadata> getMetaDataSet(List<String> sites,
            List<String> templates) {

        Set<GeospatialMetadata> metaDataSet = new HashSet<GeospatialMetadata>();

        for (String site : sites) {
            metaDataSet.clear();

            // get the unique geospatialMetadata sets to generate
            for (String templateName : templates) {
                WarngenConfiguration template = null;
                try {
                    template = WarngenConfiguration.loadConfig(templateName,
                            site, null);
                } catch (Exception e) {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Error loading template ["
                                            + templateName
                                            + "] for site ["
                                            + site
                                            + "], skipping geometry generation for site/template.",
                                    e);
                    continue;
                }

                Map<String, GeospatialMetadata> metadataMap = GeospatialFactory
                        .getMetaDataMap(template);
                for (GeospatialMetadata gmd : metadataMap.values()) {
                    metaDataSet.add(gmd);
                }
            }
        }
        return metaDataSet;
    }

    public static List<String> getBackupSites(DialogConfiguration dialogConfig) {
        String[] CWAs = dialogConfig.getBackupCWAs().split(",");
        List<String> rval = new ArrayList<String>(CWAs.length + 1);
        for (String s : CWAs) {
            if (s.length() > 0) {
                rval.add(StringUtil.parseBackupCWAs(s)[0]);
            }
        }
        return rval;
    }

    public static List<String> getTemplates(DialogConfiguration dialogConfig) {
        String[] mainProducts = dialogConfig.getMainWarngenProducts()
                .split(",");
        String[] otherProducts = dialogConfig.getOtherWarngenProducts().split(
                ",");
        List<String> rval = new ArrayList<String>(mainProducts.length
                + otherProducts.length);
        for (String s : mainProducts) {
            if (s.length() > 0) {
                rval.add(s.split("/")[1]);
            }
        }
        for (String s : otherProducts) {
            if (s.length() > 0) {
                rval.add(s.split("/")[1]);
            }
        }
        return rval;
    }

    private String getClusterDetails(String site, GeospatialMetadata metaData) {
        String fileName = generateGeoDataFilename(metaData);
        return getDetails(site, fileName);
    }

    public GeospatialDataSet generateGeoSpatialList(String site,
            GeospatialMetadata metaData) throws SpatialException {
        GeospatialDataSet dataSet = null;
        String details = getClusterDetails(site, metaData);
        ClusterTask ct = null;
        Timer lockUpdateTimer = null;
        long start = 0L;
        try {
            do {
                ct = ClusterLockUtils.lock(CLUSTER_NAME, details, TIME_OUT,
                        true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

            start = System.currentTimeMillis();
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                String message = String
                        .format("Start generate Geo Spatial data for site: %s, lock: %s.",
                                site, ct.getId().getDetails());
                statusHandler.handle(Priority.DEBUG, message);
            }

            lockUpdateTimer = new Timer(CLUSTER_NAME + " timer", true);
            TimerTask tt = new LockUpdateTask(ct.getId().getDetails());
            lockUpdateTimer.schedule(tt, TIME_OUT / 2L, TIME_OUT / 2L);

            GeospatialTime curTime = null;

            try {
                curTime = queryForCurrentTimes(metaData);
            } catch (Exception e) {
                throw new SpatialException(
                        "Unable to generate geo spatial data.  Error occurred looking up map database version times.",
                        e);
            }

            // NOTE: changes to curTimeMap are not persisted back to the
            // GeospatialTimeSet
            Map<GeospatialMetadata, GeospatialTime> lastRunTimeMap = GeospatialFactory
                    .loadLastRunGeoTimeSet(getGeospatialTimeset(site));

            GeospatialTime lastRunTime = lastRunTimeMap.get(metaData);
            boolean generate = true;
            if (curTime.equals(lastRunTime)) {
                // load from disk, need to verify they load in correctly
                try {
                    dataSet = GeospatialFactory.loadAreaGeoData(site,
                            lastRunTime);
                } catch (Exception e) {
                    statusHandler
                            .handle(Priority.WARN,
                                    "Failed to load area geometry files from disk.  Regenerating geometries.",
                                    e);
                }

                if (dataSet == null) {
                    // If the file does not exist, then geoms need to be
                    // deleted.
                    deleteGeomFiles(site, lastRunTime);
                } else {
                    generate = false;
                }
            } else {
                statusHandler
                        .handle(Priority.INFO,
                                "Geometry metadata has changed.  Regenerating geometries.");
            }

            if (generate) {
                dataSet = new GeospatialDataSet();
                GeospatialData[] areas = null;

                // generate data
                try {
                    areas = queryGeospatialData(site, metaData);
                } catch (Exception e) {
                    throw new SpatialException(
                            "Unable to generate area geometries.  Error occurred looking up geometries.",
                            e);
                }

                GeometryFactory gf = new GeometryFactory();
                int size = areas.length;
                Geometry[] geoms = new Geometry[size];
                for (int i = 0; i < size; i++) {
                    geoms[i] = areas[i].geometry;
                }

                // Get convex hull of cwa
                Geometry hull = gf.createGeometryCollection(geoms).convexHull();

                // add time zone data
                try {
                    dataSet.setTimezones(queryTimeZones(metaData, hull, areas));
                } catch (Exception e) {
                    throw new SpatialException(
                            "Unable to generate area geometries.  Error occurred looking up time zones.",
                            e);
                }

                dataSet.setAreas(areas);

                // Get parent areas that intersect with cwa
                try {
                    dataSet.setParentAreas(queryParentAreas(metaData, hull));
                } catch (Exception e) {
                    throw new SpatialException(
                            "Unable to generate area geometries.  Error occurred looking up parent areas.",
                            e);
                }

                // save to disk
                try {
                    persistGeoData(site, lastRunTimeMap, curTime, dataSet);

                    if (updaterEndpoint != null) {
                        GenerateGeospatialDataResult result = new GenerateGeospatialDataResult();
                        String updatedTimeStamp = getTimeStamp(curTime,
                                lastRunTime);
                        result.setTimestamp(updatedTimeStamp);
                        result.setSite(site);
                        result.setArea(metaData.getAreaSource());
                        try {
                            EDEXUtil.getMessageProducer().sendAsync(
                                    updaterEndpoint, result);
                        } catch (EdexException e) {
                            statusHandler.error("Could not send message to "
                                    + updaterEndpoint, e);
                        }
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Error occurred persisting area geometry data", e);
                }
            }
        } finally {
            if (lockUpdateTimer != null) {
                lockUpdateTimer.cancel();
                lockUpdateTimer = null;
            }

            if (ct != null) {
                ClusterLockUtils.unlock(ct, false);
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    long time = System.currentTimeMillis() - start;
                    String message = String
                            .format("Generated Geo Spatial data for site: %s, lock: %s, total time: %s.",
                                    site, ct.getId().getDetails(),
                                    TimeUtil.prettyDuration(time));
                    statusHandler.handle(Priority.DEBUG, message);
                }
            }
        }
        return dataSet;
    }

    public GeospatialMetadata getMetaData(WarngenConfiguration template) {
        GeospatialMetadata rval = new GeospatialMetadata();
        GeospatialConfiguration geoConfig = template.getGeospatialConfig();
        AreaSourceConfiguration areaConfig = template.getHatchedAreaSource();
        rval.setAreaSource(geoConfig.getAreaSource());
        List<String> areaFields = new ArrayList<String>();
        areaFields.add(WarningConstants.GID);
        areaFields.add(areaConfig.getAreaField());
        areaFields.add(areaConfig.getFipsField());
        areaFields.add(areaConfig.getAreaNotationField());
        if (areaConfig.getFeAreaField() != null) {
            areaFields.add(areaConfig.getFeAreaField());
        }
        if (areaConfig.getTimeZoneField() != null) {
            areaFields.add(areaConfig.getTimeZoneField());
        }
        rval.setFipsField(areaConfig.getFipsField());
        rval.setAreaFields(areaFields);
        rval.setTimeZoneSource(geoConfig.getTimezoneSource());
        rval.setTimeZoneField(geoConfig.getTimezoneField());
        rval.setParentAreaSource(geoConfig.getParentAreaSource());
        rval.setAreaNotationField(areaConfig.getAreaNotationField());
        rval.setParentAreaField(areaConfig.getParentAreaField());
        return rval;
    }

    public static GeospatialTime queryForCurrentTimes(
            GeospatialMetadata metaData) throws Exception {
        GeospatialTime rval = new GeospatialTime();
        String areaSource = metaData.getAreaSource().toLowerCase();
        String tzSource = metaData.getTimeZoneSource();
        String pAreaSource = metaData.getParentAreaSource();
        StringBuilder sql = new StringBuilder(200);
        sql.append("SELECT table_name, import_time FROM mapdata.map_version WHERE table_name in ('");
        sql.append(areaSource.toLowerCase());
        if ((tzSource != null) && (tzSource.length() > 0)) {
            tzSource = tzSource.toLowerCase();
            sql.append("', '");
            sql.append(tzSource);
        }
        if ((pAreaSource != null) && (pAreaSource.length() > 0)) {
            pAreaSource = pAreaSource.toLowerCase();
            sql.append("', '");
            sql.append(pAreaSource);
        }

        sql.append("')");
        CoreDao dao = new CoreDao(DaoConfig.forDatabase("maps"));
        Object[] rows = dao.executeSQLQuery(sql.toString());
        for (Object rowObj : rows) {
            Object[] row = (Object[]) rowObj;
            String table = row[0].toString();
            Timestamp ts = (Timestamp) row[1];
            long time = ts.getTime();

            // not using else/if in case tz or pArea was the same table as area
            if (table.equals(areaSource)) {
                rval.setAreaSourceTime(time);
            }
            if (table.equals(pAreaSource)) {
                rval.setParentSourceTime(time);
            }
            if (table.equals(tzSource)) {
                rval.setTimeZoneSourceTime(time);
            }
        }
        rval.setMetaData(metaData);
        return rval;
    }

    private GeospatialData[] queryGeospatialData(String site,
            GeospatialMetadata metaData) throws SpatialException,
            InterruptedException, ExecutionException {
        String areaSource = metaData.getAreaSource();

        HashMap<String, RequestConstraint> map = new HashMap<String, RequestConstraint>(
                2);
        String name = "cwa";
        if (areaSource.equalsIgnoreCase(WarningConstants.MARINE)) {
            name = "wfo";
        }
        map.put(name, new RequestConstraint(site, ConstraintType.LIKE));

        List<String> areaFields = metaData.getAreaFields();
        SpatialQueryResult[] features = SpatialQueryFactory.create().query(
                areaSource, areaFields.toArray(new String[areaFields.size()]),
                null, map, SearchMode.WITHIN);

        // clip against County Warning Area
        if (!areaSource.equalsIgnoreCase(WarningConstants.MARINE)) {
            String cwaSource = "cwa";
            List<String> cwaAreaFields = new ArrayList<String>(Arrays.asList(
                    "wfo", "gid"));
            HashMap<String, RequestConstraint> cwaMap = new HashMap<String, RequestConstraint>(
                    2);
            cwaMap.put("wfo", new RequestConstraint(site, ConstraintType.LIKE));
            SpatialQueryResult[] cwaFeatures = SpatialQueryFactory.create()
                    .query(cwaSource,
                            cwaAreaFields.toArray(new String[cwaAreaFields
                                    .size()]), null, cwaMap, SearchMode.WITHIN);
            updateFeatures(features, cwaFeatures, areaSource);
        }

        boolean emptyFeatureFound = false;
        List<SpatialQueryResult> geomFeatures = new ArrayList<SpatialQueryResult>();
        for (int i = 0; i < features.length; i++) {
            if (features[i].geometry.isEmpty()) {
                // create log message for a feature that has empty geometry
                emptyFeatureFound = true;
                String message = composeMessage("Geometry for", areaSource, features[i],
                        "in features[" + i + "] is empty. Check out " + areaSource
                        + " and CWA tables/shapefiles.");
                statusHandler.handle(Priority.ERROR, message);
            } else {
                geomFeatures.add(features[i]);
            }
        }
        if (emptyFeatureFound) {
            // recreate features in which each feature has no-empty geometry
            features = geomFeatures.toArray(new SpatialQueryResult[geomFeatures.size()]);
        }

        topologySimplifyQueryResults(features, areaSource);

        // convert to GeospatialData
        GeospatialData[] rval = new GeospatialData[features.length];

        for (int i = 0; i < features.length; i++) {
            SpatialQueryResult r = features[i];
            GeospatialData data = new GeospatialData();
            data.attributes = r.attributes;
            data.geometry = r.geometry;
            rval[i] = data;
        }

        return rval;
    }

    /**
     * Queue request for each feature and wait for the results.
     * 
     * @param features
     * @param cwaFeatures
     * @throws InterruptedException
     * @throws ExecutionException
     */
    private void updateFeatures(SpatialQueryResult[] features,
            SpatialQueryResult[] cwaFeatures, String areaSource) throws
            InterruptedException, ExecutionException {
        List<Future<Geometry>> featureFutures = new ArrayList<Future<Geometry>>(
                features.length);
        Geometry[] cwaFeturesGeoms = new Geometry[cwaFeatures.length];
        for (int index = 0; index < cwaFeturesGeoms.length; ++index) {
            cwaFeturesGeoms[index] = cwaFeatures[index].geometry;
        }
        List<Future<Geometry>> geomFutures = null;
        List<Callable<Geometry>> featuresCallable = new ArrayList<Callable<Geometry>>(
                featureFutures.size());

        // Queue all intersections.
        for (int index = 0; index < features.length; ++index) {

            geomFutures = submitGeomIntersection(features[index].geometry,
                    cwaFeturesGeoms);
            Callable<Geometry> callable = new FeatureCallable(geomFutures,
                    statusHandler);
            featuresCallable.add(callable);
        }

        // Finally queue all features.
        featureFutures = pool.invokeAll(featuresCallable);

        for (int index = 0; index < features.length; ++index) {
            try {
                Future<Geometry> future = featureFutures.get(index);
                features[index].geometry = future.get();
            } catch (Exception e) {
                String message = composeMessage("Error while trying to get geometry for",
                        areaSource, features[index],
                        "from element " + index + " of featureFutures.");
                statusHandler.handle(Priority.ERROR, message);
                throw e;
            }
        }
    }

    /**
     * Get future's list for a feature's geometry.
     * 
     * @param featureGeom
     * @param cwaFeaturesGeoms
     * @return geomFutures
     */
    private List<Future<Geometry>> submitGeomIntersection(Geometry featureGeom,
            Geometry[] cwaFeaturesGeoms) {
        List<Future<Geometry>> geomFutures = new ArrayList<Future<Geometry>>(
                cwaFeaturesGeoms.length);
        for (Geometry cwaGeom : cwaFeaturesGeoms) {
            Callable<Geometry> callable = new GeomIntersectionCallable(
                    featureGeom, cwaGeom);
            geomFutures.add(pool.submit(callable));
        }

        return geomFutures;
    }

    /**
     * Simplifies the overall geometries using a single collection to preserve
     * boundaries. Geometries are updated in place.
     * 
     * @param results
     */
    private void topologySimplifyQueryResults(SpatialQueryResult[] results, String areaSource) {
        for (int i = 0; i < results.length; i++) {
            try {
                results[i].geometry = TopologyPreservingSimplifier.simplify(results[i].geometry, 0.0001);
            } catch (RuntimeException e) {
                String message = composeMessage("Error while invoking "
                        + "TopologyPreservingSimplifier.simplify() for geometry of results["
                        + i + "] for", areaSource, results[i], ".");
                statusHandler.handle(Priority.ERROR, message);
                throw e;
            }
        }
    }

    /**
     * Queries the time zone table. Updates the geoData inline with the results
     * of the time zone query.
     * 
     * @param metaData
     * @param hull
     * @param geoData
     */
    private GeospatialData[] queryTimeZones(GeospatialMetadata metaData,
            Geometry hull, GeospatialData[] geoData) throws SpatialException {
        GeospatialData[] rval = null;
        String timezonePathcastTable = metaData.getTimeZoneSource();
        String timezonePathcastField = metaData.getTimeZoneField();

        // Get time zones that intersect with cwa
        if (timezonePathcastTable != null) {
            SpatialQueryResult[] timeZoneResults = SpatialQueryFactory.create()
                    .query(timezonePathcastTable,
                            new String[] { timezonePathcastField }, hull, null,
                            false, SearchMode.INTERSECTS);

            rval = new GeospatialData[timeZoneResults.length];
            for (int i = 0; i < timeZoneResults.length; i++) {
                SpatialQueryResult result = timeZoneResults[i];
                GeospatialData data = new GeospatialData();
                data.geometry = result.geometry;
                data.attributes = result.attributes;
                rval[i] = data;
            }

            // set time zone and area field
            if (timeZoneResults.length == 1) {
                SpatialQueryResult tz = timeZoneResults[0];
                Object tzField = tz.attributes.get(timezonePathcastField);
                for (GeospatialData g : geoData) {
                    g.attributes.put(timezonePathcastField, tzField);
                }
            } else {
                for (SpatialQueryResult tz : timeZoneResults) {
                    PreparedGeometry tzGeometry = PreparedGeometryFactory
                            .prepare(tz.geometry);
                    Object tzField = tz.attributes.get(timezonePathcastField);
                    for (GeospatialData g : geoData) {
                        Map<String, Object> gAtts = g.attributes;
                        if (!gAtts.containsKey(timezonePathcastField)) {
                            Point centroid = g.geometry.getCentroid();
                            if (tzGeometry.contains(centroid)) {
                                gAtts.put(timezonePathcastField, tzField);
                            }
                        }
                    }
                }
            }
        }
        return rval;
    }

    /**
     * 
     * @param metaData
     * @param hull
     * @return
     * @throws SpatialException
     */
    private GeospatialData[] queryParentAreas(GeospatialMetadata metaData,
            Geometry hull) throws SpatialException {
        GeospatialData[] rval = null;
        String parentAreaSource = metaData.getParentAreaSource();
        if (parentAreaSource != null) {
            SpatialQueryResult[] parentRegionFeatures = SpatialQueryFactory
                    .create().query(
                            parentAreaSource,
                            new String[] { metaData.getAreaNotationField(),
                                    metaData.getParentAreaField() }, hull,
                            null, false, SearchMode.INTERSECTS);
            topologySimplifyQueryResults(parentRegionFeatures, metaData.getAreaSource());

            rval = new GeospatialData[parentRegionFeatures.length];
            for (int i = 0; i < parentRegionFeatures.length; i++) {
                SpatialQueryResult result = parentRegionFeatures[i];
                GeospatialData data = new GeospatialData();
                data.geometry = result.geometry;
                data.attributes = result.attributes;
                rval[i] = data;
            }
        }

        return rval;
    }

    /**
     * Save data in the desired file and update the meta data file. Assumes
     * already have a cluster lock for the data file. A cluster lock is obtained
     * on the metadata file prior to updating it.
     * 
     * @param site
     * @param times
     * @param curTime
     * @param geoData
     * @throws SerializationException
     * @throws LocalizationException
     * @throws JAXBException
     */
    private void persistGeoData(String site,
            Map<GeospatialMetadata, GeospatialTime> times,
            GeospatialTime curTime, GeospatialDataSet geoData)
            throws SerializationException, LocalizationException, JAXBException {

        String fileName = generateGeoDataFilename(curTime.getMetaData());
        String metadataDetails = getDetails(site,
                GeospatialFactory.METADATA_FILE
                        .substring(GeospatialFactory.METADATA_FILE
                                .lastIndexOf(File.separator) + 1));

        ClusterTask ct = null;
        try {
            do {
                ct = ClusterLockUtils.lock(CLUSTER_NAME, metadataDetails,
                        TIME_OUT, true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext context = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            context.setContextName(site);

            byte[] data = SerializationUtil.transformToThrift(geoData);
            LocalizationFile lf = pathMgr.getLocalizationFile(context,
                    GeospatialFactory.GEO_DIR + fileName);
            lf.write(data);

            curTime.setFileName(fileName);
            times.put(curTime.getMetaData(), curTime);

            GeospatialTimeSet set = new GeospatialTimeSet();
            set.setData(new ArrayList<GeospatialTime>(times.values()));
            String xml = jaxb.marshalToXml(set);

            lf = pathMgr.getLocalizationFile(context,
                    GeospatialFactory.METADATA_FILE);
            lf.write(xml.getBytes());
        } finally {
            if (ct != null) {
                ClusterLockUtils.unlock(ct, false);
            }
        }
    }

    private void deleteGeomFiles(String site, GeospatialTime time) {
        String fileName = time.getFileName();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        context.setContextName(site);
        LocalizationFile lf = pathMgr.getLocalizationFile(context,
                GeospatialFactory.GEO_DIR + fileName);
        if (lf.exists()) {
            try {
                lf.delete();
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "Failed to delete area geometry file " + lf.getName(),
                        e);
            }
        }
    }

    private String generateGeoDataFilename(GeospatialMetadata metaData) {
        return metaData.getAreaSource() + "_" + metaData.hashCode() + ".bin";
    }

    private String getTimeStamp(GeospatialTime curTime,
            GeospatialTime lastRunTime) {
        long tmStampMs = 0;
        if (lastRunTime != null) {
            if (curTime.getAreaSourceTime() != lastRunTime.getAreaSourceTime()) {
                tmStampMs = curTime.getAreaSourceTime();
            } else if (curTime.getParentSourceTime() != lastRunTime
                    .getParentSourceTime()) {
                tmStampMs = curTime.getParentSourceTime();
            } else if (curTime.getTimeZoneSourceTime() != lastRunTime
                    .getTimeZoneSourceTime()) {
                tmStampMs = curTime.getTimeZoneSourceTime();
            }
        } else {
            tmStampMs = curTime.getAreaSourceTime();
        }

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        calendar.setTimeInMillis(tmStampMs);
        return sdf.format(calendar.getTime());
    }

    private String composeMessage(String s, String areaSource,
            SpatialQueryResult sqr, String where) {
        String name = (String) sqr.attributes.get("NAME");
        if (name == null) {
            name = (String) sqr.attributes.get("COUNTYNAME");
        }
        return String.format("%s %s %s (gid=%s) %s", s, areaSource, name,
                sqr.attributes.get(WarningConstants.GID).toString(), where);
    }

}
