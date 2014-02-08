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

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.warning.WarningConstants;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.DialogConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.GeospatialConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
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
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryCollectionIterator;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
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
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GeospatialDataGenerator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeospatialDataGenerator.class);

    private static final SingleTypeJAXBManager<GeospatialTimeSet> jaxb = SingleTypeJAXBManager
            .createWithoutException(GeospatialTimeSet.class);

    public static void generateUniqueGeospatialMetadataGeometries() {
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

        for (String site : sites) {
            statusHandler.handle(Priority.INFO,
                    "Generating warngen geometries for site: " + site);
            for (GeospatialMetadata md : metaDataSet) {
                try {
                    generateGeoSpatialList(site, md);
                } catch (Exception e) {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Failed to generate geospatial data for warngen",
                                    e);
                }
            }
        }
    }

    public static Set<GeospatialMetadata> getMetaDataSet(List<String> sites,
            List<String> templates) {

        Set<GeospatialMetadata> metaDataSet = new HashSet<GeospatialMetadata>();

        for (String site : sites) {
            metaDataSet.clear();

            // get the unique geospatialMetadata sets to generate
            for (String templateName : templates) {
                WarngenConfiguration template = null;
                try {
                    template = WarngenConfiguration.loadConfig(templateName,
                            site);
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
    static List<String> getBackupSites(DialogConfiguration dialogConfig) {
        String[] CWAs = dialogConfig.getBackupCWAs().split(",");
        List<String> rval = new ArrayList<String>(CWAs.length + 1);
        for (String s : CWAs) {
            if (s.length() > 0) {
                rval.add(StringUtil.parseBackupCWAs(s)[0]);
            }
        }
        return rval;
    }

    static List<String> getTemplates(DialogConfiguration dialogConfig) {
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

    public static GeospatialDataSet generateGeoSpatialList(String site,
            GeospatialMetadata metaData) throws SpatialException {
        GeospatialDataSet dataSet = null;
        String file = generateGeoDataFilename(metaData);
        ClusterTask ct = null;
        try {
            do {
                ct = ClusterLockUtils.lock("WarngenGeometryGenerator", file,
                        60000, true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

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
                    .loadLastRunGeoTimeSet(site);
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
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Error occurred persisting area geometry data", e);
                }
            }
        } finally {
            if (ct != null) {
                ClusterLockUtils.unlock(ct, false);
            }
        }
        return dataSet;
    }

    public static GeospatialMetadata getMetaData(WarngenConfiguration template) {
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

    static GeospatialTime queryForCurrentTimes(
            GeospatialMetadata metaData) throws Exception {
        GeospatialTime rval = new GeospatialTime();
        String areaSource = metaData.getAreaSource().toLowerCase();
        String tzSource = metaData.getTimeZoneSource();
        String pAreaSource = metaData.getParentAreaSource();
        StringBuilder sql = new StringBuilder(200);
        sql.append("SELECT table_name, import_time FROM mapdata.map_version WHERE table_name in ('");
        sql.append(areaSource.toLowerCase());
        if (tzSource != null && tzSource.length() > 0) {
            tzSource = tzSource.toLowerCase();
            sql.append("', '");
            sql.append(tzSource);
        }
        if (pAreaSource != null && pAreaSource.length() > 0) {
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

    private static GeospatialData[] queryGeospatialData(String site,
            GeospatialMetadata metaData) throws SpatialException {
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
            Geometry multiPolygon = null;
            Geometry clippedGeom = null;
            for (int i = 0; i < features.length; i++) {
                multiPolygon = null;
                for (int j = 0; j < cwaFeatures.length; j++) {
                    clippedGeom = features[i].geometry
                            .intersection(cwaFeatures[j].geometry);
                    if (clippedGeom instanceof GeometryCollection) {
                        GeometryCollection gc = (GeometryCollection) clippedGeom;
                        if (multiPolygon != null)
                            multiPolygon = multiPolygon
                                    .union(convertToMultiPolygon(gc));
                        else
                            multiPolygon = convertToMultiPolygon(gc);
                    }
                }
                if (multiPolygon != null)
                    features[i].geometry = multiPolygon;
                else if (clippedGeom != null)
                    features[i].geometry = clippedGeom;
            }
        }

        topologySimplifyQueryResults(features);

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
     * Convert a GeometryCollection to a MultiPolygon.
     * 
     * @param gc
     */
    private static MultiPolygon convertToMultiPolygon(GeometryCollection gc) {
        GeometryCollectionIterator iter = new GeometryCollectionIterator(gc);
        Set<Polygon> polygons = new HashSet<Polygon>();
        MultiPolygon mp = null;
        iter.next();
        while (iter.hasNext()) {
            Object o = iter.next();
            if (o instanceof MultiPolygon) {
                if (mp == null)
                    mp = (MultiPolygon) o;
                else
                    mp = (MultiPolygon) mp.union((MultiPolygon) o);
            } else if (o instanceof Polygon) {
                polygons.add((Polygon) o);
            } else if (o instanceof LineString || o instanceof Point) {
                LinearRing lr = null;
                Coordinate[] coords = null;
                if (o instanceof LineString) {
                    Coordinate[] cs = ((LineString) o).getCoordinates();
                    if (cs.length < 4) {
                        coords = new Coordinate[4];
                        for (int j = 0; j < cs.length; j++)
                            coords[j] = new Coordinate(cs[j]);
                        for (int j = cs.length; j < 4; j++)
                            coords[j] = new Coordinate(cs[3 - j]);
                    } else {
                        coords = new Coordinate[cs.length + 1];
                        for (int j = 0; j < cs.length; j++)
                            coords[j] = new Coordinate(cs[j]);
                        coords[cs.length] = new Coordinate(cs[0]);
                    }
                } else {
                    coords = new Coordinate[4];
                    for (int i = 0; i < 4; i++)
                        coords[i] = ((Point) o).getCoordinate();
                }
                lr = (((Geometry) o).getFactory()).createLinearRing(coords);
                Polygon poly = (new GeometryFactory()).createPolygon(lr, null);
                polygons.add((Polygon) poly);
            } else {
                statusHandler.handle(Priority.WARN,
                        "Unprocessed Geometry object: "
                                + o.getClass().getName());
            }
        }
        if (mp == null && polygons.size() == 0)
            return null;
        if (polygons.size() > 0) {
            Polygon[] p = polygons.toArray(new Polygon[0]);
            if (mp != null)
                mp = (MultiPolygon) mp.union(new MultiPolygon(p, gc
                        .getFactory()));
            else
                mp = new MultiPolygon(p, gc.getFactory());
        }
        return mp;
    }

    /**
     * Simplifies the overall geometries using a single collection to preserve
     * boundaries. Geometries are updated in place.
     * 
     * @param results
     */
    private static void topologySimplifyQueryResults(
            SpatialQueryResult[] results) {
        GeometryFactory gf = new GeometryFactory();
        Geometry[] geoms = new Geometry[results.length];
        for (int i = 0; i < results.length; i++) {
            geoms[i] = results[i].geometry;
        }

        GeometryCollection geoColl = gf.createGeometryCollection(geoms);
        Geometry simpGeo = TopologyPreservingSimplifier.simplify(geoColl,
                0.0001);
        if (simpGeo instanceof GeometryCollection) {
            GeometryCollection simpGeoColl = (GeometryCollection) simpGeo;
            int numGeoms = simpGeoColl.getNumGeometries();
            if (numGeoms == geoms.length) {
                for (int i = 0; i < numGeoms; i++) {
                    results[i].geometry = simpGeoColl.getGeometryN(i);
                }
            } else {
                statusHandler
                        .handle(Priority.WARN,
                                "Error in simplifying geometries.  Geometry simplification returned a different number of geometries. Expected: "
                                        + geoms.length
                                        + ", received: "
                                        + numGeoms);
            }
        } else {
            statusHandler
                    .handle(Priority.WARN,
                            "Error in simplifying geometries.  Geometry simplification returned an unhandled class.  Expected: "
                                    + GeometryCollection.class.getName()
                                    + ", received: "
                                    + simpGeo.getClass().getName());
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
    private static GeospatialData[] queryTimeZones(GeospatialMetadata metaData,
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
    private static GeospatialData[] queryParentAreas(
            GeospatialMetadata metaData, Geometry hull) throws SpatialException {
        GeospatialData[] rval = null;
        String parentAreaSource = metaData.getParentAreaSource();
        if (parentAreaSource != null) {
            SpatialQueryResult[] parentRegionFeatures = SpatialQueryFactory
                    .create().query(
                            parentAreaSource,
                            new String[] { metaData.getAreaNotationField(),
                                    metaData.getParentAreaField() }, hull,
                            null, false, SearchMode.INTERSECTS);
            topologySimplifyQueryResults(parentRegionFeatures);

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

    private static void persistGeoData(String site,
            Map<GeospatialMetadata, GeospatialTime> times,
            GeospatialTime curTime, GeospatialDataSet geoData)
            throws SerializationException, LocalizationException, JAXBException {
        String fileName = generateGeoDataFilename(curTime.getMetaData());

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
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
    }

    private static void deleteGeomFiles(String site, GeospatialTime time) {
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

    private static final String generateGeoDataFilename(
            GeospatialMetadata metaData) {
        return metaData.getAreaSource() + "_" + metaData.hashCode() + ".bin";
    }
}
