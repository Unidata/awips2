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
package com.raytheon.edex.plugin.gfe.reference;

import java.awt.Point;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.jts.JTS;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.reference.ShapeFile.IEditAreaNamer;
import com.raytheon.edex.plugin.gfe.reference.ShapeFile.IMapBackgroundFilter;
import com.raytheon.edex.plugin.gfe.reference.ShapeFile.ShapeType;
import com.raytheon.edex.plugin.gfe.textproducts.AreaDictionaryMaker;
import com.raytheon.edex.plugin.gfe.textproducts.CombinationsFileMaker;
import com.raytheon.edex.plugin.gfe.textproducts.Configurator;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleData;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.operation.buffer.BufferParameters;
import com.vividsolutions.jts.simplify.TopologyPreservingSimplifier;

/**
 * Creates edit areas for the currently selected WFO
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 10, 2008		#1075	randerso	Initial creation
 * Jun 25, 2008     #1210   randerso    Modified to get directories from UtilityContext
 * Oct 13, 2008     #1607   njensen     Added genCombinationsFiles()
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class MapManager {
    private static final Log theLogger = LogFactory.getLog(MapManager.class);

    private static final String EDIT_AREAS_DIR = FileUtil.join("gfe",
            "editAreas");

    private static final String EDIT_AREA_GROUPS_DIR = FileUtil.join("gfe",
            "editAreaGroups");

    private static final String SAMPLE_SETS_DIR = FileUtil.join("gfe",
            "sampleSets");

    protected static final String EDIT_AREA_GEN_TASK = "GfeEditAreaGeneration";

    private IFPServerConfig _config;

    private List<String> _mapErrors;

    private List<MapID> _ids;

    private Map<String, ArrayList<String>> editAreaMap = new HashMap<String, ArrayList<String>>();

    private Map<String, Map<String, String>> editAreaAttrs = new HashMap<String, Map<String, String>>();

    private List<String> iscMarkersID = new ArrayList<String>();

    private List<Coordinate> iscMarkers = new ArrayList<Coordinate>();

    private String commonStaticConfigDir;

    private String edexStaticSiteDir;

    private static final double EA_EQ_TOLERANCE = 0.0005;

    public MapManager(IFPServerConfig serverConfig) {
        this(serverConfig, null);
    }

    public MapManager(IFPServerConfig serverConfig, String dir) {
        _config = serverConfig;

        String siteId = _config.getSiteID().get(0);
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticSite = pathMgr.getContextForSite(
                LocalizationType.EDEX_STATIC, siteId);
        this.edexStaticSiteDir = pathMgr.getFile(edexStaticSite, ".")
                .getAbsolutePath();

        if (dir != null) {
            this.commonStaticConfigDir = dir;
        } else {
            LocalizationContext commonStaticConfig = pathMgr.getContext(
                    LocalizationContext.LocalizationType.COMMON_STATIC,
                    LocalizationContext.LocalizationLevel.CONFIGURED);
            commonStaticConfig.setContextName(siteId);

            this.commonStaticConfigDir = pathMgr.getFile(commonStaticConfig,
                    ".").getAbsolutePath();
        }

        _mapErrors = new ArrayList<String>();
        _ids = new ArrayList<MapID>();

        long t0 = System.currentTimeMillis();
        theLogger
                .info("MapManager " + _config.getSiteID().get(0) + " started.");

        // try{
        // Module mapMod("Maps");
        ShapeFile[] maps = Maps.getMaps(_config.getSiteID().get(0),
                Maps.GetMode.UNCONDITIONAL);

        boolean anyCached;
        // anyCached = maps != null;
        anyCached = cacheNeeded(maps,
                FileUtil.join(this.commonStaticConfigDir, EDIT_AREAS_DIR));

        theLogger.info("getMaps took: " + (System.currentTimeMillis() - t0)
                + " ms");

        // map == null tells us the shape files have not changed, skip this
        if (anyCached) {
            theLogger.info("Number of maps to process: " + maps.length);
            // edit area generation phase, if any maps are outdated, then redo
            // all
            genEditArea(maps);
            ClusterLockUtils.lock(EDIT_AREA_GEN_TASK, _config.getSiteID()
                    .get(0), 0, false);
        } else {
            theLogger.info("All edit areas are up to date.");
        }

        // adding valid maps to list
        // addValidMaps(maps);

        // configure the text products
        Configurator configurator = new Configurator(_config.getSiteID().get(0));
        theLogger.info("Configuring text products....");
        configurator.execute();

        if (anyCached) {
            // need the attributes from the edit area step to be able to do this
            // right
            String site = _config.getSiteID().get(0);
            new CombinationsFileMaker().genCombinationsFiles(site, editAreaMap);
            new AreaDictionaryMaker().genAreaDictionary(site, editAreaAttrs);
        }

        // configure products (formatter templates)
        // try
        // {
        // Module txtConfigMod("configureTextProducts");
        // String mode = "CREATE";
        // txtConfigMod.getAttr("configureTextProducts").call(
        // _config.baseDir().stringPtr(),
        // _config.siteID()[0].stringPtr(),
        // mode.stringPtr(), _altA2Afile.stringPtr());
        // }
        // catch (Error &e)
        // {
        // std::ostringstream o;
        // o + "Error in configuring text products: " + e + std::endl
        // + std::ends;
        // e.clear();
        // String error =
        // "********* TEXT PRODUCT CONFIGURATION ERROR *********\n";
        // error += o.str().c_str();
        // theLogger.error(error);
        // _mapErrors.append(error);
        // }
        //
        //
        // // configure products (dictionaries and combos)
        // try
        // {
        // Dictionary attD;
        // for (int i = 0; i < _ids.length(); i++)
        // attD.add(_ids[i].name().stringPtr(),
        // getAttributes(_ids[i]));
        // Module txtConfigMod("createComboAreaDict");
        // txtConfigMod.getAttr("configureTextComboAreaDict").call(attD,
        // _config.baseDir().stringPtr(),
        // _config.siteID()[0].stringPtr());
        // }
        // catch (Error &e)
        // {
        // std::ostringstream o;
        // o + "Error in configuring combinations/areaDictionary: "
        // + e + std::endl + std::ends;
        // e.clear();
        // String error =
        // "********* COMBINATIONS/AREADICTIONARY CONFIGURATION ERROR
        // *********\n";
        // error += o.str().c_str();
        // theLogger.error(error);
        // _mapErrors.append(error);
        // }
        //
        // // determine unused shapefiles
        // theLogger.debug("Unused Shapefiles: " + unusedShapefiles(basename)
        // );
        //
        // mapMod.unload();
        // }
        // catch (Error &e)
        // {
        // theLogger.error("Error in Maps.py, localMaps.py, MapFiles.py, "
        // + "or localMapFiles.py file: ", e);
        // e.clear();
        // }

        theLogger.info("MapManager ready.");
        long t1 = System.currentTimeMillis();
        theLogger.info("MapCreation time: " + (t1 - t0) + " ms");
    }

    /**
     * @param maps
     */
    @SuppressWarnings("unused")
    private void addValidMaps(ShapeFile[] maps) {
        // PathMgr pm(_config.dbBaseDirectory(), "MAPS");
        // String directory(pm.writePath(AccessLevel::baseName(), ""));
        for (int i = 0; i < maps.length; i++) {
            try {
                // if cache file has no records, then don't add to list
                // String cacheName = directory + maps[i].getName();
                // ShapeFile shapeFile = new ShapeFile(cacheName);
                ShapeFile shapeFile = maps[i];
                shapeFile.open();
                int nrecs = shapeFile.getFeatureCount();
                theLogger.debug("RECORDS for: " + maps[i].getDisplayName()
                        + " " + nrecs);
                shapeFile.close();

                if (nrecs > 0) {
                    _ids.add(new MapID(maps[i].getDisplayName(), maps[i]
                            .getShapeType(), maps[i].getFile()
                            .getAbsolutePath()));
                } else {
                    theLogger.debug("Map [" + maps[i].getDisplayName()
                            + "] contains no records.");
                }
            } catch (Exception e) {
                String error = "********* MAP BACKGROUND GENERATION ERROR - Cached Shapefile *********\n"
                        + "Error in generating map #"
                        + i
                        + " Name: "
                        + maps[i].getDisplayName()
                        + " Basename: "
                        + maps[i].getFile();
                theLogger.error(error, e);
                _mapErrors.add(error);
            }
        }
    }

    /**
     * Searches the parent directory of a provided list of shape files to
     * determine whether or not they contain a file that is newer than any files
     * in a specified directory.
     * 
     * @param maps
     *            An array of shape files.
     * @param directory
     *            A directory containing the resultant edit areas from the shape
     *            files.
     * @return True, if any file in the parent folder of any of the shape files
     *         is newer than anything in the specified directory. Else, false.
     */
    @SuppressWarnings("unused")
    private boolean cacheNeeded(ShapeFile[] maps, final String directory) {
        // calc newest file inside maps.directory()
        long newestShapeFile = Long.MIN_VALUE;
        for (ShapeFile map : maps) {
            File shapePath = map.getFile().getParentFile().getAbsoluteFile();
            File[] shapeFiles = shapePath.listFiles(new FileFilter() {
                @Override
                public boolean accept(File file) {
                    return file.isFile();
                }
            });

            if (shapeFiles != null) {
                for (File file : shapeFiles) {
                    newestShapeFile = Math.max(newestShapeFile,
                            file.lastModified());
                }
            }
        }

        // also check for siteConfig or localConfig changes
        String configDir = FileUtil.join(edexStaticSiteDir, "config", "gfe");
        File file = new File(configDir, "siteConfig.py");
        newestShapeFile = Math.max(newestShapeFile, file.lastModified());
        file = new File(configDir, "localConfig.py");
        if (file.exists()) {
            newestShapeFile = Math.max(newestShapeFile, file.lastModified());
        }

        // calc oldest file in directory
        long oldestEditArea = Long.MIN_VALUE;
        File[] editAreaFiles = null;
        File dir = new File(directory);
        if (dir.exists()) {
            editAreaFiles = dir.listFiles(new FileFilter() {
                @Override
                public boolean accept(File file) {
                    return file.isFile();
                }
            });
        }

        if (editAreaFiles != null) {
            if (editAreaFiles.length > 0) {
                Arrays.sort(editAreaFiles, new Comparator<File>() {
                    @Override
                    public int compare(File f1, File f2) {
                        return Long.valueOf(f1.lastModified()).compareTo(
                                f2.lastModified());
                    }
                });

                oldestEditArea = editAreaFiles[0].lastModified();
            }

            // FIXME use last execution time instead
            // ClusterTask task = ClusterLockUtils.lookupLock(
            // MapManager.EDIT_AREA_GEN_TASK, cwa);
            // oldestEditArea = task.getLastExecution();
        }

        return (newestShapeFile > oldestEditArea);
    }

    private void genEditArea(ShapeFile[] maps) {
        long t0 = System.currentTimeMillis();

        theLogger.info("Edit Area generation phase");

        @SuppressWarnings("unused")
        WsId fakeBase = null;
        try {
            fakeBase = new WsId(InetAddress.getLocalHost(), "BASE", "ifpServer");
        } catch (UnknownHostException e1) {
            theLogger.error("Unable to get IP address for localhost");
        }

        // _refMgr->deleteAllReferenceData(fakeBase);
        // _sampleMgr->deleteData(fakeBase, SampleID("ISC_Marker_Set"));
        // _textMgr->deleteAllTextData(fakeBase, "EditAreaGroup");
        File d = new File(FileUtil.join(commonStaticConfigDir, EDIT_AREAS_DIR));
        if (d.exists()) {
            FileUtil.deleteDir(d);
        }

        d = new File(FileUtil.join(commonStaticConfigDir, EDIT_AREA_GROUPS_DIR));
        if (d.exists()) {
            FileUtil.deleteDir(d);
        }

        d = new File(FileUtil.join(commonStaticConfigDir, SAMPLE_SETS_DIR));
        FilenameFilter filter = new FilenameFilter() {

            @Override
            public boolean accept(File dir, String name) {
                return name.trim().startsWith("ISC_Marker_Set");
            }
        };
        if (d.exists()) {
            for (File file : FileUtil.listFiles(d, filter, false)) {
                file.delete();
            }
        }

        for (int i = 0; i < maps.length; i++) {
            ShapeFile m = maps[i];
            try {
                if (m.getShapeType() != ShapeType.POLYGON) {
                    continue;
                }

                makeReferenceData(m);
            } catch (Exception e) {
                String error = "********* EDIT AREA GENERATION ERROR - MakeReferenceData  *********\n"
                        + "Error in generating edit areas, map #"
                        + i
                        + " Name: "
                        + m.getDisplayName()
                        + " Basename: "
                        + m.getFile();
                theLogger.error(error, e);
                _mapErrors.add(error);
            }
        }
        writeISCMarker();

        long t1 = System.currentTimeMillis();
        theLogger.info("EditArea generation time: " + (t1 - t0) + " ms");
    }

    /**
     * 
     */
    private void writeISCMarker() {

        // find all office types
        List<String> foundOfficeTypes = new ArrayList<String>();
        for (int i = 0; i < this.iscMarkersID.size(); i++) {
            int index = this._config.allSites().indexOf(iscMarkersID.get(i));
            if (index != -1) {
                String officeType = _config.officeTypes().get(index);
                if (!foundOfficeTypes.contains(officeType)) {
                    foundOfficeTypes.add(officeType);
                }
            }
        }

        // create each sample set based on office type
        for (int i = 0; i < foundOfficeTypes.size(); i++) {
            List<Coordinate> values = new ArrayList<Coordinate>();
            for (int j = 0; j < iscMarkersID.size(); j++) {
                int index = _config.allSites().indexOf(iscMarkersID.get(j));
                if (index != -1
                        && _config.officeTypes().get(index)
                                .equals(foundOfficeTypes.get(i))) {
                    values.add(iscMarkers.get(j));
                }
            }

            String markerSetName = "ISC_Marker_Set_" + foundOfficeTypes.get(i);
            SampleId sampleId = new SampleId(markerSetName);
            SampleData sd = new SampleData(new SampleId(markerSetName), values);

            File sampleDir = new File(FileUtil.join(commonStaticConfigDir,
                    SAMPLE_SETS_DIR));
            if (!sampleDir.exists()) {
                sampleDir.mkdirs();
            }

            if (sampleDir.isDirectory() && sampleDir.canWrite()) {
                File path = new File(FileUtil.join(sampleDir.getAbsolutePath(),
                        sampleId.getName() + ".xml"));
                if (!path.exists()) {
                    try {
                        SerializationUtil.jaxbMarshalToXmlFile(sd,
                                path.getAbsolutePath());
                    } catch (Exception e) {
                        theLogger.error("Error writing ISC Markers to file "
                                + path.getAbsolutePath(), e);
                    }
                }
            } else {
                theLogger.error("Unable to write to " + sampleDir);
            }
        }
    }

    /**
     * Based on the supplied map configuration, creates edit areas, saves them,
     * and updates group names. Handles special creation for ISC edit areas,
     * edit area groups, and sample sets.
     * 
     * @param m
     * @param string
     */
    private void makeReferenceData(ShapeFile mapDef) {
        // we skip over entries that don't have an editAreaName
        // this will throw an exception, if editAreaName not defined.
        Object ean = mapDef.getEditAreaName();
        if (ean == null) {
            return;
        }

        theLogger.debug("creating: " + mapDef.getDisplayName());
        List<ReferenceData> data = createReferenceData(mapDef);
        if (data.size() == 0) {
            return;
        }

        // save the reference data
        saveEditAreas(data);

        // handle the group list, if specified
        String groupName = mapDef.getGroupName();
        if (groupName != null) {
            List<String> list = new ArrayList<String>();
            for (int i = 0; i < data.size(); i++) {
                list.add(data.get(i).getId().getName());
            }

            // Need some special edit areas for ISC
            // Create ISC_Tool_Area and ISC_Send_Area if ISC_CWA
            List<String> knownSites = _config.allSites();
            boolean anySites = false;
            if (groupName.equals("ISC")) {
                String thisSite = _config.getSiteID().get(0);
                for (int i = 0; i < data.size(); i++) {
                    String n = data.get(i).getId().getName();
                    if (n.length() == 7 && n.startsWith("ISC_")) {
                        String cwa = n.substring(4, 7);
                        if (cwa.equals(thisSite)) {
                            theLogger
                                    .debug("creating: ISC_Tool_Area and ISC_Send_Area"
                                            + " from "
                                            + data.get(i).getId().getName());

                            List<ReferenceData> areas = new ArrayList<ReferenceData>();
                            ReferenceData swath = createSwathArea(
                                    "ISC_Tool_Area", data.get(i), 4);
                            if (swath != null) {
                                areas.add(swath);
                                list.add(swath.getId().getName());
                            }

                            ReferenceData extend = new ReferenceData(
                                    data.get(i));
                            extend.setId(new ReferenceID("ISC_Send_Area"));
                            if (swath != null) {
                                extend.orEquals(swath);
                            }
                            areas.add(extend);
                            list.add(extend.getId().getName());

                            saveEditAreas(areas);
                        }

                        // Need some special sample sets for ISC
                        // Create ISC_Marker_Set if any CWA, use ISC_cwa areas
                        if (knownSites.contains(cwa)) {
                            if (!anySites) {
                                anySites = true;
                                theLogger.debug("creating: ISC_Marker_Set");
                            }
                            createISCMarker(cwa, data.get(i));
                        }
                    }
                }
            }
            saveGroupList(groupName, list);
        }
    }

    /**
     * Save the edit areas in data to the file system using serialization.
     * 
     * @param data
     *            The ReferenceDatas to save as edit areas.
     */
    private void saveEditAreas(List<ReferenceData> data) {
        File areaDir = new File(FileUtil.join(this.commonStaticConfigDir,
                EDIT_AREAS_DIR));
        if (!areaDir.exists()) {
            areaDir.mkdirs();
        }
        if (areaDir.isDirectory() && areaDir.canWrite()) {
            for (ReferenceData ref : data) {
                ref.getPolygons(CoordinateType.LATLON);
                File path = new File(FileUtil.join(areaDir.getAbsolutePath(),
                        ref.getId().getName() + ".xml"));
                if (path.exists()) {
                    // Don't write to file. If the new one is different from the
                    // old one, write a warning to the log.
                    ReferenceData other = null;
                    try {
                        other = (ReferenceData) SerializationUtil
                                .jaxbUnmarshalFromXmlFile(path);
                    } catch (Exception e) {
                        theLogger.error(
                                "Error reading edit area file "
                                        + path.getAbsolutePath(), e);
                    }

                    Geometry refG = ref.getPolygons(CoordinateType.LATLON);
                    Geometry othG = other.getPolygons(CoordinateType.LATLON);
                    refG.normalize();
                    othG.normalize();
                    if (!refG.equalsExact(othG, EA_EQ_TOLERANCE)) {
                        theLogger.warn("Ignoring " + ref.getId().getName()
                                + " due to previous definition.");
                    }
                } else {
                    // Write the new edit area file.
                    try {
                        SerializationUtil.jaxbMarshalToXmlFile(ref,
                                path.getAbsolutePath());
                    } catch (Exception e) {
                        theLogger.error("Error writing edit area to file "
                                + path.getAbsolutePath(), e);
                    }
                }
            }
        } else {
            theLogger.error("Unable to write to " + areaDir);
        }

    }

    /**
     * Calculates the markers and saves them up in _iscMarkers, given the wfo id
     * and a reference data for that wfo.
     * 
     * @param wfo
     * @param area
     */
    private void createISCMarker(String wfo, ReferenceData area) {
        try {
            // get the edit area as a Grid2DBit
            GridLocation gridLoc = _config.dbDomain();
            ReferenceData awipsArea = new ReferenceData(area);
            Grid2DBit bits = awipsArea.getGrid();

            // determine the center of the edit area, and then the center cell
            Coordinate centerCell = area.overallDomain(CoordinateType.GRID)
                    .centre();

            // if center point not in grid, warp it to closest
            if (centerCell.x < 0) {
                centerCell.x = 0;
            }
            if (centerCell.y < 0) {
                centerCell.y = 0;
            }
            if (centerCell.x >= bits.getXdim()) {
                centerCell.x = bits.getXdim() - 1;
            }
            if (centerCell.y >= bits.getYdim()) {
                centerCell.y = bits.getYdim() - 1;
            }

            // get closest point to the center that is part of edit area
            Point modCenterCell = new Point();
            if (!bits.findNearestSet(new Point((int) centerCell.x,
                    (int) centerCell.y), modCenterCell)) {
                return; // nothing found(should never see this)
            }

            // calculate the lat/lon 'center' point for this WFO
            Coordinate centerLatLon = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(modCenterCell.x, modCenterCell.y),
                    PixelOrientation.CENTER, gridLoc);
            iscMarkers.add(centerLatLon);
            iscMarkersID.add(wfo);
        } catch (Exception e) {
            theLogger.error("Error generating ISC markers for wfo:" + wfo, e);
        }

    }

    /**
     * Saves the supplied List of names in the supplied group.The names will be
     * appended to the existing list (if they are not already in the list).
     * 
     * @param groupName
     * @param l
     */
    private void saveGroupList(String groupName, List<String> list) {
        File groupDir = new File(FileUtil.join(commonStaticConfigDir,
                EDIT_AREA_GROUPS_DIR));
        if (!groupDir.exists()) {
            groupDir.mkdirs();
        }

        // load the existing list
        List<String> l = loadGroupList(groupName);

        // add names not already in the list
        for (String s : list) {
            if (!l.contains(s)) {
                l.add(s);
            }
        }

        Collections.sort(l);
        if (groupDir.isDirectory() && groupDir.canWrite()) {
            File path = new File(FileUtil.join(groupDir.getAbsolutePath(),
                    groupName + ".txt"));
            BufferedWriter out = null;
            try {
                out = new BufferedWriter(new FileWriter(path));
                out.write(Integer.toString(l.size()));
                out.write('\n');
                for (String name : l) {
                    out.write(name);
                    out.write('\n');
                }
            } catch (IOException e) {
                theLogger.error("Error saving edit area group: " + groupName);
            } finally {
                if (out != null) {
                    try {
                        out.close();
                    } catch (IOException e) {
                        // nothing to do
                    }
                }
            }
        } else {
            theLogger.error("Unable to write to " + groupDir);
        }
    }

    private List<String> loadGroupList(String groupName) {
        List<String> list = new ArrayList<String>();
        File groupDir = new File(FileUtil.join(commonStaticConfigDir,
                EDIT_AREA_GROUPS_DIR));
        if (groupDir.exists() && groupDir.isDirectory()) {
            File groupFile = new File(FileUtil.join(groupDir.getAbsolutePath(),
                    groupName + ".txt"));
            if (groupFile.exists()) {
                BufferedReader in = null;
                try {
                    in = new BufferedReader(new FileReader(groupFile));
                    String s = in.readLine();
                    int count = Integer.parseInt(s);
                    for (int i = 0; i < count; i++) {
                        String area = in.readLine();
                        list.add(area);
                    }
                } catch (Exception e) {
                    theLogger.error(
                            "Error reading group file: "
                                    + groupFile.getAbsolutePath(), e);
                } finally {
                    if (in != null) {
                        try {
                            in.close();
                        } catch (IOException e) {
                            // nothing to do
                        }
                    }
                }
            }
        }

        return list;
    }

    /**
     * Creates and saves a swath area around the given referenceData with
     * lat/lon polygons
     * 
     * @param name
     * @param refData
     * @param cells
     * @return
     */
    private ReferenceData createSwathArea(String name, ReferenceData refData,
            float cells) {
        MultiPolygon origPolygons = refData.getPolygons(CoordinateType.GRID);

        Polygon[] polygons = new Polygon[origPolygons.getNumGeometries()];
        for (int i = 0; i < origPolygons.getNumGeometries(); i++) {
            LineString extRing = ((Polygon) origPolygons.getGeometryN(i))
                    .getExteriorRing();

            extRing = (LinearRing) TopologyPreservingSimplifier.simplify(
                    extRing, 0.5);

            polygons[i] = (Polygon) extRing.buffer(cells / 2.0, 8,
                    BufferParameters.CAP_SQUARE);
        }

        GeometryFactory gf = new GeometryFactory();
        Geometry geom = gf.createGeometryCollection(polygons).union();
        MultiPolygon mp;
        if (geom.getNumGeometries() > 1) {
            mp = (MultiPolygon) geom;
        } else if (geom.getNumGeometries() > 0) {
            mp = gf.createMultiPolygon(new Polygon[] { (Polygon) geom
                    .getGeometryN(1) });
        } else {
            mp = gf.createMultiPolygon(new Polygon[0]);
        }
        ReferenceData swath = new ReferenceData(refData.getGloc(),
                new ReferenceID(name), mp, CoordinateType.GRID);
        return swath;
    }

    private String defaultEditAreaNaming(Map<String, String> info,
            Object eanDefinition) {
        // simple case, the edit area name definition is the attribute key
        if (eanDefinition instanceof String) {
            String ean = (String) eanDefinition;
            if (info.containsKey(ean)) {
                return info.get(ean);
            } else {
                return ean;
            }

        } else if (eanDefinition instanceof String[]) {
            String s = "";
            for (String e : (String[]) eanDefinition) {
                // valid attribute
                if (info.containsKey(e)) {
                    if (s.length() == 0) {
                        s = info.get(e);
                    } else {
                        s = s + "_" + info.get(e);
                    }
                    // not valid attribute, so use definition directly
                } else {
                    if (s.length() == 0) {
                        s = e;
                    } else {
                        s = s + "_" + e;
                    }
                }
            }
            return s;

        } else {
            return "";
        }

    }

    /**
     * Returns a sequence of ReferenceData objects for the supplied shapefile
     * basename. Determines the naming of the edit areas by using the supplied
     * mapconfig object's attributes.
     * 
     * @param name
     * @param mapDef
     * @return
     */
    private List<ReferenceData> createReferenceData(ShapeFile mapDef) {
        // ServerResponse sr;
        List<ReferenceData> data = new ArrayList<ReferenceData>();

        // Module dean("DefaultEditAreaNaming");
        ArrayList<String> created = new ArrayList<String>();
        GeometryFactory gf = new GeometryFactory();
        try {
            // PathMgr pm(_config.dbBaseDirectory(), "MAPS");
            // String directory = pm.writePath(AccessLevel.baseName(), "");
            ShapeFile shapeFile = mapDef;

            IMapBackgroundFilter filter = shapeFile.getMapBackgroundFilter();

            GridLocation gloc = _config.dbDomain();
            MathTransform transform = MapUtil.getTransformFromLatLon(
                    PixelOrientation.CENTER, gloc);

            Coordinate c0 = new Coordinate(0, 0);
            Coordinate c1 = new Coordinate(gloc.getNx() - 1, 0);
            Coordinate c2 = new Coordinate(gloc.getNx() - 1, gloc.getNy() - 1);
            Coordinate c3 = new Coordinate(0, gloc.getNy() - 1);
            Polygon p = gf.createPolygon(gf.createLinearRing(new Coordinate[] {
                    c0, c1, c2, c3, c0 }), null);
            PreparedGeometry boundingGeometry = PreparedGeometryFactory
                    .prepare(p);

            shapeFile.open();
            Map<String, ReferenceData> tempData = new HashMap<String, ReferenceData>();
            while (shapeFile.hasNext()) {
                SimpleFeature f = shapeFile.next();
                Map<String, String> info = shapeFile.getAttributes(f);

                if (filter != null && !filter.filter(info)) {
                    continue;
                }

                String editAreaName;
                // functions return a string
                if (mapDef.getEditAreaName() instanceof IEditAreaNamer) {
                    editAreaName = ((IEditAreaNamer) mapDef.getEditAreaName())
                            .getEditAreaName(info);
                    // non-functions allow only a string
                } else {
                    editAreaName = defaultEditAreaNaming(info,
                            mapDef.getEditAreaName());
                }

                ReferenceData tmp;

                // validate edit area name, add edit area to the dictionary
                String ean = validateEAN(editAreaName);
                if (ean.length() == 0) {
                    continue;
                }

                Geometry mp = (Geometry) f.getDefaultGeometry();
                Geometry mpGrid = JTS.transform(mp, transform);
                if (!boundingGeometry.intersects(mpGrid)) {
                    continue;
                }

                // handle append case
                tmp = tempData.get(ean);
                if (tmp != null) {
                    mp = mp.union(tmp.getPolygons(CoordinateType.LATLON));
                    mp = mp.buffer(0.0);
                }
                // handle new case
                else {
                    created.add(ean);
                    editAreaAttrs.put(ean, info);
                }
                MultiPolygon polygons;
                if (mp instanceof MultiPolygon) {
                    polygons = (MultiPolygon) mp;
                } else if (mp instanceof Polygon) {
                    polygons = gf
                            .createMultiPolygon(new Polygon[] { (Polygon) mp });
                } else {
                    theLogger.info("Creating empty polygon");
                    polygons = gf.createMultiPolygon(new Polygon[] {});
                }

                if (!polygons.isValid()) {
                    String error = shapeFile.getFile()
                            + " contains invalid polygons.";
                    for (int i = 0; i < polygons.getNumGeometries(); i++) {
                        Geometry g = polygons.getGeometryN(i);
                        if (!g.isValid()) {
                            error += "\n" + g;
                        }
                    }
                    theLogger.error(error);
                }

                tempData.put(ean, new ReferenceData(_config.dbDomain(),
                        new ReferenceID(ean), polygons, CoordinateType.LATLON));
            }

            shapeFile.close();

            // transfer dictionary values to Seq values
            data.addAll(tempData.values());
            tempData.clear();
        } catch (Exception e) {
            String error = "********* EDIT AREA GENERATION ERROR - Create Reference Data *********\n"
                    + "Error in generating edit areas from maps for map "
                    + mapDef.getDisplayName();
            theLogger.error(error, e);
            _mapErrors.add(error);
        }

        theLogger.debug("EAs: " + created);
        editAreaMap.put(mapDef.getDisplayName(), created);
        return data;
    }

    /**
     * Validate edit area name. Changes the name as needed to ensure a valid
     * python name.
     * 
     * @param editAreaName
     * @return
     */
    private String validateEAN(String ean) {
        String s = ean;

        // strip out white space and punctuation (except _)
        for (int i = s.length() - 1; i >= 0; i--) {
            if (!Character.isLetterOrDigit(s.charAt(i)) && s.charAt(i) != '_') {
                s = s.substring(0, i) + s.substring(i + 1);
            }
        }

        // ensure 1st character is not a number. If a number, preprend.
        if (s.length() > 0 && Character.isDigit(s.charAt(0))) {
            s = "ea" + s;
        }

        return s;
    }
}
