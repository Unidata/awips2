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
import java.util.Map.Entry;

import jep.JepException;

import org.geotools.geometry.jts.JTS;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.geometry.BoundingBox;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.exception.MissingLocalMapsException;
import com.raytheon.edex.plugin.gfe.reference.DbShapeSource.ShapeType;
import com.raytheon.edex.plugin.gfe.textproducts.AreaDictionaryMaker;
import com.raytheon.edex.plugin.gfe.textproducts.CombinationsFileMaker;
import com.raytheon.edex.plugin.gfe.textproducts.Configurator;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleData;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.core.EDEXUtil;
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
 * Sep 18, 2012     #1091   randerso    Changed to use Maps.py and localMaps.py
 * Mar 14, 2013 1794        djohnson    Consolidate common FilenameFilter implementations.
 * Mar 28, 2013     #1837   dgilling    Better error reporting if a map table
 *                                      from localMaps.py could not be found,
 *                                      warnings clean up.
 * Sep 30, 2013     #2361   njensen     Use JAXBManager for XML
 * Jan 21, 2014     #2720   randerso    Improve efficiency of merging polygons in edit area generation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class MapManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapManager.class);

    private static final String EDIT_AREAS_DIR = FileUtil.join("gfe",
            "editAreas");

    private static final String EDIT_AREA_GROUPS_DIR = FileUtil.join("gfe",
            "editAreaGroups");

    private static final String SAMPLE_SETS_DIR = FileUtil.join("gfe",
            "sampleSets");

    private IFPServerConfig _config;

    private List<String> _mapErrors;

    private final Map<String, List<String>> editAreaMap = new HashMap<String, List<String>>();

    private final Map<String, Map<String, Object>> editAreaAttrs = new HashMap<String, Map<String, Object>>();

    private final List<String> iscMarkersID = new ArrayList<String>();

    private final List<Coordinate> iscMarkers = new ArrayList<Coordinate>();

    private final String commonStaticConfigDir;

    private final String edexStaticBaseDir;

    private final String edexStaticConfigDir;

    private final String edexStaticSiteDir;

    private static final double EA_EQ_TOLERANCE = 0.0005;

    private PythonEval pyScript;

    public MapManager(IFPServerConfig serverConfig) {
        this(serverConfig, null);
    }

    @SuppressWarnings("unchecked")
    public MapManager(IFPServerConfig serverConfig, String dir) {
        try {
            _config = serverConfig;

            String siteId = _config.getSiteID().get(0);
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext edexStaticBase = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
            this.edexStaticBaseDir = pathMgr.getFile(edexStaticBase, ".")
                    .getAbsolutePath();
            LocalizationContext edexStaticConfig = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.CONFIGURED);
            this.edexStaticConfigDir = pathMgr.getFile(edexStaticConfig, ".")
                    .getAbsolutePath();
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

                this.commonStaticConfigDir = pathMgr.getFile(
                        commonStaticConfig, ".").getAbsolutePath();
            }

            _mapErrors = new ArrayList<String>();

            long t0 = System.currentTimeMillis();
            statusHandler.info("MapManager " + _config.getSiteID().get(0)
                    + " started.");

            String includePath = PyUtil.buildJepIncludePath(true,
                    GfePyIncludeUtil.getGfeConfigIncludePath(siteId),
                    FileUtil.join(edexStaticBaseDir, "gfe"),
                    GfePyIncludeUtil.getCommonPythonIncludePath());

            List<DbShapeSource> maps = null;
            pyScript = null;
            try {
                pyScript = new PythonEval(includePath,
                        MapManager.class.getClassLoader());
                pyScript.eval("from Maps import *");
                Object results = pyScript.execute("getMaps", null);
                maps = (List<DbShapeSource>) results;
            } catch (JepException e) {
                statusHandler.error("Error getting maps", e);
            }

            boolean needUpdate = true;
            needUpdate = updateNeeded(maps,
                    FileUtil.join(this.commonStaticConfigDir, EDIT_AREAS_DIR));

            statusHandler.info("getMaps took: "
                    + (System.currentTimeMillis() - t0) + " ms");

            if (needUpdate) {
                statusHandler.info("Number of maps to process: " + maps.size());
                // edit area generation phase,
                // if any edit areas are out of date, then redo all
                genEditArea(maps);
            } else {
                statusHandler.info("All edit areas are up to date.");
            }

            // configure the text products
            Configurator configurator = new Configurator(_config.getSiteID()
                    .get(0));
            statusHandler.info("Configuring text products....");
            configurator.execute();

            if (needUpdate) {
                // need the attributes from the edit area step to be able to do
                // this right
                String site = _config.getSiteID().get(0);
                new CombinationsFileMaker().genCombinationsFiles(site,
                        editAreaMap);
                new AreaDictionaryMaker()
                        .genAreaDictionary(site, editAreaAttrs);
            }

            statusHandler.info("MapManager ready.");
            long t1 = System.currentTimeMillis();
            statusHandler.info("MapCreation time: " + (t1 - t0) + " ms");
        } finally {
            if (pyScript != null) {
                pyScript.dispose();
            }
        }
    }

    /**
     * @param maps
     */
    private boolean updateNeeded(List<DbShapeSource> maps,
            final String directory) {
        // calc newest file inside maps.directory()
        long newestSource = Long.MIN_VALUE;
        List<DbShapeSource> failedMaps = new ArrayList<DbShapeSource>();
        for (DbShapeSource map : maps) {
            try {
                newestSource = Math.max(newestSource, map.getLastUpdated()
                        .getTime());
            } catch (MissingLocalMapsException e) {
                reportMissingLocalMap(map, "retrieving last update time", e);
                failedMaps.add(map);
            }
        }
        maps.removeAll(failedMaps);

        // Determine time of last modification of Maps.py, serverConfig,
        // localConfig, localMaps, and siteConfig.
        String baseConfigDir = FileUtil
                .join(edexStaticBaseDir, "config", "gfe");
        String siteConfigDir = FileUtil
                .join(edexStaticSiteDir, "config", "gfe");

        File file = new File(baseConfigDir, "Maps.py");
        newestSource = Math.max(newestSource, file.lastModified());

        file = new File(baseConfigDir, "serverConfig.py");
        newestSource = Math.max(newestSource, file.lastModified());

        file = new File(siteConfigDir, "siteConfig.py");
        newestSource = Math.max(newestSource, file.lastModified());

        file = new File(siteConfigDir, "localConfig.py");
        if (file.exists()) {
            newestSource = Math.max(newestSource, file.lastModified());
        }

        // special case check for localMaps going away
        // if the tag file exists, then localMaps was previously used
        file = new File(siteConfigDir, "localMaps.py");
        File localMapsTag = new File(FileUtil.join(this.edexStaticConfigDir,
                "config", "gfe", "usingLocalMaps"));
        if (file.exists()) {
            newestSource = Math.max(newestSource, file.lastModified());
            localMapsTag.mkdirs();
        } else if (localMapsTag.exists()) {
            localMapsTag.delete();
            newestSource = System.currentTimeMillis();
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
        }

        return (newestSource > oldestEditArea);
    }

    private void genEditArea(List<DbShapeSource> maps) {
        long t0 = System.currentTimeMillis();

        statusHandler.info("Edit Area generation phase");

        @SuppressWarnings("unused")
        WsId fakeBase = null;
        try {
            fakeBase = new WsId(InetAddress.getLocalHost(), "BASE", "ifpServer");
        } catch (UnknownHostException e1) {
            statusHandler.error("Unable to get IP address for localhost");
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

        if (d.exists()) {
            final FilenameFilter filter = FilenameFilters
                    .byFilePrefix("ISC_Marker_Set");
            for (File file : FileUtil.listFiles(d, filter, false)) {
                file.delete();
            }
        }

        int i = 0;
        BoundingBox bounds = null;
        try {
            bounds = MapUtil.getBoundingEnvelope(this._config.dbDomain());
        } catch (Exception e1) {
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);
        }

        for (DbShapeSource m : maps) {
            m.setBoundingBox(bounds);
            try {
                m.open();
                if (m.getShapeType() != ShapeType.POLYGON) {
                    continue;
                }

                makeReferenceData(m);
            } catch (MissingLocalMapsException e) {
                String error = reportMissingLocalMap(m, "retrieving map data",
                        e);
                _mapErrors.add(error);
            } catch (Exception e) {
                String error = "********* EDIT AREA GENERATION ERROR - MakeReferenceData  *********\n"
                        + "Error in generating edit areas, map #"
                        + i
                        + " Name: "
                        + m.getDisplayName()
                        + " Basename: "
                        + m.getTableName();
                statusHandler.error(error, e);
                _mapErrors.add(error);
            } finally {
                try {
                    m.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            i++;
        }
        writeISCMarker();

        long t1 = System.currentTimeMillis();
        statusHandler.info("EditArea generation time: " + (t1 - t0) + " ms");
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
                if ((index != -1)
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
                        SampleData.getJAXBManager().marshalToXmlFile(sd,
                                path.getAbsolutePath());
                    } catch (Exception e) {
                        statusHandler.error(
                                "Error writing ISC Markers to file "
                                        + path.getAbsolutePath(), e);
                    }
                }
            } else {
                statusHandler.error("Unable to write to " + sampleDir);
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
    private void makeReferenceData(DbShapeSource mapDef) {
        // we skip over entries that don't have an editAreaName
        // this will throw an exception, if editAreaName not defined.
        if (!mapDef.hasEditAreaName()) {
            return;
        }

        statusHandler.debug("creating: " + mapDef.getDisplayName());
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
                    if ((n.length() == 7) && n.startsWith("ISC_")) {
                        String cwa = n.substring(4, 7);
                        if (cwa.equals(thisSite)) {
                            statusHandler
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
                                statusHandler.debug("creating: ISC_Marker_Set");
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
                        other = ReferenceData.getJAXBManager()
                                .unmarshalFromXmlFile(path);
                    } catch (Exception e) {
                        statusHandler.error("Error reading edit area file "
                                + path.getAbsolutePath(), e);
                    }

                    Geometry refG = ref.getPolygons(CoordinateType.LATLON);
                    Geometry othG = other.getPolygons(CoordinateType.LATLON);
                    refG.normalize();
                    othG.normalize();
                    if (!refG.equalsExact(othG, EA_EQ_TOLERANCE)) {
                        statusHandler.warn("Ignoring " + ref.getId().getName()
                                + " due to previous definition.");
                    }
                } else {
                    // Write the new edit area file.
                    try {
                        ReferenceData.getJAXBManager().marshalToXmlFile(ref,
                                path.getAbsolutePath());
                    } catch (Exception e) {
                        statusHandler.error("Error writing edit area to file "
                                + path.getAbsolutePath(), e);
                    }
                }
            }
        } else {
            statusHandler.error("Unable to write to " + areaDir);
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
            statusHandler.error("Error generating ISC markers for wfo:" + wfo,
                    e);
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
                statusHandler.error("Error saving edit area group: "
                        + groupName);
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
            statusHandler.error("Unable to write to " + groupDir);
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
                    statusHandler.error("Error reading group file: "
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

    /**
     * Returns a sequence of ReferenceData objects for the supplied shape
     * source. Determines the naming of the edit areas by using the supplied
     * mapconfig object's attributes.
     * 
     * @param name
     * @param mapDef
     * @return
     */
    private List<ReferenceData> createReferenceData(DbShapeSource mapDef) {
        // ServerResponse sr;
        List<ReferenceData> data = new ArrayList<ReferenceData>();

        // Module dean("DefaultEditAreaNaming");
        ArrayList<String> created = new ArrayList<String>();
        GeometryFactory gf = new GeometryFactory();
        DbShapeSource shapeSource = mapDef;
        try {
            // PathMgr pm(_config.dbBaseDirectory(), "MAPS");
            // String directory = pm.writePath(AccessLevel.baseName(), "");

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

            Map<String, Geometry> tempData = new HashMap<String, Geometry>();
            while (shapeSource.hasNext()) {
                SimpleFeature f = shapeSource.next();
                Map<String, Object> info = shapeSource.getAttributes(f);

                if (mapDef.isFiltered()
                        && !runFilter(mapDef.getInstanceName(), info)) {
                    continue;
                }

                String editAreaName = runNamer(mapDef.getInstanceName(), info);
                Geometry tmp;

                // validate edit area name, add edit area to the dictionary
                String ean = validateEAN(editAreaName);
                if (ean.length() == 0) {
                    continue;
                }

                Geometry mp = (Geometry) f.getDefaultGeometry();
                if (mp == null) {
                    continue;
                }

                Geometry mpGrid = JTS.transform(mp, transform);
                if (!boundingGeometry.intersects(mpGrid)) {
                    continue;
                }

                // handle append case
                tmp = tempData.get(ean);
                if (tmp != null) {
                    // Combine multiple geometries into a geometry collection
                    mp = gf.buildGeometry(Arrays.asList(mp, tmp));
                }
                // handle new case
                else {
                    created.add(ean);
                    editAreaAttrs.put(ean, info);
                }

                tempData.put(ean, mp);
            }

            for (Entry<String, Geometry> entry : tempData.entrySet()) {
                String ean = entry.getKey();
                Geometry mp = entry.getValue();

                // Compute buffer(0.0) to clean up geometry issues
                mp = mp.buffer(0.0);

                MultiPolygon polygons;
                if (mp instanceof MultiPolygon) {
                    polygons = (MultiPolygon) mp;
                } else if (mp instanceof Polygon) {
                    polygons = gf
                            .createMultiPolygon(new Polygon[] { (Polygon) mp });
                } else {
                    String error = "Table: " + shapeSource.getTableName()
                            + " edit area:" + ean
                            + " contains geometry of type "
                            + mp.getClass().getSimpleName()
                            + " Creating empty polygon";
                    statusHandler.error(error);
                    polygons = gf.createMultiPolygon(new Polygon[] {});
                }

                if (!polygons.isValid()) {
                    String error = "Table: "
                            + shapeSource.getTableName()
                            + " edit area:"
                            + ean
                            + " contains invalid polygons. This edit area will be skipped.";
                    for (int i = 0; i < polygons.getNumGeometries(); i++) {
                        Geometry g = polygons.getGeometryN(i);
                        if (!g.isValid()) {
                            error += "\n" + g;
                        }
                    }
                    statusHandler.error(error);
                    continue;
                }

                // transfer dictionary values to Seq values
                data.add(new ReferenceData(_config.dbDomain(), new ReferenceID(
                        ean), polygons, CoordinateType.LATLON));
            }

            tempData.clear();
        } catch (Exception e) {
            String error = "********* EDIT AREA GENERATION ERROR - Create Reference Data *********\n"
                    + "Error in generating edit areas from maps for map "
                    + mapDef.getDisplayName();
            statusHandler.error(error, e);
            _mapErrors.add(error);
        }

        statusHandler.debug("EAs: " + created);
        editAreaMap.put(mapDef.getDisplayName(), created);
        return data;
    }

    private String runNamer(String instance, Map<String, Object> info) {
        String editAreaName = "";

        Map<String, Object> args = new HashMap<String, Object>();
        args.put("instance", instance);
        args.put("info", info);
        try {
            editAreaName = (String) pyScript.execute("runNamer", args);
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception getting editAreaName for " + instance, e);
        }

        return editAreaName;
    }

    private boolean runFilter(String instance, Map<String, Object> info) {
        boolean result = false;

        Map<String, Object> args = new HashMap<String, Object>();
        args.put("instance", instance);
        args.put("info", info);
        try {
            result = (Boolean) pyScript.execute("runFilter", args);
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM, "Exception filtering "
                    + instance, e);
        }

        return result;
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
            if (!Character.isLetterOrDigit(s.charAt(i)) && (s.charAt(i) != '_')) {
                s = s.substring(0, i) + s.substring(i + 1);
            }
        }

        // ensure 1st character is not a number. If a number, preprend.
        if ((s.length() > 0) && Character.isDigit(s.charAt(0))) {
            s = "ea" + s;
        }

        return s;
    }

    private String reportMissingLocalMap(DbShapeSource missingMap,
            String operation, MissingLocalMapsException e) {
        String errorLog = "Error in " + operation + " for map named ["
                + missingMap.getDisplayName() + "]: Could not find table ["
                + missingMap.getTableName() + "] in maps database.";
        statusHandler.error(errorLog, e);

        String errorUser = errorLog
                + " Edit areas for this map will not be generated."
                + " Check site ["
                + _config.getSiteID().get(0)
                + "] localMaps.py configuration and verify all necessary shape files have been imported.";
        EDEXUtil.sendMessageAlertViz(Priority.ERROR,
                "com.raytheon.edex.plugin.gfe", "GFE", "GFE", errorUser,
                errorUser, null);

        return errorLog;
    }
}
