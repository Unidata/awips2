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
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import org.geotools.geometry.jts.JTS;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;
import org.locationtech.jts.operation.buffer.BufferParameters;
import org.locationtech.jts.operation.valid.IsValidOp;
import org.locationtech.jts.simplify.TopologyPreservingSimplifier;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.geometry.BoundingBox;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.exception.MissingLocalMapsException;
import com.raytheon.edex.plugin.gfe.reference.DbShapeSource.ShapeType;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.textproducts.AreaDictionaryMaker;
import com.raytheon.edex.plugin.gfe.textproducts.CombinationsFileMaker;
import com.raytheon.edex.plugin.gfe.textproducts.Configurator;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceMgr;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleData;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.core.EDEXUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Creates edit areas for the currently selected WFO
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 02, 2015  1075     randerso  Initial creation
 * Jun 25, 2008  1210     randerso  Modified to get directories from
 *                                  UtilityContext
 * Oct 13, 2008  1607     njensen   Added genCombinationsFiles()
 * Sep 18, 2012  1091     randerso  Changed to use Maps.py and localMaps.py
 * Mar 14, 2013  1794     djohnson  Consolidate common FilenameFilter
 *                                  implementations.
 * Mar 28, 2013  1837     dgilling  Better error reporting if a map table from
 *                                  localMaps.py could not be found, warnings
 *                                  clean up.
 * Sep 30, 2013  2361     njensen   Use JAXBManager for XML
 * Jan 21, 2014  2720     randerso  Improve efficiency of merging polygons in
 *                                  edit area generation
 * Aug 27, 2014  3563     randerso  Fix issue where edit areas are regenerated
 *                                  unnecessarily
 * Oct 20, 2014  3685     randerso  Changed structure of editAreaAttrs to keep
 *                                  zones from different maps separated
 * Feb 19, 2015  4125     rjpeter   Fix jaxb performance issue
 * Apr 01, 2015  4353     dgilling  Improve logging of Geometry validation
 *                                  errors.
 * Apr 08, 2015  4383     dgilling  Change ISC_Send_Area to be union of areas
 *                                  ISC_XXX and all edit area prefixes in
 *                                  AdditionalISCRouting.
 * May 11, 2015  4259     njensen   Silence jep thread warning by closing
 *                                  pyScript earlier
 * May 21, 2015  4518     dgilling  Change ISC_Send_Area to always be union of
 *                                  at least ISC_XXX and FireWxAOR_XXX.
 * Jul 18, 2016  5747     dgilling  Move edex_static to common_static.
 * Sep 12, 2016  5861     randerso  Change IFPServerConfig.getSiteID() to return a single value
 *                                  instead of a list containing only one value.
 * Oct 20, 2016  5953     randerso  Move usingLocalMaps flag directory to
 *                                  gfe/config
 * Mar 29, 2017  5861     randerso  Improve handling of Maps.py exceptions
 * Jun 12, 2017  6298     mapeters  Update references to refactored ReferenceMgr
 * Jul 31, 2017  6342     randerso  Get ReferenceMgr from IFPServer. Code cleanup.
 * Jun 03, 2019  7852     dgilling  Update code for jep 3.8.
 * Oct 23, 2019  7944     dgilling  Fix Comparator used by updateNeeded.
 * Feb 18, 2020  20542    ryu       Regenerate edit areas (and related files) if 
 *                                  previous attempt did not complete.
 *
 * </pre>
 *
 * @author randerso
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

    private static final String EDITAREA_GEN_TAGFILE = ".EditArea_Generation_Incomplete";

    private IFPServer ifpServer;

    private IFPServerConfig config;

    private List<String> _mapErrors;

    private final Map<String, List<String>> editAreaMap = new HashMap<>();

    private final Map<String, List<Map<String, Object>>> editAreaAttrs = new HashMap<>();

    private final List<String> iscMarkersID = new ArrayList<>();

    private final List<Coordinate> iscMarkers = new ArrayList<>();

    private final String commonStaticConfigDir;

    private final String baseDir;

    private final String configuredDir;

    private final String siteDir;

    private static final double EA_EQ_TOLERANCE = 0.0005;

    private PythonEval pyScript;
    
    /**
     * Constructor
     *
     * @param ifpServer
     *            IFPServer instance for this site
     * @throws GfeException
     */
    public MapManager(IFPServer ifpServer) throws GfeException {
        try {
            this.ifpServer = ifpServer;
            this.config = ifpServer.getConfig();

            String siteId = config.getSiteID();
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext baseCtx = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            this.baseDir = pathMgr.getFile(baseCtx, ".").getAbsolutePath();
            LocalizationContext configuredCtx = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            configuredCtx.setContextName(siteId);
            this.configuredDir = pathMgr.getFile(configuredCtx, ".")
                    .getAbsolutePath();
            LocalizationContext siteCtx = pathMgr
                    .getContextForSite(LocalizationType.COMMON_STATIC, siteId);
            this.siteDir = pathMgr.getFile(siteCtx, ".").getAbsolutePath();

            this.commonStaticConfigDir = this.configuredDir;

            _mapErrors = new ArrayList<>();

            long t0 = System.currentTimeMillis();
            statusHandler
                    .info("MapManager " + config.getSiteID() + " started.");

            String includePath = PyUtil.buildJepIncludePath(true,
                    GfePyIncludeUtil.getGfeConfigIncludePath(siteId),
                    FileUtil.join(baseDir, "gfe", "python"),
                    PythonIncludePathUtil.getCommonPythonIncludePath());

            List<DbShapeSource> maps = null;
            pyScript = null;
            try {
                JepConfig jepConfig = new JepConfig()
                        .setIncludePath(includePath)
                        .setClassLoader(this.getClass().getClassLoader());
                pyScript = new PythonEval(jepConfig);
                pyScript.eval("from Maps import *");

                @SuppressWarnings("unchecked")
                List<DbShapeSource> results = (List<DbShapeSource>) pyScript
                        .execute("getMaps", null);

                maps = results;
            } catch (JepException e) {
                throw new GfeException("Error getting maps", e);
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

            /*
             * after maps, pyScript is no longer needed, Configurator will make
             * its own python interpreter on the same thread
             */
            if (pyScript != null) {
                try {
                    pyScript.close();
                    pyScript = null;
                } catch (JepException e) {
                    statusHandler.warn("Failed to dispose script instance.", e);
                }
            }

            // configure the text products
            Configurator configurator = new Configurator(config.getSiteID());
            statusHandler.info("Configuring text products....");
            configurator.execute();

            if (needUpdate) {
                // need the attributes from the edit area step to be able to do
                // this right
                String site = config.getSiteID();
                new CombinationsFileMaker().genCombinationsFiles(site,
                        editAreaMap);
                new AreaDictionaryMaker().genAreaDictionary(site,
                        editAreaAttrs);

                // Completed edit areas generation and related tasks. Remove tag.

                File tag = new File(FileUtil.join(commonStaticConfigDir, 
                                                  EDIT_AREAS_DIR),
                                    EDITAREA_GEN_TAGFILE);
                if (tag.exists()) {
                    tag.delete();
                }
            }

            statusHandler.info("MapManager ready.");
            long t1 = System.currentTimeMillis();
            statusHandler.info("MapCreation time: " + (t1 - t0) + " ms");
        } finally {
            if (pyScript != null) {
                try {
                    pyScript.close();
                } catch (JepException e) {
                    statusHandler.warn("Failed to dispose script instance.", e);
                }
            }
        }
    }

    private boolean updateNeeded(List<DbShapeSource> maps,
            final String directory) {

        // Check for tag file. If exits, the last attempt didn't complete
        // and it needs to be redone.

        File gentag = new File(FileUtil.join(directory, 
                               EDITAREA_GEN_TAGFILE));

        if (gentag.exists()) {
            statusHandler.info("The previous attempt of edit area generation"
            		+ " and related tasks did not complete. Will try again.");
            return true;
        }

        // calc newest file inside maps.directory()
        long newestSource = Long.MIN_VALUE;
        List<DbShapeSource> failedMaps = new ArrayList<>();
        for (DbShapeSource map : maps) {
            try {
                newestSource = Math.max(newestSource,
                        map.getLastUpdated().getTime());
            } catch (MissingLocalMapsException e) {
                reportMissingLocalMap(map, "retrieving last update time", e);
                failedMaps.add(map);
            }
        }
        maps.removeAll(failedMaps);

        // Determine time of last modification of Maps.py, serverConfig,
        // localConfig, localMaps, and siteConfig.
        String baseConfigDir = FileUtil.join(baseDir, "gfe", "config");
        String siteConfigDir = FileUtil.join(siteDir, "gfe", "config");

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
        File localMapsTag = new File(FileUtil.join(this.configuredDir, "gfe",
                "config", "usingLocalMaps"));
        if (file.exists()) {
            newestSource = Math.max(newestSource, file.lastModified());
            localMapsTag.mkdirs();
        } else if (localMapsTag.exists()) {
            statusHandler.info(
                    "localMaps.py file removed. Edit areas will be regenerated.");
            localMapsTag.delete();
            newestSource = System.currentTimeMillis();
        }

        // calc oldest file in directory
        long oldestEditArea = Long.MIN_VALUE;
        File dir = new File(directory);
        if (dir.exists()) {
            try {
                Optional<Path> oldestFile = Files.list(dir.toPath())
                        .filter(f -> Files.isRegularFile(f)).min(Comparator
                                .comparingLong(f -> f.toFile().lastModified()));
                if (oldestFile.isPresent()) {
                    oldestEditArea = oldestFile.get().toFile().lastModified();
                }
            } catch (IOException e) {
                statusHandler
                        .warn("Could not read last modified times for path ["
                                + dir + "].", e);
            }
        }

        return (newestSource > oldestEditArea);
    }

    private void genEditArea(List<DbShapeSource> maps) {
        long t0 = System.currentTimeMillis();

        statusHandler.info("Edit Area generation phase");

        File d = new File(FileUtil.join(commonStaticConfigDir, EDIT_AREAS_DIR));
        if (d.exists()) {
            FileUtil.deleteDir(d);
        }

        d = new File(
                FileUtil.join(commonStaticConfigDir, EDIT_AREA_GROUPS_DIR));
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

        // Create tag file in the new edit area directory to start the process.

        File areaDir = new File(
                FileUtil.join(this.commonStaticConfigDir, EDIT_AREAS_DIR));
        File gentag = new File(areaDir, EDITAREA_GEN_TAGFILE);
        gentag.mkdirs();


        int i = 0;
        BoundingBox bounds = null;
        try {
            bounds = MapUtil.getBoundingEnvelope(this.config.dbDomain());
        } catch (Exception e1) {
            statusHandler.handle(Priority.PROBLEM, e1.getLocalizedMessage(),
                    e1);
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
                        + "Error in generating edit areas, map #" + i
                        + " Name: " + m.getDisplayName() + " Basename: "
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
        writeSpecialISCEditAreas();
        
        long t1 = System.currentTimeMillis();
        statusHandler.info("EditArea generation time: " + (t1 - t0) + " ms");
    }

    private void writeISCMarker() {

        // find all office types
        List<String> foundOfficeTypes = new ArrayList<>();
        for (int i = 0; i < this.iscMarkersID.size(); i++) {
            int index = this.config.allSites().indexOf(iscMarkersID.get(i));
            if (index != -1) {
                String officeType = config.officeTypes().get(index);
                if (!foundOfficeTypes.contains(officeType)) {
                    foundOfficeTypes.add(officeType);
                }
            }
        }

        // create each sample set based on office type
        for (int i = 0; i < foundOfficeTypes.size(); i++) {
            List<Coordinate> values = new ArrayList<>();
            for (int j = 0; j < iscMarkersID.size(); j++) {
                int index = config.allSites().indexOf(iscMarkersID.get(j));
                if ((index != -1) && config.officeTypes().get(index)
                        .equals(foundOfficeTypes.get(i))) {
                    values.add(iscMarkers.get(j));
                }
            }

            String markerSetName = "ISC_Marker_Set_" + foundOfficeTypes.get(i);
            SampleId sampleId = new SampleId(markerSetName);
            SampleData sd = new SampleData(new SampleId(markerSetName), values);

            File sampleDir = new File(
                    FileUtil.join(commonStaticConfigDir, SAMPLE_SETS_DIR));
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
                        statusHandler.error("Error writing ISC Markers to file "
                                + path.getAbsolutePath(), e);
                    }
                }
            } else {
                statusHandler.error("Unable to write to " + sampleDir);
            }
        }
    }

    private void writeSpecialISCEditAreas() {
        statusHandler.debug("Creating: ISC_Tool_Area and ISC_Send_Area.");

        ReferenceMgr refDataMgr = ifpServer.getReferenceMgr();
        String thisSite = config.getSiteID();

        List<ReferenceData> areas = new ArrayList<>();
        List<String> editAreaNames = new ArrayList<>();

        ReferenceData iscSendArea = null;
        ReferenceID iscAreaName = new ReferenceID("ISC_" + thisSite);
        ServerResponse<ReferenceData> sr = refDataMgr.getData(iscAreaName);
        if (sr.isOkay()) {
            iscSendArea = new ReferenceData(sr.getPayload());
            iscSendArea.setId(new ReferenceID("ISC_Send_Area"));
            areas.add(iscSendArea);
            editAreaNames.add(iscSendArea.getId().getName());

            ReferenceData toolArea = createSwathArea("ISC_Tool_Area",
                    iscSendArea, 4);
            if (toolArea != null) {
                areas.add(toolArea);
                editAreaNames.add(toolArea.getId().getName());
            }
        } else {
            String errorMsg = String.format(
                    "Could not retrieve ISC edit area for site %s: %s",
                    thisSite, sr.message());
            statusHandler.error(errorMsg);
            return;
        }

        /*
         * ISC_Send_Area will always be at least the union of the ISC and
         * FireWxAOR edit areas. If any additional ISC databases have been
         * defined, we'll union in that database's edit area too.
         */
        Collection<String> altISCEditAreas = new HashSet<>();
        altISCEditAreas.add("FireWxAOR_");
        altISCEditAreas.addAll(config.alternateISCEditAreaMasks());
        for (String altISCEditArea : altISCEditAreas) {
            ReferenceID editAreaName = new ReferenceID(
                    altISCEditArea + thisSite);
            sr = refDataMgr.getData(editAreaName);
            if (sr.isOkay()) {
                ReferenceData refData = sr.getPayload();
                iscSendArea.orEquals(refData);
            } else {
                String errorMsg = String.format(
                        "Could not retrieve additional ISC edit area %s for site %s: %s. It will not be included in ISC_Send_Area defintion.",
                        editAreaName.getName(), thisSite, sr.message());
                statusHandler.warn(errorMsg);
            }
        }

        ReferenceData swath = createSwathArea("ISC_Swath", iscSendArea, 4);
        if (swath != null) {
            iscSendArea.orEquals(swath);
        }

        saveEditAreas(areas);
        saveGroupList("ISC", editAreaNames);
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
        if (data.isEmpty()) {
            return;
        }

        // save the reference data
        saveEditAreas(data);

        // handle the group list, if specified
        String groupName = mapDef.getGroupName();
        if (groupName != null) {
            List<String> list = new ArrayList<>();
            for (int i = 0; i < data.size(); i++) {
                list.add(data.get(i).getId().getName());
            }

            // Need some special edit areas for ISC
            // Create ISC_Tool_Area and ISC_Send_Area if ISC_CWA
            List<String> knownSites = config.allSites();
            boolean anySites = false;
            if ("ISC".equals(groupName)) {
                for (int i = 0; i < data.size(); i++) {
                    String n = data.get(i).getId().getName();
                    if ((n.length() == 7) && n.startsWith("ISC_")) {
                        String cwa = n.substring(4, 7);

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
        File areaDir = new File(
                FileUtil.join(this.commonStaticConfigDir, EDIT_AREAS_DIR));
        if (!areaDir.exists()) {
            areaDir.mkdirs();
        }

        if (areaDir.isDirectory() && areaDir.canWrite()) {
            SingleTypeJAXBManager<ReferenceData> jaxbManager = ReferenceData
                    .getJAXBManager();

            for (ReferenceData ref : data) {
                ref.getPolygons(CoordinateType.LATLON);
                File path = new File(FileUtil.join(areaDir.getAbsolutePath(),
                        ref.getId().getName() + ".xml"));
                if (path.exists()) {
                    // Don't write to file. If the new one is different from the
                    // old one, write a warning to the log.
                    ReferenceData other = null;
                    try {
                        other = jaxbManager.unmarshalFromXmlFile(path);
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
                        jaxbManager.marshalToXmlFile(ref,
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
            GridLocation gridLoc = config.dbDomain();
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
            if (!bits.findNearestSet(
                    new Point((int) centerCell.x, (int) centerCell.y),
                    modCenterCell)) {
                // nothing found (should never see this)
                return;
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
        File groupDir = new File(
                FileUtil.join(commonStaticConfigDir, EDIT_AREA_GROUPS_DIR));
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
            try (BufferedWriter out = new BufferedWriter(
                    new FileWriter(path))) {
                out.write(Integer.toString(l.size()));
                out.write('\n');
                for (String name : l) {
                    out.write(name);
                    out.write('\n');
                }
            } catch (IOException e) {
                statusHandler
                        .error("Error saving edit area group: " + groupName, e);
            }
        } else {
            statusHandler.error("Unable to write to " + groupDir);
        }
    }

    private List<String> loadGroupList(String groupName) {
        List<String> list = new ArrayList<>();
        File groupDir = new File(
                FileUtil.join(commonStaticConfigDir, EDIT_AREA_GROUPS_DIR));
        if (groupDir.exists() && groupDir.isDirectory()) {
            File groupFile = new File(FileUtil.join(groupDir.getAbsolutePath(),
                    groupName + ".txt"));
            if (groupFile.exists()) {
                try (BufferedReader in = new BufferedReader(
                        new FileReader(groupFile))) {
                    String s = in.readLine();
                    int count = Integer.parseInt(s);
                    for (int i = 0; i < count; i++) {
                        String area = in.readLine();
                        list.add(area);
                    }
                } catch (Exception e) {
                    statusHandler.error("Error reading group file: "
                            + groupFile.getAbsolutePath(), e);
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

            extRing = (LinearRing) TopologyPreservingSimplifier
                    .simplify(extRing, 0.5);

            polygons[i] = (Polygon) extRing.buffer(cells / 2.0, 8,
                    BufferParameters.CAP_SQUARE);
        }

        GeometryFactory gf = new GeometryFactory();
        Geometry geom = gf.createGeometryCollection(polygons).union();
        MultiPolygon mp;
        if (geom.getNumGeometries() > 1) {
            mp = (MultiPolygon) geom;
        } else if (geom.getNumGeometries() > 0) {
            mp = gf.createMultiPolygon(
                    new Polygon[] { (Polygon) geom.getGeometryN(1) });
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
        List<ReferenceData> data = new ArrayList<>();
        List<Map<String, Object>> attributes = new ArrayList<>();
        editAreaAttrs.put(mapDef.getDisplayName(), attributes);

        // Module dean("DefaultEditAreaNaming");
        ArrayList<String> created = new ArrayList<>();
        GeometryFactory gf = new GeometryFactory();
        DbShapeSource shapeSource = mapDef;
        try {
            // PathMgr pm(_config.dbBaseDirectory(), "MAPS");
            // String directory = pm.writePath(AccessLevel.baseName(), "");

            GridLocation gloc = config.dbDomain();
            MathTransform transform = MapUtil
                    .getTransformFromLatLon(PixelOrientation.CENTER, gloc);

            Coordinate c0 = new Coordinate(0, 0);
            Coordinate c1 = new Coordinate(gloc.getNx() - 1, 0);
            Coordinate c2 = new Coordinate(gloc.getNx() - 1, gloc.getNy() - 1);
            Coordinate c3 = new Coordinate(0, gloc.getNy() - 1);
            Polygon p = gf.createPolygon(gf.createLinearRing(
                    new Coordinate[] { c0, c1, c2, c3, c0 }), null);
            PreparedGeometry boundingGeometry = PreparedGeometryFactory
                    .prepare(p);

            Map<String, Geometry> tempData = new HashMap<>();
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
                    info.put("editarea", ean);
                    attributes.add(info);
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

                IsValidOp polygonValidator = new IsValidOp(polygons);
                if (!polygonValidator.isValid()) {
                    String error = String.format(
                            "Table: %s edit area: %s contains invalid polygons: %s. This edit area will be skipped.",
                            shapeSource.getTableName(), ean,
                            polygonValidator.getValidationError());
                    statusHandler.error(error);
                    continue;
                }

                // transfer dictionary values to Seq values
                data.add(new ReferenceData(config.dbDomain(),
                        new ReferenceID(ean), polygons, CoordinateType.LATLON));
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

        Map<String, Object> args = new HashMap<>();
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

        Map<String, Object> args = new HashMap<>();
        args.put("instance", instance);
        args.put("info", info);
        try {
            result = (Boolean) pyScript.execute("runFilter", args);
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception filtering " + instance, e);
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
            if (!Character.isLetterOrDigit(s.charAt(i))
                    && (s.charAt(i) != '_')) {
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
                + " Check site [" + config.getSiteID()
                + "] localMaps.py configuration and verify all necessary shape files have been imported.";
        EDEXUtil.sendMessageAlertViz(Priority.ERROR,
                "com.raytheon.edex.plugin.gfe", "GFE", "GFE", errorUser,
                errorUser, null);

        return errorLog;
    }
}
