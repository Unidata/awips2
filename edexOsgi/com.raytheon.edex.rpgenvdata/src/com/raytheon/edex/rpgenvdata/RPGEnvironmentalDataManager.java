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
package com.raytheon.edex.rpgenvdata;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.tools.bzip2.CBZip2OutputStream;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.edex.rpgenvdata.Configuration.Field;
import com.raytheon.edex.rpgenvdata.Configuration.Level;
import com.raytheon.edex.rpgenvdata.Configuration.ParameterDef;
import com.raytheon.edex.uengine.tasks.query.TermQuery;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.message.ExternalData;
import com.raytheon.rcm.message.GenericData;
import com.raytheon.rcm.message.GenericData.Component;
import com.raytheon.rcm.message.GenericData.GridComponent;
import com.raytheon.rcm.message.GenericData.Parameter;
import com.raytheon.rcm.mqsrvr.ReplyObj.ConfigReply;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.mqsrvr.ReqObj.SendMessageToRPG;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class RPGEnvironmentalDataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RPGEnvironmentalDataManager.class);

    private Configuration configuration;

    private PluginDao gridDao;

    private MyRcmClient rcmClient;

    private StringBuilder logMessages = new StringBuilder();

    private SourceGridData currentGridData;

    /**
     * If true, pick closest data time to current time even if it is too far
     * away to be useful. Used for testing.
     */
    private boolean anyTimeIsGood = false;

    private boolean compressionEnabled = true;

    public RPGEnvironmentalDataManager() {
        try {
            initialize();
        } catch (Exception e) {
            log(Priority.SIGNIFICANT, "Error initializing", e);
        }
    }

    protected void log(Priority priority, String message) {
        log(priority, message, null);
    }

    protected void log(Priority priority, String message, Throwable throwable) {
        // Priority.name() looks different from actual logs.
        logMessages.append(priority.name()).append(": ");
        if (message != null) {
            logMessages.append(message);
            if (throwable != null) {
                logMessages.append(": ");
            }
        }
        if (throwable != null) {
            String excMessage = throwable.getMessage();
            if (excMessage != null) {
                logMessages.append(excMessage);
            }
        }
        logMessages.append('\n');

        statusHandler.handle(priority, message, throwable);
    }

    public void initialize() throws JAXBException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        File file = pathMgr.getFile(edexStaticBase, "rpgenvdata"
                + File.separator + "EnvironParamsLevelTable.xml");

        Unmarshaller u = Configuration.getUnmashaller();
        configuration = (Configuration) u.unmarshal(file);

        if (!validateConfiguration(configuration)) {
            configuration = null;
        }

        try {
            gridDao = PluginFactory.getInstance().getPluginDao(
                    GridConstants.GRID);
        } catch (PluginException e) {
            log(Priority.SIGNIFICANT, "Unable to get grid dao", e);
        }

        rcmClient = new MyRcmClient();
        try {
            rcmClient.initialize();
        } catch (Exception e) {
            rcmClient = null;
            log(Priority.SIGNIFICANT,
                    "Error initializing RadarServer connection", e);
        }
    }

    public void close() {
        rcmClient.close();
        currentGridData = null;
        logMessages = new StringBuilder();
    }

    private boolean validateConfiguration(Configuration configuration) {
        if (configuration.model == null || configuration.model.name == null
                || configuration.model.description == null) {
            logConfigError("Model not defined");
        } else if (configuration.clipRadius == null) {
            logConfigError("Clip radious not defined");
        } else if (configuration.fields == null
                || configuration.fields.length == 0) {
            logConfigError("No fields defined");
        } else {
            for (Field field : configuration.fields) {
                if (field.name == null || field.name.length() == 0
                        || field.description == null
                        || field.description.length() == 0) {
                    logConfigError("Missing field name or description");
                    return false;
                }
                for (Level levelSpec : field.levels) {
                    if (levelSpec.name == null || levelSpec.name.length() == 0
                            || levelSpec.description == null
                            || levelSpec.description.length() == 0) {
                        logConfigError(String
                                .format("Level of field %s missing name or description",
                                        field.name));
                        return false;
                    }
                }
            }
            return true;
        }
        return false;
    }

    private void logConfigError(String message) {
        log(Priority.SIGNIFICANT, "Configuration error: " + message);
    }

    public void sendEnvironmentalDataToRPGs() {
        if (rcmClient == null || configuration == null) {
            return;
        }

        ConfigReply reply = rcmClient.sendCheckedAndHandled(
                new ReqObj.GetRadarConfig(), ConfigReply.class);
        if (reply != null) {
            int nSent = 0;
            for (RadarConfig rc : reply.config) {
                if (rc.isDedicated() && rc.isEnabled() &&
                /*
                 * AWIPS 1 code implies that TDWRs should be skipped, but this
                 * may not happen depending on what generates radarSites.txt.
                 */
                // Util.getRadarType(rc) == RadarType.WSR &&
                        rc.isSendEnvironmentalData()) {

                    sendEnvironmentalDataToRPG(rc.getRadarID());
                    ++nSent;
                }
            }
            if (nSent == 0) {
                log(Priority.EVENTA/* PROBLEM? */,
                        "No appropriate RPGs to send environmental data to.");
            }
        }
    }

    public void sendBiasTableToRPGs() {
        String path = AppsDefaults.getInstance().getToken("bias_message_dir");
        if (path != null) {
            processBiasTableDirectory(path);
        } else {
            log(Priority.SIGNIFICANT, "Bias table directory not configured.");
        }
    }

    public void processBiasTableDirectory(String path) {
        HashMap<Integer, File> map = new HashMap<Integer, File>();
        Pattern fnPattern = Pattern
                .compile("^(\\d+)\\.\\d+\\.\\d+\\.\\d+\\.\\d+$");
        // DDD .SSS .PP .? .DDMMHH
        File dir = new File(path);
        File[] files = dir.listFiles();

        Arrays.sort(files, new Comparator<File>() {
            public int compare(File o1, File o2) {
                // Sort descending to get most recent first
                return Long.signum(o2.lastModified() - o1.lastModified());
            }
        });

        for (File f : files) {
            // Ignore non-files
            if (!f.isFile()) {
                continue;
            }

            String fName = f.getName();

            // Ignore products that are being created
            if (fName.endsWith(".temp")) {
                continue;
            }

            Matcher matcher = fnPattern.matcher(fName);

            // Delete anything that does not match the expected pattern
            if (!matcher.matches()) {
                f.delete();
                continue;
            }

            int nexradID = Integer.parseInt(matcher.group(1));
            if (!map.containsKey(nexradID)) {
                map.put(nexradID, f);
            } else {
                f.delete(); // Delete old products
            }
        }

        if (rcmClient == null) {
            return;
        }

        ConfigReply reply = rcmClient.sendCheckedAndHandled(
                new ReqObj.GetRadarConfig(), ConfigReply.class);

        if (reply == null) {
            return;
        }

        fileLoop: for (Map.Entry<Integer, File> entry : map.entrySet()) {
            int id = entry.getKey();
            File f = entry.getValue();

            String radarID = null;
            for (RadarConfig rc : reply.config) {
                if (id == rc.getNexradID()) {
                    radarID = rc.getRadarID();
                    if (rc.isDedicated() && rc.isEnabled()
                            && rc.isSendEnvironmentalData()) {
                        break;
                    } else {
                        f.delete();
                        continue fileLoop;
                    }
                }
            }
            if (radarID == null) {
                log(Priority.SIGNIFICANT,
                        String.format("Unknown radar ID %d", id), null);
                continue;
            }

            byte[] message;
            try {
                FileInputStream fin = new FileInputStream(f);
                try {
                    message = new byte[(int) f.length()];
                    (new DataInputStream(fin)).readFully(message);
                } finally {
                    try {
                        fin.close();
                    } catch (IOException e) {
                        // nothing
                    }
                }
            } catch (IOException e) {
                log(Priority.SIGNIFICANT,
                        "Error reading file " + f.getAbsolutePath(), e);
                continue;
            }
            try {
                message = convertBiasTableByteOrder(message);
            } catch (RuntimeException e) {
                log(Priority.SIGNIFICANT,
                        "Error processing " + f.getAbsolutePath(), e);
                continue;
            }

            sendMessageToRPG("bias table " + f.getAbsolutePath(), message,
                    radarID);
            // TODO: Delete file if we get positive confirmation
        }
    }

    /**
     * Converts bias table to big endian format if in little format.
     * 
     * The biasmesgen program generates the bias table in little endian format.
     * However, it is not enough to simply byte-swap each half-word.
     */
    public static byte[] convertBiasTableByteOrder(byte[] message) {
        ByteBuffer input = ByteBuffer.wrap(message);
        if (input.get(0) == 0x0f && input.get(1) == 0x00) {
            // Input is in little-endian format. Convert to big-endian.
            byte[] result = new byte[message.length];
            ByteBuffer output = ByteBuffer.wrap(result);

            input.order(ByteOrder.LITTLE_ENDIAN);
            convertShorts(input, output, 2); // message code, date
            output.putInt(input.getInt()); // time
            output.putInt(input.getInt()); // message length
            /*
             * src, dst, block count, divider, block id, version, block length
             */
            convertShorts(input, output, 7);
            convertShorts(input, output, 4); // site and radar id
            convertShorts(input, output, 12); // obs and generation time

            int nRows = input.getShort() & 0xffff;
            output.putShort((short) nRows);

            while (nRows-- > 0) {
                for (int i = 0; i < 5; ++i) {
                    output.putInt(input.getInt());
                }
            }

            output.put(input); // If there is anything left over,

            return result;
        } else {
            return message;
        }
    }

    private static void convertShorts(ByteBuffer source, ByteBuffer dest,
            int count) {
        for (int i = 0; i < count; ++i) {
            dest.putShort(source.getShort());
        }
    }

    public void sendEnvironmentalDataToRPG(String radarID) {
        byte[] message = generateEnvironmentalDataForRPG(radarID);
        if (message != null) {
            sendMessageToRPG("environmental data", message, radarID);
        }
    }

    public void sendMessageToRPG(String what, byte[] message, String radarID) {
        if (rcmClient != null) {
            SendMessageToRPG msg = new SendMessageToRPG();
            msg.radarID = radarID;
            msg.message = message;
            if (rcmClient.sendCheckedAndHandled(msg) != null) {
                log(Priority.EVENTA, String.format("Sent %s to %s", what,
                        radarID != null ? radarID : "RPG"));
            }
        } else {
            log(Priority.PROBLEM,
                    "Not sending to RPG because RadarServer is not available.");
        }
    }

    public byte[] generateEnvironmentalDataForRPG(String radarID) {
        SourceGridData newData = new SourceGridData();
        DataTime newDataTime = newData.findDataTime();
        if (newDataTime != null) {
            if (currentGridData == null
                    || !newDataTime.equals(currentGridData.findDataTime())) {
                currentGridData = newData;
            }
        }

        SourceGridData dataToUse = currentGridData;
        if (dataToUse != null) {
            return dataToUse.generateEnvironmentalDataForRPG(radarID);
        }

        return null;
    }

    public Configuration getConfiguration() {
        return configuration;
    }

    private static enum BuiltInParameter {
        mod_name, mod_run_date, mod_run_time, val_date, val_time, forecast_hr, coord_system, proj, lat_lower_left, lon_lower_left, lat_upper_right, lon_upper_right, lat_tang_pt, lon_tang_pt, numXpts, numYpts;
    }

    private class MyRcmClient extends RcmClient {

        @Override
        public void onFailure(String detail, Exception e) {
            log(Priority.SIGNIFICANT, detail, e);
        }
    }

    class SourceGridData {
        private DataTime dataTime;

        private GridCoverage gridCoverage;

        String getProjectionName() {
            /*
             * The RPG code assumes the grid has a Lambert conformal projection
             * no matter what value is given for the project name. There is a
             * test, however, that looks for the string "Lambert Conformal"
             * which may indicate a more formal check may be performed in the
             * future. LambertConformalGridCoverage.getProjectionType() returns
             * the correct value, so just use that for now.
             */
            return gridCoverage != null ? gridCoverage.getProjectionType()
                    : null;
        }

        private class GeoInfo {
            GridEnvelope2D gridDomain;

            GridEnvelope2D clipDomain;

            DirectPosition2D clipLLLatLon;

            DirectPosition2D clipURLatLon;

            DirectPosition2D tangentPoint;
        }

        private class GeoInfoException extends Exception {
            public GeoInfoException(String message) {
                super(message);
            }
        }

        private GeoInfo getGeoInfo(String radarID, GridCoverage gridCoverage)
                throws Exception {
            GeoInfo result = new GeoInfo();
            RadarStationDao rsd = new RadarStationDao();
            RadarStation radarStation;

            radarStation = rsd.queryByRdaId(radarID.toUpperCase());
            if (radarStation == null) {
                throw new GeoInfoException(String.format(
                        "Radar site \"%s\" not found", radarID));
            }

            GridGeometry2D gridGeom = MapUtil.getGridGeometry(gridCoverage);
            GridEnvelope2D ge = gridGeom.getGridRange2D();
            int maxY = ge.getHigh(1);
            MathTransform llToGrid;
            MathTransform gridToLL;

            llToGrid = TransformFactory.latLonToGrid(gridGeom,
                    PixelInCell.CELL_CORNER);
            gridToLL = TransformFactory.gridToLatLon(gridGeom,
                    PixelInCell.CELL_CORNER);

            DirectPosition2D stationLL = new DirectPosition2D(
                    radarStation.getLon(), radarStation.getLat());
            DirectPosition2D stationIJx = new DirectPosition2D(0, 0);

            llToGrid.transform(stationLL, stationIJx);

            long radarI = Math.round(stationIJx.x);
            long radarJ = maxY - Math.round(stationIJx.y);
            int i1, j1, i2, j2;

            // TODO: get this from the math transform?...
            long radInPointsI, radInPointsJ;
            if (gridCoverage instanceof LambertConformalGridCoverage) {
                LambertConformalGridCoverage lcgc = (LambertConformalGridCoverage) gridCoverage;
                Unit<?> spacingUnit = Unit.valueOf(lcgc.getSpacingUnit());
                Unit<?> clipRadUnit = Unit
                        .valueOf(configuration.clipRadius.units);
                if (!clipRadUnit.isCompatible(spacingUnit)) {
                    throw new GeoInfoException(
                            String.format(
                                    "Grid spacing units (%s) not compatible clip radius units (%s)",
                                    spacingUnit, clipRadUnit));
                }
                UnitConverter uc = spacingUnit.getConverterTo(clipRadUnit);
                radInPointsI = Math.round(configuration.clipRadius.value
                        / uc.convert(lcgc.getDx()));
                radInPointsJ = Math.round(configuration.clipRadius.value
                        / uc.convert(lcgc.getDy()));

                result.tangentPoint = new DirectPosition2D(lcgc.getLov(),
                        lcgc.getLatin1());
            } else {
                throw new GeoInfoException(
                        "Only Lambert conformal projection is supported");
            }

            i1 = (int) (radarI - radInPointsI);
            i2 = (int) (radarI + radInPointsI);
            j1 = (int) (radarJ - radInPointsJ);
            j2 = (int) (radarJ + radInPointsJ);

            if (i1 < ge.getLow(0) || i2 > ge.getHigh(0) || j1 < ge.getLow(1)
                    || j2 > ge.getHigh(1)) {
                throw new GeoInfoException(
                        String.format(
                                "Clipped region for radar %s is outside %s grid bounds",
                                radarID, configuration.model.name));
            }

            result.gridDomain = ge;
            result.clipDomain = new GridEnvelope2D(i1, j1, i2 - i1 + 1, j2 - j1
                    + 1);
            result.clipLLLatLon = new DirectPosition2D(0, 0);
            result.clipURLatLon = new DirectPosition2D(0, 0);

            gridToLL.transform(new DirectPosition2D(i1, maxY - j1),
                    result.clipLLLatLon);
            gridToLL.transform(new DirectPosition2D(i2, maxY - j2),
                    result.clipURLatLon);

            return result;
        }

        byte[] generateEnvironmentalDataForRPG(String radarID) {
            if (findDataTime() == null) {
                return null;
            }

            if (gridCoverage == null || gridDao == null) {
                return null;
            }

            GeoInfo geoInfo;
            try {
                geoInfo = getGeoInfo(radarID, gridCoverage);
            } catch (GeoInfoException e) {
                log(Priority.SIGNIFICANT,
                        "Error getting geographic information: "
                                + e.getMessage());
                return null;
            } catch (Exception e) {
                log(Priority.SIGNIFICANT,
                        "Error getting geographic information: ", e);
                return null;
            }

            GenericData genericData = new GenericData();

            genericData.name = "Env Grid Data";
            genericData.description = "Environmental Gridded Data";
            genericData.code = 99; // Unknown. Copied from AWIPS 1.
            genericData.type = 7; // From ICD. 7 = External
            genericData.generationTime = (int) (System.currentTimeMillis() / 1000);

            ArrayList<GridComponent> grids = new ArrayList<GridComponent>(
                    configuration.fields.length);

            for (Field field : configuration.fields) {
                for (Level levelSpec : field.levels) {
                    List<GridRecord> records = inventory(dataTime, field,
                            levelSpec);
                    if (records == null) {
                        continue;
                    } else if (records.size() == 0) {
                        log(Priority.PROBLEM, String.format(
                                "No inventory for %s (%s) at %s", field.name,
                                field.description, dataTime));
                        continue;
                    }
                    Collection<Double> levelList = getDesiredLevels(field,
                            levelSpec);
                    // TODO: more efficient matching
                    for (Double level : levelList) {
                        for (GridRecord rec : records) {
                            if (level.equals(rec.getLevel().getLevelonevalue())) {
                                grids.add(generateGrid(radarID, field,
                                        levelSpec, level, geoInfo.gridDomain,
                                        geoInfo.clipDomain, rec));
                                break;
                            }
                        }

                    }
                }
            }

            if (grids.size() == 0) {
                return null; // There should already be a log message
            }

            genericData.components = grids.toArray(new Component[grids.size()]);

            for (ParameterDef pd : configuration.parameters) {
                Parameter p = new Parameter(pd.id);
                p.setName(pd.name);
                p.setType(pd.type);
                p.setUnits(pd.units);
                if (pd.value != null) {
                    p.setValue(pd.value);
                } else {
                    BuiltInParameter biParam = null;
                    try {
                        biParam = BuiltInParameter.valueOf(p.getId());
                    } catch (Exception e) {
                        // handled below
                    }
                    if (biParam == BuiltInParameter.mod_name) {
                        p.setValue(configuration.model.description);
                    } else if (biParam == BuiltInParameter.mod_run_date) {
                        p.setValue(String.format("%1$tY%1$tm%1$td",
                                dataTime.getRefTime()));
                    } else if (biParam == BuiltInParameter.mod_run_time) {
                        p.setValue(String.format("%1$tH:%1$tM:%1$tS",
                                dataTime.getRefTime()));
                    } else if (biParam == BuiltInParameter.val_date) {
                        p.setValue(String.format("%1$tY%1$tm%1$td",
                                dataTime.getValidTime()));
                    } else if (biParam == BuiltInParameter.val_time) {
                        p.setValue(String.format("%1$tH:%1$tM:%1$tS",
                                dataTime.getValidTime()));
                    } else if (biParam == BuiltInParameter.forecast_hr) {
                        p.setValue(dataTime.getFcstTime() / 3600);
                    } else if (biParam == BuiltInParameter.proj) {
                        p.setValue(getProjectionName());
                    } else if (biParam == BuiltInParameter.lat_lower_left) {
                        p.setValue(geoInfo.clipLLLatLon.y);
                    } else if (biParam == BuiltInParameter.lon_lower_left) {
                        p.setValue(geoInfo.clipLLLatLon.x);
                    } else if (biParam == BuiltInParameter.lat_upper_right) {
                        p.setValue(geoInfo.clipURLatLon.y);
                    } else if (biParam == BuiltInParameter.lon_upper_right) {
                        p.setValue(geoInfo.clipURLatLon.x);
                    } else if (biParam == BuiltInParameter.lat_tang_pt) {
                        p.setValue(geoInfo.tangentPoint.y);
                    } else if (biParam == BuiltInParameter.lon_tang_pt) {
                        p.setValue(geoInfo.tangentPoint.x);
                    } else if (biParam == BuiltInParameter.numXpts) {
                        p.setValue(geoInfo.clipDomain.getSpan(0));
                    } else if (biParam == BuiltInParameter.numYpts) {
                        p.setValue(geoInfo.clipDomain.getSpan(1));
                    } else {
                        log(Priority.PROBLEM,
                                String.format("Unknown parameter '%s'",
                                        p.getId()));
                        p = null;
                    }
                }
                if (p != null) {
                    genericData.parameters.put(p);
                }
            }

            return ExternalData.encode(genericData, new Bzip2Compressor());
        }

        GridComponent generateGrid(String radarID, Field field,
                Level levelSpec, double level, GridEnvelope2D domain,
                GridEnvelope2D clip, GridRecord gridRecord) {

            int clippedNx = clip.getSpan(0);
            int clippedNy = clip.getSpan(1);

            GridComponent grid = new GridComponent();
            grid.gridType = GridComponent.EQUALLY_SPACED;
            grid.dimensions = new int[] { clippedNx, clippedNy };
            grid.parameters = new GenericData.ParameterSet();

            Parameter p = new Parameter("Level"); // Must be title case
            p.setName(levelSpec.description);
            p.setValue(level); // AWIPS1 uses int, but RPG code uses strtod so
                               // this should be fine.
            p.setUnits(levelSpec.units);
            grid.parameters.put(p);

            p = grid.getDataAttributes();
            p.setName(field.description);
            p.setUnits(field.units);
            // p.putAttribute("unit", field.units);// AWIPS1 does this is but it
            // is very wrong?!
            p.setType("float"); // Only 'float' type current is supported

            if (gridDao == null) {
                return null;
            }

            FloatDataRecord dataRecord;
            try {
                dataRecord = (FloatDataRecord) (gridDao.getHDF5Data(gridRecord,
                        -1)[0]);
            } catch (Exception e) {
                log(Priority.SIGNIFICANT, "Error retrieving grid data", e);
                return null;
            }

            float[] data = dataRecord.getFloatData();
            float[] clippedData = new float[grid.getPointCount()];
            int stride = domain.getSpan(0);
            int iidx = clip.getLow(0) + clip.getLow(1) * stride;
            int oidx = 0;

            for (int j = 0; j < clippedNy; ++j) {
                for (int i = 0; i < clippedNx; ++i) {
                    clippedData[oidx++] = data[iidx + i];
                }
                iidx += stride;
            }

            grid.data = clippedData;

            return grid;
        }

        // Can be used to determine if there is a better matching time (but that
        // would usually be the case?)
        DataTime findDataTime() {
            if (dataTime == null) {
                try {
                    if (gridDao == null) {
                        return null;
                    }
                    DatabaseQuery q = new DatabaseQuery(gridDao.getDaoClass()
                            .getName());
                    q.addDistinctParameter("dataTime");
                    q.addQueryParam(GridConstants.DATASET_ID,
                            configuration.model.name);
                    q.addOrder("dataTime.refTime", false);
                    q.addOrder("dataTime.fcstTime", false);
                    List<DataTime> qrTimeList = (List<DataTime>) gridDao
                            .queryByCriteria(q);

                    /*
                     * Filter out data times that have a valid time period. None
                     * of the fields we are currently interested in use the
                     * valid period.
                     */
                    ArrayList<DataTime> dataTimeList = new ArrayList<DataTime>(
                            qrTimeList.size());
                    for (DataTime dt : qrTimeList) {
                        if (!dt.getUtilityFlags().contains(FLAG.PERIOD_USED)) {
                            dataTimeList.add(dt);
                        }
                    }

                    int allegedForecastTimeStep = findAllegedTimeStep(dataTimeList);

                    DataTime desiredValidTime = findDesiredValidTime(
                            System.currentTimeMillis(), dataTimeList,
                            allegedForecastTimeStep);
                    if (desiredValidTime == null) {
                        log(Priority.SIGNIFICANT,
                                "Unable to find a suitable valid time");
                        return null;
                    }
                    dataTime = performTimeAndInventoryMatch(desiredValidTime,
                            dataTimeList);
                    if (dataTime == null) {
                        log(Priority.SIGNIFICANT,
                                "Unable to find a suitable data time for valid time "
                                        + desiredValidTime);
                    }
                } catch (Exception e) {
                    log(Priority.SIGNIFICANT, "Error querying data times", e);
                    // TODO: should have dataTimeSearchDone so this will not be
                    // done again
                    dataTime = null;
                    return null;
                }
            }
            return dataTime;
        }

        /**
         * Find a data time that matches the given valid time and for which the
         * field/plane inventory is acceptable. Also determines the grid
         * coverage for the model.
         * 
         * @param desiredValidTime
         *            A DataTime that carries the desired valid time
         * @param dataTimeList
         *            A list of DataTimes
         * @return The DataTime from the list that has the same valid time as
         *         desiredValidTime and has a an acceptable inventory. Returns
         *         null if there is no such DataTime.
         */
        private DataTime performTimeAndInventoryMatch(
                DataTime desiredValidTime, List<DataTime> dataTimeList) {

            dataTimeLoop: for (DataTime dt : dataTimeList) {
                if (!dt.getValidTime().equals(desiredValidTime.getValidTime())) {
                    continue;
                }

                for (Field field : configuration.fields) {
                    for (Level levelSpec : field.levels) {
                        List<GridRecord> records = inventory(dt, field,
                                levelSpec);
                        if (records == null) {
                            continue dataTimeLoop;
                        }

                        if (gridCoverage == null) {
                            if (records.size() > 0) {
                                gridCoverage = records.get(0).getLocation();
                            }
                        }

                        Collection<Double> desiredLevels = getDesiredLevels(
                                field, levelSpec);
                        if (desiredLevels == null) {
                            continue dataTimeLoop;
                        }

                        int nFound = 0;
                        for (GridRecord gr : records) {
                            if (desiredLevels.contains(gr.getLevel()
                                    .getLevelonevalue())) {
                                ++nFound;
                            }
                        }
                        if ((float) nFound / (float) desiredLevels.size() < field
                                .getAcceptableInventoryRatio()) {
                            log(Priority.PROBLEM, String.format(
                                    "Data time %s has %d/%d levels of %s", dt,
                                    nFound, desiredLevels.size(), field.name));
                            // Not acceptable, try another time
                            continue dataTimeLoop;
                        }
                    }
                }

                // All fields/levels checked out, use this one
                return dt;
            }
            return null;
        }

        private List<GridRecord> inventory(DataTime dataTime, Field field,
                Level levelSpec) {
            try {
                TermQuery q = new TermQuery(GridConstants.GRID);
                q.addParameter(GridConstants.DATASET_ID,
                        configuration.model.name);
                q.addParameter(GridConstants.PARAMETER_ABBREVIATION, field.name);
                q.addParameter(GridConstants.MASTER_LEVEL_NAME, levelSpec.name);
                q.addParameter("dataTime", dataTime.toString());

                return (List<GridRecord>) (List<?>) q.execute();
            } catch (Exception e) {
                log(Priority.SIGNIFICANT, String.format(
                        "Unable to get inventory for %s %s %s",
                        configuration.model.name, field.name, levelSpec.name),
                        e);
                return null;
            }
        }

        /**
         * Find a good valid time in a list of data times.
         * <code>dataTimeList</code> need not be a list of unique valid times.
         * However, it must not have data times with a forecast period.
         * 
         * @param timeInMillis
         *            the time to match (e.g., the current real time)
         * @param dataTimeList
         *            the potential list of matches
         * @param allegedForecastTimeStep
         * @return
         */
        private DataTime findDesiredValidTime(long timeInMillis,
                List<DataTime> dataTimeList, int allegedForecastTimeStep) {
            DataTime best = null;
            long maxDeltaMillis;

            if (anyTimeIsGood) {
                maxDeltaMillis = -1;
            } else {
                maxDeltaMillis = allegedForecastTimeStep > 0 ? (allegedForecastTimeStep * 6 / 10) * 1000
                        : -1;
            }

            for (DataTime dt : dataTimeList) {
                long dtVtMillis = dt.getValidTime().getTimeInMillis();
                if (maxDeltaMillis > 0) {
                    if (Math.abs(dtVtMillis - timeInMillis) <= maxDeltaMillis) {
                        return dt;
                    }
                } else {
                    if (best == null
                            || Math.abs(dtVtMillis - timeInMillis) < Math
                                    .abs(best.getValidTime().getTimeInMillis()
                                            - timeInMillis)) {
                        best = dt;
                    }
                }
            }

            return best;
        }

        private int findAllegedTimeStep(List<DataTime> dataTimeList) {
            if (configuration.timeStepHint != null) {
                return configuration.timeStepHint;
            } else {
                int best = -1;
                for (DataTime dt : dataTimeList) {
                    int ft = dt.getFcstTime();
                    if (ft > 0 && (best == -1 || ft < best)) {
                        best = ft;
                    }
                }
                return best;
            }
        }
    }

    private class Bzip2Compressor implements ExternalData.ICompressor {

        private int compressionMethod;

        @Override
        public byte[] compress(byte[] data) {
            if (isCompressionEnabled()) {
                try {
                    ByteArrayOutputStream baOut = new ByteArrayOutputStream(
                            data.length / 2);
                    baOut.write(66); // 'B' -- BZip2 magic value
                    baOut.write(90); // 'Z'
                    int blockSize100k = Math.max(1/*
                                                   * CBZip2OutputStream.
                                                   * MIN_BLOCKSIZE
                                                   */,
                            Math.min(9/*
                                       * CBZip2OutputStream . MAX_BLOCKSIZE
                                       */, (data.length + 99999) / 100000));
                    CBZip2OutputStream out = new CBZip2OutputStream(baOut,
                            blockSize100k);
                    // CBZip2OutputStream.chooseBlockSize(data.length) // why is
                    // this missing?
                    out.write(data);
                    out.flush();
                    out.close();
                    byte[] result = baOut.toByteArray();
                    compressionMethod = 1;
                    return result;
                } catch (IOException e) {
                    log(Priority.SIGNIFICANT, "Compression failed", e);
                    return null;
                }
            } else {
                compressionMethod = 0;
                return data;
            }
        }

        @Override
        public int getCompressionMethod() {
            return compressionMethod;
        }

    }

    private HashMap<String, Collection<Double>> allegedLevels = new HashMap<String, Collection<Double>>();

    private String getAllegedLevelsKey(Field field, Level levelSpec) {
        return field.name + "_" + levelSpec.name;
    }

    public Collection<Double> getDesiredLevels(Field field, Level levelSpec) {
        String key = getAllegedLevelsKey(field, levelSpec);
        Collection<Double> levels = allegedLevels.get(key);
        if (levels == null) {
            if (levelSpec.levels != null) {
                levels = new ArrayList<Double>(levelSpec.levels.length);
                for (String levelName : levelSpec.levels) {
                    double level;
                    try {
                        level = Double.parseDouble(levelName);
                    } catch (RuntimeException e) {
                        log(Priority.SIGNIFICANT, String.format(
                                "Bad level \"%s\" for field %s", levelName,
                                field.name), e);
                        continue;
                    }
                    levels.add(level);
                }
            } else {
                try {
                    if (gridDao == null) {
                        return null;
                    }
                    DatabaseQuery q = new DatabaseQuery(gridDao.getDaoClass()
                            .getName());
                    q.addDistinctParameter(GridConstants.LEVEL_ONE);
                    q.addQueryParam(GridConstants.DATASET_ID,
                            configuration.model.name);
                    q.addQueryParam(GridConstants.PARAMETER_ABBREVIATION,
                            field.name);
                    q.addQueryParam(GridConstants.MASTER_LEVEL_NAME,
                            levelSpec.name);
                    q.addOrder(GridConstants.LEVEL_ONE, true);
                    levels = (List<Double>) gridDao.queryByCriteria(q);
                } catch (Exception e) {
                    log(Priority.SIGNIFICANT, String.format(
                            "Error querying possible levels for field %s",
                            field.name), e);
                    return null;
                }
            }
            allegedLevels.put(key, levels);
        }
        return levels;
    }

    public boolean isAnyTimeIsGood() {
        return anyTimeIsGood;
    }

    public void setAnyTimeIsGood(boolean anyTimeIsGood) {
        this.anyTimeIsGood = anyTimeIsGood;
    }

    public boolean isCompressionEnabled() {
        return compressionEnabled;
    }

    public void setCompressionEnabled(boolean compressionEnabled) {
        this.compressionEnabled = compressionEnabled;
    }

    public String getMessages() {
        return logMessages.toString();
    }
}
