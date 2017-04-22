package com.raytheon.uf.edex.ndm.dataplugin.subscriber;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.opengis.feature.Property;
import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.dataplugin.util.Shapefile;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.PrecisionModel;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Decoder implementation for radar shape files plugin
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10Oct2011    10520       JWork       Initial check-in.
 * 09/11/2012   DR 15366    D. Friedman Set SRID on radar stations.
 * Mar 06, 2014 2876        mpduff      Moved NationalDatasetSubscriber.
 * Jul 09, 2015 4500        rjpeter     Fix SQL Injection concern.
 * Jan 27, 2016 5237        tgurney     Replace deprecated LocalizationFile
 *                                      method call
 * Mar 02, 2016 5434        bkowal      Relocated to ndm dataplugin. Eliminated
 *                                      deprecation warnings.
 * Jul 18, 2016 5744        mapeters    Use common_static instead of edex_static
 *                                      for saving/retrieving shape files
 * </pre>
 */
public class Import88DLocationsUtil implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Import88DLocationsUtil.class);

    private static final String SHAPEFILE = "fsl-w88d.shp";

    private static final String NDM_DIR_NAME = "ndm" + IPathManager.SEPARATOR;

    private static final int WGS84_SRID = 4326;

    /** The list of the required files comprising a Shapefile set */
    private static final String[] SHAPE_FILES = new String[] { "fsl-w88d.shp",
            "fsl-w88d.shx", "fsl-w88d.dbf" };

    /**
     * The collection of feature attribute names this code needs from the
     * shapefile
     */
    private enum databaseColumns {
        LAT, EQP_ELV, NAME, LON, IMMUTABLEX, RDA_ID, ELEVMETER, THE_GEOM, WFO_ID, RPG_ID_DEC
    }

    private final long theTimeRange = 3600000l;

    /** The file name of the file currently being processed. */
    private String fileName = null;

    /** The date, in millis, of the file currently being processed. */
    private long theCurrentFileDateMillis = 0;

    /**
     * The list of RDA_ID's from the shapefile used to determine if an ID has
     * been removed from the database
     */
    private ArrayList<String> theRDAList = null;

    private IPathManager pathMgr = null;

    private LocalizationContext commonConfiguredCtx = null;

    @Override
    public void notify(String aFileName, File aFile) {
        pathMgr = PathManagerFactory.getPathManager();
        commonConfiguredCtx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        fileName = aFileName;
        // Read file from NDM directory and move to Common directory
        ILocalizationFile ndmDirFile = getPathInfoWrite();

        saveFile(aFile, ndmDirFile);

        if (allFilesAvailable()) {
            // Start the ShapeFile update
            updateRadarFromShapeFiles();
        }
    }

    /**
     * Adaptor method to use theFileName attribute.
     * 
     * @return LocalizationFile
     */
    private ILocalizationFile getPathInfoWrite() {
        ILocalizationFile ndmDir = pathMgr.getLocalizationFile(
                commonConfiguredCtx, NDM_DIR_NAME + fileName);

        return ndmDir;
    }

    /**
     * Returns the localization path information for the NDM filename passed in.
     * 
     * @return LocalizationFile
     */
    private File getPathInfoRead(String aFileName) {
        File file = pathMgr.getFile(commonConfiguredCtx, NDM_DIR_NAME
                + aFileName);
        return file;
    }

    /**
     * Copies the file from NDM ingest directory to the landing zone
     * 
     * @param file
     * @param outFile
     */
    private void saveFile(File file, ILocalizationFile outFile) {
        if ((file != null) && file.exists()) {
            try {
                byte[] fileByteArray = Files.readAllBytes(Paths.get(file
                        .getAbsolutePath()));

                try (OutputStream os = outFile.openOutputStream()) {
                    os.write(fileByteArray);
                }

                // / Add the outFile's time stamp to check later on.
                theCurrentFileDateMillis = System.currentTimeMillis();

            } catch (FileNotFoundException e) {
                if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to find file: " + file.getName(), e);
                }
            } catch (IOException e) {
                if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Could not create output file: "
                                    + outFile.getPath(), e);
                }
            } catch (LocalizationException e) {
                if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Failed to write localization file: "
                                    + outFile.getPath(), e);
                }
            }
        }
    }

    /**
     * Determines if all the files required for a Shapefile are present and
     * within a given time threshold. For this method the files are
     * "fsl-w88d(.shp,.shx,.dbf)"
     * 
     * @return
     */
    private boolean allFilesAvailable() {
        File tempFile = null;
        long timeStampMillis = 0;
        long differenceMillis = 0;
        boolean rVal = false;
        ArrayList<String> badList = new ArrayList<>();
        ArrayList<String> goodList = new ArrayList<>();

        // Determine if the two other files are available or if the time
        // threshold has been crossed and are too old to process
        for (String key : SHAPE_FILES) {
            if (!key.equals(fileName)) {
                tempFile = getPathInfoRead(key);
                timeStampMillis = tempFile.lastModified();
                differenceMillis = theCurrentFileDateMillis - timeStampMillis;

                if ((timeStampMillis != 0L)
                        && (differenceMillis <= theTimeRange)) {
                    goodList.add(key);
                } else {
                    badList.add(key);
                }
            }
        }

        if (goodList.size() == 2) {
            rVal = true;
        } else {
            // Log what files are "missing"
            String logMessage = null;
            String header = "Received file [" + fileName
                    + "] still require file(s) [" + badList.get(0);
            if (badList.size() == 2) {
                logMessage = header + ", " + badList.get(1) + "]";
            } else {
                logMessage = header + "]";
            }
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO, logMessage);
            }
        }
        return rVal;
    }

    private void updateRadarFromShapeFiles() {
        SimpleFeature simpleFeature = null;
        List<HashMap<String, String>> dataRows = new ArrayList<>();
        HashMap<String, String> nameValuePair = null;
        Collection<Property> propCollection = null;
        String name = null;
        String value = null;
        List<RadarStation> stationList = new ArrayList<>();

        File tempFile = getPathInfoRead(SHAPEFILE);
        Shapefile shapefile = new Shapefile(tempFile);
        if (shapefile.readShapefile()) {
            Set<String> featureIDs = shapefile.getFeatureIDs();
            for (String id : featureIDs) {
                simpleFeature = shapefile.getFeature(id);
                propCollection = simpleFeature.getProperties();
                nameValuePair = new HashMap<>();
                for (Property property : propCollection) {
                    name = property.getDescriptor().getName().getLocalPart();
                    value = property.getValue().toString();
                    nameValuePair.put(name, value);
                }
                dataRows.add(nameValuePair);
            }
            stationList = buildStations(dataRows);
            updateDatabase(stationList);
            checkForMissingStations();

        } else {
            /**
             * TODO: Add code to handle when file could not be read.
             */
        }
    }

    private void updateDatabase(List<RadarStation> aStationList) {
        RadarStationDao radarStationDAO = new RadarStationDao();

        for (RadarStation station : aStationList) {
            radarStationDAO.saveOrUpdate(station);
        }

        /*
         * Kludge for DR 15366: The GeoTools WKBWriter does not store SRIDs so
         * we must update them manually.
         * 
         * Once GetTools is updated/fixed, this should be removed.
         */
        try {
            radarStationDAO.setSridOnAllRadarStation();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to update the SRIDs in the radar_spatial table", e);
        }

        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            statusHandler
                    .handle(Priority.INFO,
                            "The NDM update of database table RADAR_SPATIAL has been completed successfully");
        }
    }

    /**
     * Using the list of RDA IDs from the shapefile determine if one or more
     * stations are missing and log a warning message for the missing stations
     */
    private void checkForMissingStations() {
        RadarStationDao dao = new RadarStationDao();
        List<String> currentList = dao.getRDA_IDs();
        currentList.removeAll(theRDAList);
        if (currentList.size() > 0) {
            for (String rda_id : currentList) {
                if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "The RADAR_SPATIAL database table for the row with a RDA_ID of ["
                                            + rda_id
                                            + "] needs to be removed per latest NDM update");

                }
            }
        }
    }

    private List<RadarStation> buildStations(
            List<HashMap<String, String>> aDataList) {
        List<RadarStation> rVal = new ArrayList<>();
        theRDAList = new ArrayList<>();
        String rda_id = null;
        RadarStation tempStation = null;
        Set<String> keySet = null;
        GeometryFactory gf = new GeometryFactory(new PrecisionModel(),
                WGS84_SRID);
        WKTReader wkt = new WKTReader(gf);
        for (HashMap<String, String> aHashMap : aDataList) {
            keySet = aHashMap.keySet();
            tempStation = new RadarStation();
            for (String key : keySet) {
                switch (databaseColumns.valueOf(key.toUpperCase())) {
                case LAT:
                    tempStation.setLat(Float.valueOf(aHashMap.get(key)));
                    break;
                case EQP_ELV:
                    tempStation.setEqpElv(Float.valueOf(aHashMap.get(key)));
                    break;
                case NAME:
                    tempStation.setName(aHashMap.get(key));
                    break;
                case LON:
                    tempStation.setLon(Float.valueOf(aHashMap.get(key)));
                    break;
                case IMMUTABLEX:
                    tempStation.setImmutablEx(Float.valueOf(aHashMap.get(key)));
                    break;
                case RDA_ID:
                    rda_id = aHashMap.get(key);
                    tempStation.setRdaId(rda_id);
                    // Add the rda_id to a list in order to determine if a
                    // rda_id has been removed from the database
                    theRDAList.add(rda_id);
                    break;
                case ELEVMETER:
                    tempStation.setElevMeter(Float.valueOf(aHashMap.get(key)));
                    break;
                case THE_GEOM:
                    try {
                        tempStation.setStation((Point) wkt.read(aHashMap
                                .get(key)));
                    } catch (ParseException e) {
                        if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage() + e);
                        }
                    }
                    break;
                case WFO_ID:
                    tempStation.setWfoId(aHashMap.get(key));
                    break;
                case RPG_ID_DEC:
                    tempStation.setRpgIdDec(aHashMap.get(key));
                    break;
                default:
                    if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unknown token [" + key + "}");
                    }
                    break;
                }
                rVal.add(tempStation);
            }
        }
        return rVal;
    }
}
