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
package com.raytheon.uf.edex.plugin.loctables.ingest;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.ndm.ingest.IDataSetIngester;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * Location Tables NDM subscriber
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 08, 2010            jkorman     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * Apr 28, 2014   3086     skorolev    Updated setupLocalFiles method
 * Sep 04, 2014   3220     skorolev    Removed parameter currentSite from FSSObs configuration managers.
 * Sep 03, 2015   3841     skorolev    Corrected getInstance for FSSObs monitors.
 * Oct 12, 2015   4911     rjpeter     Updated to reload all location data and diff table as a whole.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class LocationTablesIngest implements INationalDatasetSubscriber {

    private static final String NDM_LOC_DIR = "spatialTables" + File.separator;

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final Map<String, TableHandler> handlers;

    private final IDataSetIngester ndmIngester;

    /**
     * Location Tables Ingest.
     * 
     * @param pluginName
     * @param ingester
     */
    public LocationTablesIngest(IDataSetIngester ingester) {
        ndmIngester = ingester;

        // always want to process in same order
        handlers = new LinkedHashMap<String, TableHandler>();
    }

    public INationalDatasetSubscriber registerHandler(String fileName,
            TableHandler handler) {
        handlers.put(fileName, handler);
        return ndmIngester.registerListener(fileName, this);
    }

    /**
     * Accept INationalDatasetSubscriber notifications.
     */
    @Override
    public void notify(String fileName, File file) {
        if (handlers.containsKey(fileName)) {
            processFile(file);
        } else {
            logger.warn("No handler exists for file [" + fileName + "]");
        }

    }

    /**
     * 
     * @param file
     * @return
     */
    public synchronized void processFile(File file) {
        try {
            storeNdmFile(file);
        } catch (Exception e) {
            logger.error(
                    "Update of common_obs_spatial cancelled.  Failed to store "
                            + file.getPath() + " to localization", e);
            return;
        }

        try {
            Map<String, ObStationRow> gidMap = new HashMap<String, ObStationRow>();

            for (Map.Entry<String, TableHandler> entry : handlers.entrySet()) {
                LocalizationFile locFile = getSpatialFile(entry.getKey());
                List<ObStationRow> stations = entry.getValue().process(locFile);
                if (stations != null) {
                    addStations(gidMap, stations);
                }
            }

            checkICAOs(gidMap);

            // persist the gidMap
            ObStationDao dao = new ObStationDao();
            LocationTablesProcessor proc = new LocationTablesProcessor(dao,
                    gidMap);
            DatabaseQuery query = new DatabaseQuery(ObStation.class);
            dao.processByCriteria(query, proc);
            logger.info(String
                    .format("Processing of file [%s] Complete.  Stations Added/Updated/Deleted: [%d/%d/%d]",
                            file.getName(), proc.getStationsAdded(),
                            proc.getStationsUpdated(),
                            proc.getStationsDeleted()));
        } catch (Exception e) {
            logger.error("Error occurred processing file: " + file.getName(), e);
        }
    }

    protected void addStations(Map<String, ObStationRow> gidMap,
            List<ObStationRow> stations) {
        for (ObStationRow station : stations) {
            if (station != null) {
                String key = station.getGid();
                if (!gidMap.containsKey(key)) {
                    gidMap.put(key, station);
                }

                if (ObStation.CAT_TYPE_SFC_RAOB
                        .equals(station.getCatalogType())) {
                    // check for fixed land for this raob
                    key = ObStation.createGID(ObStation.CAT_TYPE_SFC_FXD,
                            station.getStationId());
                    if (gidMap.containsKey(key)) {
                        ObStationRow aggregate = gidMap.get(key);
                        aggregate.setUpperAirElevation(station
                                .getUpperAirElevation());
                        aggregate.setUpperAirGeometry(station
                                .getUpperAirGeometry());
                        if (aggregate.getIcao() == null) {
                            aggregate.setIcao(station.getIcao());
                        }
                    }
                } else if (ObStation.CAT_TYPE_SFC_FXD.equals(station
                        .getCatalogType())) {
                    // check for raob for this fixed land
                    key = ObStation.createGID(ObStation.CAT_TYPE_SFC_RAOB,
                            station.getStationId());
                    if (gidMap.containsKey(key)) {
                        ObStationRow aggregate = gidMap.get(key);
                        station.setUpperAirElevation(aggregate
                                .getUpperAirElevation());
                        station.setUpperAirGeometry(aggregate
                                .getUpperAirGeometry());
                        if (station.getIcao() == null) {
                            station.setIcao(aggregate.getIcao());
                        }
                    }
                }
            }
        }
    }

    /**
     * Post process all fixed station types and add an associated ICAO entry if
     * it doesn't exist.
     */
    private void checkICAOs(Map<String, ObStationRow> gidMap) {
        List<ObStationRow> newStations = new ArrayList<>();

        for (ObStationRow row : gidMap.values()) {
            if (ObStation.CAT_TYPE_SFC_FXD.equals(row.getCatalogType())) {
                // This synoptic has an associated ICAO, check to see if it is
                // in the ICAOs
                String icao = row.getIcao();
                if (icao != null) {
                    String key = ObStation.createGID(ObStation.CAT_TYPE_ICAO,
                            icao);
                    if (!gidMap.containsKey(key)) {
                        ObStationRow icaoRow = new ObStationRow(
                                ObStation.CAT_TYPE_ICAO);
                        icaoRow.setIcao(icao);
                        icaoRow.setStationId(icao);
                        icaoRow.setWmoIndex(row.getWmoIndex());
                        icaoRow.setWmoRegion(row.getWmoRegion());

                        icaoRow.setCountry(row.getCountry());
                        icaoRow.setState(row.getState());

                        icaoRow.setElevation(row.getElevation());
                        icaoRow.setLocation(row.getLocation());

                        newStations.add(icaoRow);
                    }
                }
            }
        }

        for (ObStationRow newStation : newStations) {
            gidMap.put(newStation.getGid(), newStation);
        }
    }

    /**
     * Store the given ndm file in the localization directory.
     * 
     * @param file
     * @throws IOException
     * @throws LocalizationException
     */
    protected void storeNdmFile(File file) throws IOException,
            LocalizationException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.CONFIGURED);
        LocalizationFile locFile = pm.getLocalizationFile(context, NDM_LOC_DIR
                + file.getName());

        try (SaveableOutputStream out = locFile.openOutputStream();
                InputStream in = new FileInputStream(file)) {
            FileUtil.copy(in, out);
            out.save();
        }
    }

    /**
     * Get a list of the lines of the given file.
     * 
     * @param fileName
     * @return a list of the lines of the file
     */
    protected LocalizationFile getSpatialFile(String fileName) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile file = pm.getStaticLocalizationFile(
                LocalizationType.EDEX_STATIC, NDM_LOC_DIR + fileName);
        return file;
    }
}
