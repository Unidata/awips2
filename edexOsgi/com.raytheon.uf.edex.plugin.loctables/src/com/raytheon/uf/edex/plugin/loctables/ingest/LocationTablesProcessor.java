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

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.database.processor.IDatabaseProcessor;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * Processor for ObStation and ObStationRow differencing. Takes what the
 * common_obs_spatial table should be and applies all adds/updates/deletes to
 * match the expected view.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2015  4911      rjpeter     Initial creation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class LocationTablesProcessor implements IDatabaseProcessor<ObStation> {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final ObStationDao dao;

    private final Map<String, ObStationRow> gidMap;

    private final Set<ObStation> stationsToUpdate = new HashSet<>();

    private final Set<ObStation> stationsToDelete = new HashSet<>();

    private int batchSize = 100;

    private int stationsAdded = 0;

    private int stationsUpdated = 0;

    private int stationsDeleted = 0;

    public LocationTablesProcessor(ObStationDao dao,
            Map<String, ObStationRow> gidMap) {
        this.dao = dao;
        this.gidMap = gidMap;
    }

    @Override
    public boolean process(ObStation currentStation) throws Exception {
        // remove the entry from gidMap so that it won't be further processed
        ObStationRow updatedStation = gidMap.remove(currentStation.getGid());

        if (updatedStation == null) {
            stationsToDelete.add(currentStation);
        } else if (updatedStation.requiresUpdate(currentStation)) {
            stationsToUpdate.add(currentStation);
        }

        return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void finish() throws Exception {
        /*
         * persist changes, can't be done during process due to clearing of
         * hibernate session
         */
        for (ObStationRow station : gidMap.values()) {
            logger.info("Adding station: " + station.getGid());
            dao.create(station.toObStation());
            stationsAdded++;
        }
        for (ObStation station : stationsToUpdate) {
            logger.info("Updating station: " + station.getGid());
            dao.update(station);
            stationsUpdated++;
        }
        for (ObStation station : stationsToDelete) {
            logger.info("Removing station: " + station.getGid());
            dao.delete(station);
            stationsDeleted++;
        }
    }

    @Override
    public int getBatchSize() {
        return batchSize;
    }

    @Override
    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
    }

    /**
     * @return the stationsAdded
     */
    public int getStationsAdded() {
        return stationsAdded;
    }

    /**
     * @return the stationsUpdated
     */
    public int getStationsUpdated() {
        return stationsUpdated;
    }

    /**
     * @return the stationsDeleted
     */
    public int getStationsDeleted() {
        return stationsDeleted;
    }

}
