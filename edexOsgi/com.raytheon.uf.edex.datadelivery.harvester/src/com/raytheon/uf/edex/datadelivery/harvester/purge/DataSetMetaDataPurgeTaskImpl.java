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
package com.raytheon.uf.edex.datadelivery.harvester.purge;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.harvester.Agent;
import com.raytheon.uf.common.datadelivery.harvester.CrawlAgent;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfig;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfigurationManager;
import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.CrawlLauncher;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * Purges {@link DataSetMetaData} instances that are no longer accessible on
 * their originating servers. This class performs the wiring up of specific
 * {@link DataSetMetaData} types with their {@link IServiceDataSetMetaDataPurge}
 * implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 04, 2012 1102       djohnson     Initial creation
 * Oct 05, 2012 1241       djohnson     Replace RegistryManager calls with registry handler calls.
 * Dec 12, 2012 1410       dhladky      multi provider configurations.
 * Sept 30, 2013 1797      dhladky      Generics
 * Apr 12,2014   3012     dhladky      Purge never worked, fixed to make work.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DataSetMetaDataPurgeTaskImpl implements IDataSetMetaDataPurgeTask {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataSetMetaDataPurgeTaskImpl.class);


    /** Data access object for registry objects */
    private RegistryObjectDao rdo;
    
    /**
     * Purges a {@link DataSetMetaData} instance.
     * 
     * @param metaData
     *            the metadata
     * @throws NullPointerException
     *             if the metadata passed in is null
     */
    @VisibleForTesting
    static void purgeMetaData(DataSetMetaData<?> metaData) {

        checkNotNull(metaData, "metaData must not be null!");

        statusHandler.info(String.format(
                "Purging DataSetMetaData for url [%s]", metaData.getUrl()));
        try {
            DataDeliveryHandlers.getDataSetMetaDataHandler().delete(metaData);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to delete a DataSetMetaData instance!", e);
        }
    }

    /**
     * Default Constructor.
     */
    public DataSetMetaDataPurgeTaskImpl(RegistryObjectDao rdo) {
        this.rdo = rdo;
    }
  
    /**
     * Gets the entire list of DSMD ids from the registry.
     * 
     * @return the map
     */
    @VisibleForTesting
    List<String> getDataSetMetaDataIds() {
        ArrayList<String> ids = null;
        try {
            // Gets the list of all available lids for current DataSetMetaData objects
            ids = (ArrayList<String>) rdo.getRegistryObjectIdsOfType(DataDeliveryRegistryObjectTypes.DATASETMETADATA);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve DataSetMetaData ids!", e);
            return Collections.emptyList();
        }

        return ids;
    }

    /**
     * Returns the Retention times by Provider name.
     * 
     * @return the {@link HarvesterConfig}
     */
    @VisibleForTesting
    static Map<String, String> getHarvesterConfigs() {

        // first get the Localization directory and find all harvester
        // configs
        List<HarvesterConfig> configs = new ArrayList<HarvesterConfig>();

        // if many, start many
        for (LocalizationFile lf : CrawlLauncher.getLocalizedFiles()) {

            HarvesterConfig hc = null;
            try {
                hc = HarvesterConfigurationManager.getHarvesterFile(lf.getFile());
            } catch (Exception se) {
                statusHandler.handle(Priority.PROBLEM,
                        se.getLocalizedMessage(), se);
            }

            if (hc != null) {
                if (hc.getAgent() != null) {
                    // we only want crawler types for CrawlerMetadata
                    Agent agent = hc.getAgent();
                    if (agent instanceof CrawlAgent) {
                        // collect file
                        configs.add(hc);
                    }
                }
            }
        }
        
        Map<String, String> configMap = null;

        if (!configs.isEmpty()) {
            configMap = new HashMap<String, String>(
                    configs.size());
            for (HarvesterConfig config : configs) {
                configMap.put(config.getProvider().getName(), config.getRetention());
            }
        } else {
            return Collections.emptyMap();
        }

        return configMap;
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public void run() {

        ITimer timer = TimeUtil.getTimer();
        timer.start();

        List<String> idList = getDataSetMetaDataIds();
        Map<String, String> configMap = getHarvesterConfigs();
        int deletes = 0;
        
        for (String id : idList) {

            try {

                DataSetMetaData<?> metaData = DataDeliveryHandlers
                        .getDataSetMetaDataHandler().getById(id);
                Integer retention = Integer.valueOf(configMap.get(metaData.getProviderName()));

                if (retention != null) {

                    if (retention == -1) {
                        // no purging for this DSMD type
                        continue;
                    } else {
                        // retention is in days
                        retention = retention * (-1);
                        // we are subtracting from current
                        Calendar thresholdTime = TimeUtil.newGmtCalendar();
                        thresholdTime.add(Calendar.DAY_OF_YEAR, retention);

                        if (thresholdTime.getTimeInMillis() >= metaData
                                .getDate().getTime()) {
                            purgeMetaData(metaData);
                            deletes++;
                        }
                    }

                } else {
                    statusHandler
                            .warn("No retention time set for this DataSetMetaData provider! "
                                    + id
                                    + "Provider: "
                                    + metaData.getProviderName());
                }

            } catch (Exception e) {
                statusHandler.error("DataSetMetaData purge failed! " + id, e);
            }
        }

        timer.stop();
        statusHandler.info(String.format(
                "DataSetMetaData purge completed in %s ms.",
                timer.getElapsedTime()+" deleted: "+deletes));
    }
}
