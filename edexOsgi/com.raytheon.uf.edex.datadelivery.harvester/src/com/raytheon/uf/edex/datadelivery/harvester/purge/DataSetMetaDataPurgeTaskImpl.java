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
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Multimap;
import com.google.common.collect.Ordering;
import com.google.common.collect.TreeMultimap;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.IDataSetMetaDataVisitor;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.harvester.config.Agent;
import com.raytheon.uf.edex.datadelivery.harvester.config.CrawlAgent;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.CrawlLauncher;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class DataSetMetaDataPurgeTaskImpl implements IDataSetMetaDataPurgeTask,
        IDataSetMetaDataVisitor {

    /**
     * Maintains state for an instance of the purge task.
     */
    private static class State {
        /**
         * This boolean flag is used to mark whether or not the DataSetMetaData
         * group should be continued, it will be set to false when the purge has
         * found a DataSetMetaData instance that should NOT be purged
         */
        private boolean continueWithDataSet = true;

        /**
         * The harvester configurations instance at the time the purge started.
         */
        private List<HarvesterConfig> harvesterConfigs;
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataSetMetaDataPurgeTaskImpl.class);

    private static final IOpenDapGriddedPurge GRIDDED_OPENDAP = new OpenDapGriddedPurgeImpl();

    /**
     * This is the unique identifying key for this metadata's dataset in the
     * map.
     * 
     * @param metaData
     *            the metaDat
     * @return the key
     */
    @VisibleForTesting
    static String getDatasetMetaDataMapKey(DataSetMetaData metaData) {
        return metaData.getDataSetName() + metaData.getProviderName();
    }

    /**
     * Purges a {@link DataSetMetaData} instance.
     * 
     * @param metaData
     *            the metadata
     * @throws NullPointerException
     *             if the metadata passed in is null
     */
    @VisibleForTesting
    static void purgeMetaData(DataSetMetaData metaData) {

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

    private final IOpenDapGriddedPurge openDapGriddedPurge;

    // Used to maintain state on a per-thread basis, in case two purges somehow
    // overrun each other
    private final ThreadLocal<State> threadState = new ThreadLocal<State>();

    /**
     * Default Constructor.
     */
    DataSetMetaDataPurgeTaskImpl() {
        this(GRIDDED_OPENDAP);
    }

    /**
     * Constructor accepting specific purge strategies.
     * 
     * @param openDapGriddedPurge
     *            openDapGriddedPurge
     * 
     */
    @VisibleForTesting
    DataSetMetaDataPurgeTaskImpl(IOpenDapGriddedPurge openDapGriddedPurge) {
        this.openDapGriddedPurge = openDapGriddedPurge;
    }

    /**
     * Clears the state for a running instance.
     */
    private void clearState() {
        threadState.set(null);
    }

    /**
     * Returns all {@link DataSetMetaData} instances that are to be checked for
     * validity.
     * 
     * @return the {@link DataSetMetaData} instances
     */
    @VisibleForTesting
    List<DataSetMetaData> getDataSetMetaDatas() {
        try {
            return DataDeliveryHandlers.getDataSetMetaDataHandler().getAll();
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve DataSetMetaData instances!", e);
            return Collections.emptyList();
        }
    }

    /**
     * Creates a map from the DataSetMetaData key as defined by
     * {@link #getDatasetMetaDataMapKey(DataSetMetaData)} to the
     * {@link SortedSet} of instances.
     * 
     * @return the map
     */
    @VisibleForTesting
    Multimap<String, DataSetMetaData> getDataSetNameKeyedInstanceMap() {
        Multimap<String, DataSetMetaData> map = TreeMultimap.create(
                Ordering.<String> natural(), DataSetMetaData.DATE_COMPARATOR);

        for (DataSetMetaData metaData : getDataSetMetaDatas()) {
            String key = getDatasetMetaDataMapKey(metaData);
            map.put(key, metaData);
        }

        return map;
    }

    /**
     * Returns the HarvesterConfig Array from localization.
     * 
     * @return the {@link HarvesterConfig}
     */
    @VisibleForTesting
    List<HarvesterConfig> getHarvesterConfigs() {

        // first get the Localization directory and find all harvester
        // configs
        List<HarvesterConfig> configs = new ArrayList<HarvesterConfig>();

        // if many, start many
        for (LocalizationFile lf : CrawlLauncher.getLocalizedFiles()) {

            HarvesterConfig hc = null;
            try {
                hc = SerializationUtil.jaxbUnmarshalFromXmlFile(
                        HarvesterConfig.class, lf.getFile());
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

        return configs;
    }

    /**
     * This method consolidates the logic of applying a purge strategy for a
     * specific data type and service (e.g. OpenDAP for Gridded data) on a
     * specific {@link DataSetMetaData} of that type. The generics ensure strict
     * adherence to the data type mappings.
     * 
     * @param <T>
     *            the type that extends DataSetMetaData
     * @param metaData
     *            the metadata instance
     * @param purge
     *            the purge strategy
     */
    private <T extends DataSetMetaData> void handleVisit(T metaData,
            IServiceDataSetMetaDataPurge<T> purge) {
        State state = threadState.get();
        List<HarvesterConfig> harvesterConfigs = state.harvesterConfigs;

        for (HarvesterConfig config : harvesterConfigs) {

            if (purge.isTimeToPurge(metaData, config)) {
                purgeMetaData(metaData);
            } else {
                // Found a non-purgeable metadata instance
                state.continueWithDataSet = false;
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    final String id = RegistryUtil
                            .getRegistryObjectKey(metaData);
                    statusHandler
                            .debug(String
                                    .format("Provider: "
                                            + config.getProvider().getName()
                                            + " : DataSetMetaData with id [%s] does not require purging.",
                                            id));
                }
            }
        }
    }

    /**
     * Initializes the state for a running instance.
     * 
     * @return the State instance
     */
    @VisibleForTesting
    State initializeState() {
        State state = new State();
        state.harvesterConfigs = getHarvesterConfigs();
        threadState.set(state);

        return state;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void run() {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        Multimap<String, DataSetMetaData> dataSetKeyedMap = getDataSetNameKeyedInstanceMap();

        try {
            State state = initializeState();

            for (String key : dataSetKeyedMap.keySet()) {
                Collection<DataSetMetaData> metaDatas = dataSetKeyedMap
                        .get(key);
                Iterator<DataSetMetaData> iter = metaDatas.iterator();

                state.continueWithDataSet = true;
                while (iter.hasNext() && state.continueWithDataSet) {
                    DataSetMetaData metaData = iter.next();
                    metaData.accept(this);
                }
            }
        } finally {
            clearState();
        }

        timer.stop();
        statusHandler.info(String.format(
                "DataSetMetaData purge completed in %s ms.",
                timer.getElapsedTime()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void visit(OpenDapGriddedDataSetMetaData metaData) {
        handleVisit(metaData, openDapGriddedPurge);
    }
}
