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
package com.raytheon.uf.viz.datadelivery.filter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterSettingsXML;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterTypeXML;

/**
 * Metadata manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 01, 2012            mpduff       Initial creation
 * Jun 21, 2012 736        djohnson     Use queries to perform DataSet filtering.
 * Jul 24, 2012 955        djohnson     Matching datasets are returned in a {@link Set}.
 * Aug 02, 2012 955        djohnson     Type-safe registry query/responses.
 * Aug 10, 2012 1022       djohnson     Store provider name in {@link SubsetXml},  use {@link GriddedDataSet}.
 * Aug 20, 2012 0743       djohnson     Use {@link ITimer} to time operations, use DataSet.
 * Aug 28, 2012 1022       djohnson     Speed up filter retrieval by using worker threads.
 * Oct 05, 2012 1241       djohnson     Replace RegistryManager calls with registry handler calls.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class MetaDataManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetaDataManager.class);

    private static MetaDataManager instance = new MetaDataManager();

    /**
     * Get an instance of this class.
     * 
     * @return an instance
     */
    public static MetaDataManager getInstance() {
        return instance;
    }

    // These collection variables must be volatile so we can safely publish new
    // references from a multi-threaded call
    private volatile SortedSet<String> allAvailableDataSets;

    private volatile SortedSet<String> allAvailableProviders;

    private volatile SortedSet<String> allAvailableLevels;

    private volatile SortedSet<String> allAvailableParameters;

    /**
     * envelope of selected area.
     */
    private ReferencedEnvelope envelope;

    private boolean reread = true;

    private String[] dataTypes;

    /**
     * Private constructor.
     */
    private MetaDataManager() {
    }

    /**
     * Get all available Data Providers
     * 
     * @return all data providers
     */
    public SortedSet<String> getAvailableDataProviders() {
        if (allAvailableProviders == null) {
            allAvailableProviders = new TreeSet<String>();

            try {
                for (Provider provider : DataDeliveryHandlers
                        .getProviderHandler().getAll()) {
                    allAvailableProviders.add(provider.getName());
                }
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve the list of providers.", e);
            }
        }

        return allAvailableProviders;
    }

    /**
     * Get all available data sets
     * 
     * @return all data sets
     */
    public SortedSet<String> getAvailableDataSets() {
        if (allAvailableDataSets == null) {
            allAvailableDataSets = new TreeSet<String>();

            try {
                allAvailableDataSets.addAll(DataDeliveryHandlers
                        .getDataSetNameHandler().getByDataTypes(
                                Arrays.asList(dataTypes)));
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve the dataset list.", e);
            }
        }

        return allAvailableDataSets;
    }

    public List<String> getAvailableDataTypes() {
        Set<String> typeSet = new TreeSet<String>();

        ITimer timer = TimeUtil.getTimer();
        timer.start();

        try {
            for (Provider provider : DataDeliveryHandlers.getProviderHandler()
                    .getAll()) {

                for (ProviderType type : provider.getProviderType()) {
                    typeSet.add(type.toString());
                }
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve the provider list.", e);
        }

        List<String> typeList = new ArrayList<String>(typeSet);
        timer.stop();

        return typeList;
    }

    /**
     * Get all available levels
     * 
     * @return available levels
     */
    public SortedSet<String> getAvailableLevels() {
        if (allAvailableLevels == null) {
            allAvailableLevels = new TreeSet<String>();

            try {
                allAvailableLevels.addAll(DataDeliveryHandlers
                        .getParameterHandler().getDataLevelTypeDescriptions(
                                Arrays.asList(dataTypes)));
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve the levels.", e);
            }
        }

        return allAvailableLevels;
    }

    /**
     * Get all available parameters
     * 
     * @return all available parameters
     */
    public SortedSet<String> getAvailableParameters() {
        if (allAvailableParameters == null) {
            allAvailableParameters = new TreeSet<String>();

            try {
                allAvailableParameters.addAll(DataDeliveryHandlers
                        .getParameterHandler().getNamesByDataTypes(
                                Arrays.asList(dataTypes)));
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve the parameter list.", e);
            }
        }
        return allAvailableParameters;
    }

    public DataSet getDataSet(String dataSetName, String providerName) {
        try {
            return DataDeliveryHandlers.getDataSetHandler()
                    .getByNameAndProvider(dataSetName, providerName);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve dataset.", e);
            return null;
        }
    }

    /**
     * Get the data sets that match the filter criteria.
     * 
     * @param xml
     *            The FilterSettingsXML filter settings
     * 
     * @return DataSet objects that match the filter criteria
     */
    public Set<DataSet> getMatchingDataSets(FilterSettingsXML xml) {
        Set<DataSet> filteredDataSets = new HashSet<DataSet>();

        // Get the filter types
        ArrayList<FilterTypeXML> filterTypeList = xml.getFilterTypeList();

        HashMap<String, ArrayList<String>> filterMap = new HashMap<String, ArrayList<String>>();

        // Now filter the available data values
        for (FilterTypeXML filterType : filterTypeList) {
            if (filterType.getValues().size() > 0) {
                filterMap.put(filterType.getFilterType(),
                        filterType.getValues());
            }
        }

        List<String> providers = null;
        List<String> dataSetNames = null;
        Set<LevelType> levels = null;
        List<String> parameterNames = null;

        // iterate over the filters
        for (String filterType : filterMap.keySet()) {
            List<String> values = filterMap.get(filterType);

            // If there are no values to filter on, just continue to the next
            // filter
            if (values == null) {
                continue;
            }

            if (filterType.equalsIgnoreCase("Data Provider")) {
                providers = values;
            } else if (filterType.equalsIgnoreCase("Data Set")) {
                dataSetNames = values;
            } else if (filterType.equalsIgnoreCase("Level")) {
                levels = new HashSet<LevelType>(values.size());
                for (String value : values) {
                    levels.add(LevelType.fromDescription(value));
                }
            } else if (filterType.equalsIgnoreCase("Parameter")) {
                parameterNames = values;
            }
        }

        try {
            filteredDataSets.addAll(DataDeliveryHandlers.getDataSetHandler()
                    .getByFilters(providers, dataSetNames, levels,
                            parameterNames, envelope));
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve the filtered datasets.", e);
        }

        return filteredDataSets;
    }

    /**
     * Get the metadata
     * 
     * @param dataType
     */
    public void readMetaData(String dataType) {
        if (reread) {
            dataTypes = new String[] { dataType };

            reread = false;
            allAvailableDataSets = null;
            allAvailableLevels = null;
            allAvailableParameters = null;
            allAvailableProviders = null;

            retrieveNewMetadata();
        }
    }

    public void rereadMetaData() {
        reread = true;
    }

    /**
     * Uses multiple threads to speed up the retrieval of new metadata
     * information. This reduces the loading of new metadata from the cumulative
     * time of all queries to the max time of the queries.
     */
    private void retrieveNewMetadata() {

        Runnable loadProviders = new Runnable() {
            @Override
            public void run() {
                getAvailableDataProviders();
            }
        };

        Runnable loadDataSets = new Runnable() {
            @Override
            public void run() {
                getAvailableDataSets();
            }
        };

        Runnable loadParameters = new Runnable() {
            @Override
            public void run() {
                getAvailableParameters();
            }
        };

        Runnable loadLevels = new Runnable() {
            @Override
            public void run() {
                getAvailableLevels();
            }
        };

        Runnable[] runnables = new Runnable[] { loadProviders, loadDataSets,
                loadParameters, loadLevels };
        FutureTask<?>[] tasks = new FutureTask[runnables.length];
        ExecutorService executor = Executors
                .newFixedThreadPool(runnables.length);
        for (int i = 0; i < runnables.length; i++) {
            FutureTask<Void> task = new FutureTask<Void>(runnables[i], null);
            tasks[i] = task;
            executor.submit(task);
        }

        for (FutureTask<?> task : tasks) {
            try {
                task.get();
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN, "Unable to load metadata",
                        e);
            }
        }
    }

    public void setArea(ReferencedEnvelope envelope) {
        this.envelope = envelope;
    }
}
