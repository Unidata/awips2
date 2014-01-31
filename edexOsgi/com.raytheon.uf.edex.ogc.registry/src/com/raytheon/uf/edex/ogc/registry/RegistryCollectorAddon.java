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
package com.raytheon.uf.edex.ogc.registry;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfig;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfigurationManager;
import com.raytheon.uf.common.datadelivery.harvester.OGCAgent;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataSetName;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ogc.common.db.ICollectorAddon;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;

/**
 * Collector Used to gather data with DPA, used for AWIPS registry data feeds from providers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2013            bclement     Initial creation
 * Aug 08, 2013            dhladky      Made operational
 * Jan 13, 2014  #2679     dhladky      multiple layers
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class RegistryCollectorAddon<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        implements ICollectorAddon<D, L, R> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryCollectorAddon.class);

    protected HarvesterConfig config = null;

    protected OGCAgent agent = null;

    protected Map<String, Parameter> parameters = null;

    /**
	 * 
	 */
    public RegistryCollectorAddon() {
        this.config = HarvesterConfigurationManager.getOGCConfiguration();
        setAgent((OGCAgent) config.getAgent());
        storeProvider(config.getProvider());
    }

    public HarvesterConfig getConfig() {
        return config;
    }

    public void setConfig(HarvesterConfig config) {
        this.config = config;
    }

    /**
     * Find the DPA config
     * 
     * @return
     */
    public HarvesterConfig getConfiguration() {
        return config;
    }

    /**
     * Stroe Data objects
     * 
     * @param metaDatas
     * @param dataSet
     */
    public void storeMetaData(final DataSetMetaData<?> metaData) {

        IDataSetMetaDataHandler handler = DataDeliveryHandlers
                .getDataSetMetaDataHandler();

        final String description = metaData.getDataSetDescription();
        statusHandler.info("Attempting store of DataSetMetaData[" + description
                + "]");

        try {
            handler.update(metaData);
            statusHandler.info("DataSetMetaData [" + description
                    + "] successfully stored in Registry");
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM, "DataSetMetaData ["
                    + description + "] failed to store in Registry");

        }
    }

    /**
     * 
     * @param dataSetToStore
     */
    protected void storeDataSetName(DataSet<?, ?> dataSetToStore) {

        DataSetName dsn = new DataSetName();
        // Set the RegistryObject Id keys for this Object
        // using the values from the DataSetMetaData Object.
        dsn.setProviderName(dataSetToStore.getProviderName());
        dsn.setDataSetType(dataSetToStore.getDataSetType());
        dsn.setDataSetName(dataSetToStore.getDataSetName());

        // Now add the parameter Objects so we can associate
        // the DataSetName with parameters..
        dsn.setParameters(dataSetToStore.getParameters());

        try {
            DataDeliveryHandlers.getDataSetNameHandler().update(dsn);
            statusHandler.info("DataSetName object store complete, dataset ["
                    + dsn.getDataSetName() + "]");
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "DataSetName object store failed:", e);
        }
    }

    /**
     * @param dataSet
     */
    protected void storeDataSet(final DataSet<?, ?> dataSet) {

        DataSet<?, ?> dataSetToStore = getDataSetToStore(dataSet);
        final String dataSetName = dataSetToStore.getDataSetName();
        IDataSetHandler handler = DataDeliveryHandlers.getDataSetHandler();

        try {
            handler.update(dataSetToStore);
            statusHandler.info("Dataset [" + dataSetName
                    + "] successfully stored in Registry");
            storeDataSetName(dataSet);

        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM, "Dataset [" + dataSetName
                    + "] failed to store in Registry");
        }
    }

    /**
     * Make sure our provider is contained in the Registry
     * 
     * @param provider
     */
    protected void storeProvider(final Provider provider) {

        try {
            DataDeliveryHandlers.getProviderHandler().update(provider);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Provider [" + provider.getName()
                            + "] failed to store in Registry");
        }
    }

    /**
     * Checks for a {@link DataSet} already existing with the same name in the
     * Registry. If so, then combine the objects.
     * 
     * @param dataSet
     *            the dataSet
     * @return the dataSet instance that should be stored to the registry
     */

    @SuppressWarnings({ "rawtypes", "unchecked" })
    protected DataSet getDataSetToStore(DataSet dataSet) {
        try {
            DataSet<Time, Coverage> result = DataDeliveryHandlers.getDataSetHandler()
                    .getByNameAndProvider(dataSet.getDataSetName(),
                            dataSet.getProviderName());
            if (result != null) {
                dataSet.combine(result);
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve dataset.", e);
        }
        return dataSet;
    }

    /**
     * Store a parameter object to the registry. If necessary, also store the
     * ParameterLevel Objects needed to successfully store the Parameter Object.
     * 
     * @param parameter
     *            The Parameter Object to store.
     */
    protected void storeParameter(Parameter parameter) {

        try {
            DataDeliveryHandlers.getParameterHandler().update(parameter);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to store parameter [" + parameter.getName() + "]");
        }
    }

    /**
     * Get me my level types for this param
     * 
     * @param cp
     * @return
     */
    public List<DataLevelType> getDataLevelTypes(Parameter cp) {
        return cp.getLevelType();
    }

    protected abstract void setCoverage(L layer);

    protected abstract Coverage getCoverage(String layerName);

    public void setParameters(L layer) {
        synchronized (layer) {
            if (getParameters() == null || getParameters().isEmpty()) {
                parameters = new HashMap<String, Parameter>();
                for (Parameter parm : agent.getLayer(layer.getName())
                        .getParameters()) {
                    // place in map
                    parameters.put(parm.getName(), parm);
                    storeParameter(parm);
                }
            }
        }
    }

    public Map<String, Parameter> getParameters() {
        return parameters;
    }

    protected abstract void setDataSet(L layer);

    protected abstract DataSet<?, ?> getDataSet(String layerName);

    protected abstract void setDataSetMetaData(L layer);

    protected abstract DataSetMetaData<?> getDataSetMetaData(String layerName);

    protected abstract DataType getDataType();

    public abstract Levels getLevels(DataLevelType type, String collectionName);

    public OGCAgent getAgent() {
        return agent;
    }

    public void setAgent(OGCAgent agent) {
        this.agent = agent;
    }
    
    public abstract String isWithinLayer(R record);
    
    public abstract ISpatialObject getSpatial(R record);

}
