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
package com.raytheon.uf.edex.ogc.common.db;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.time.DateUtils;

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
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   754       dhladky      initial creation, based on B Clements original
 * 04/22/2013   1746      dhladky      Removed DB dependency from WFS code
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class LayerCollector<DIMENSION extends SimpleDimension, L extends SimpleLayer<DIMENSION>> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LayerCollector.class);

    protected LayerTransformer<DIMENSION, L> transformer;
    
    protected HarvesterConfig config = null;
    
    protected OGCAgent agent = null;

    protected Map<String, Parameter> parameters = null;

    public LayerCollector(LayerTransformer<DIMENSION, L> transformer) {
        setTransformer(transformer);
        this.config = HarvesterConfigurationManager.getOGCConfiguration();
        setAgent((OGCAgent)config.getAgent());
        storeProvider(config.getProvider());
    }

    public HarvesterConfig getConfig() {
        return config;
    }

    public void setConfig(HarvesterConfig config) {
        this.config = config;
    }

    public void update(L old, L shiny) {
        updateDates(old, shiny);
        updateDims(old, shiny);
    }

    public void updateDates(L old, L shiny) {
        Set<Date> shinyTimes = shiny.getTimes();
        Set<Date> oldTimes = old.getTimes();
        if (shinyTimes != null && shinyTimes != null) {
            for (Date time : shinyTimes) {
                oldTimes.add(time);
            }
        }
    }

    public void updateDims(L old, L shiny) {
        Set<DIMENSION> oldDims = old.getDimensions();
        Set<DIMENSION> shinyDims = shiny.getDimensions();
        if (oldDims != null) {
            if (shinyDims != null) {
                updateDimLists(oldDims, shinyDims);
            }
        }
    }

    public Map<String, DIMENSION> getDimMap(Set<DIMENSION> dims) {
        HashMap<String, DIMENSION> rval = new HashMap<String, DIMENSION>(
                dims.size());
        for (DIMENSION sd : dims) {
            rval.put(sd.getName(), sd);
        }
        return rval;
    }

    public void updateDimLists(Set<DIMENSION> oldDims, Set<DIMENSION> shinyDims) {
        Map<String, DIMENSION> oldMap = getDimMap(oldDims);
        Map<String, DIMENSION> shinyMap = getDimMap(shinyDims);
        for (String name : shinyMap.keySet()) {
            DIMENSION shinyDim = shinyMap.get(name);
            DIMENSION oldDim = oldMap.get(name);
            if (oldDim == null) {
                oldDims.add(shinyDim);
            } else {
                updateDimValues(oldDim, shinyDim);
            }
        }
    }

    public void updateDimValues(DIMENSION oldDim, DIMENSION shinyDim) {
        Set<String> oldValues = oldDim.getValues();
        Set<String> shinyValues = shinyDim.getValues();
        if (oldValues != null && shinyValues != null) {
            for (String val : shinyValues) {
                oldValues.add(val);
            }
        }
    }

    /**
     * Take the Calendar back to the first instant of the current hour. This is
     * equivalent to calling roundToHour with cutoff of 59
     * 
     * @param cal
     * @return
     */
    public static Calendar truncateToHour(Calendar cal) {
        return DateUtils.truncate(cal, Calendar.HOUR);
    }

    /**
     * Take the Date back to the first instant of the current hour. This is
     * equivalent to calling roundToHour with cutoff of 59
     * 
     * @param d
     * @return
     */
    public static Date truncateToHour(Date d) {
        return DateUtils.truncate(d, Calendar.HOUR);
    }

    /**
     * Round the Calendar to the nearest hour determined by cutoff
     * 
     * @param cal
     * @param cutoff
     *            if cal's minute value is greater than cutoff, the return will
     *            be rounded up to the next hour, else rounded down
     * @return
     */
    public static Calendar roundToHour(Calendar cal, int cutoff) {
        if (cutoff < 0 || cutoff > 59) {
            cutoff %= 60;
        }
        if (cal.get(Calendar.MINUTE) > cutoff) {
            cal = (Calendar) cal.clone();
            cal.add(Calendar.HOUR, 1);
        }
        return truncateToHour(cal);
    }

    /**
     * Round the Date to the nearest hour determined by cutoff
     * 
     * @param d
     * @param cutoff
     *            if d's minute value is greater than cutoff, the return will be
     *            rounded up to the next hour, else rounded down
     * @return
     */
    public static Date roundToHour(Date d, int cutoff) {
        Calendar tmp = Calendar.getInstance();
        tmp.setTime(d);
        tmp = roundToHour(tmp, cutoff);
        return tmp.getTime();
    }
    
    /**
     * Find the DPA config
     * @return
     */
    public HarvesterConfig getConfiguration() {
        return config;
    }
   
    /**
     * Stroe Data objects
     * @param metaDatas
     * @param dataSet
     */
    public void storeMetaData(final DataSetMetaData metaData) {

        IDataSetMetaDataHandler handler = DataDeliveryHandlers
                .getDataSetMetaDataHandler();

        final String description = metaData.getDataSetDescription();
        statusHandler.info("Attempting store of DataSetMetaData["+description+"]");

        try {
            handler.update(metaData);
            statusHandler.info("DataSetMetaData [" + description
                    + "] successfully stored in Registry");
        } catch (RegistryHandlerException e) {
            statusHandler.info("DataSetMetaData [" + description
                    + "] failed to store in Registry");


        }
    }

    /**
     * 
     * @param dataSetToStore
     */
    protected void storeDataSetName(DataSet dataSetToStore) {

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
            statusHandler.error("DataSetName object store failed:", e);
        }
    }

    /**
     * @param dataSet
     */
    protected void storeDataSet(final DataSet dataSet) {

        DataSet dataSetToStore = getDataSetToStore(dataSet);
        final String dataSetName = dataSetToStore.getDataSetName();
        IDataSetHandler handler = DataDeliveryHandlers.getDataSetHandler();
        
        try {
            handler.update(dataSetToStore);
            statusHandler.info("Dataset [" + dataSetName
                    + "] successfully stored in Registry");
            storeDataSetName(dataSet);

        } catch (RegistryHandlerException e) {
            statusHandler.info("Dataset [" + dataSetName
                    + "] failed to store in Registry");
        }
    }
    
    /**
     * Make sure our provider is contained in the Registry
     * @param provider
     */
    protected void storeProvider(final Provider provider) {
        
        try {
            DataDeliveryHandlers.getProviderHandler().update(provider);
        } catch (RegistryHandlerException e) {
            statusHandler.info("Provider [" + provider.getName()
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
    protected DataSet getDataSetToStore(DataSet dataSet) {
        try {
            DataSet result = DataDeliveryHandlers.getDataSetHandler()
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
            statusHandler.info("Failed to store parameter ["
                    + parameter.getName() + "]");
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
    
    protected abstract L newLayer();
    
    protected abstract void setCoverage(L layer);
    
    protected abstract Coverage getCoverage();

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
    
    protected abstract DataSet getDataSet();
    
    protected abstract void setDataSetMetaData(L layer);
    
    protected abstract DataSetMetaData getDataSetMetaData();
    
    protected abstract DataType getDataType();
    
    public abstract Levels getLevels(DataLevelType type, String collectionName);
  
    public LayerTransformer<DIMENSION, L> getTransformer() {
        return transformer;
    }

    public void setTransformer(LayerTransformer<DIMENSION, L> transformer) {
        this.transformer = transformer;
    }

    public OGCAgent getAgent() {
        return agent;
    }

    public void setAgent(OGCAgent agent) {
        this.agent = agent;
    }

}
