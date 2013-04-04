package com.raytheon.uf.edex.datadelivery.retrieval;

import java.util.Iterator;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataSetName;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetQuery;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Parse MetaData.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011 218        dhladky      Initial creation
 * May 15, 2012 455        jspinks      Modified for storing object associations in registry.
 * Jun 21, 2012 736        djohnson     Change OPERATION_STATUS to OperationStatus.
 * Aug 02, 2012 955        djohnson     Type-safe registry query/responses.
 * Aug 10, 2012 1022       djohnson     {@link DataSetQuery} requires provider name.
 * Aug 20, 2012 0743       djohnson     Finish making registry type-safe.
 * Sep 14, 2012 1169       djohnson     Use storeOrReplaceRegistryObject.
 * Oct 03, 2012 1241       djohnson     Use registry handler, move unresolved reference handling into handlers themselves.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class MetaDataParser implements IParseMetaData {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetaDataParser.class);

    protected ServiceConfig serviceConfig;

    public MetaDataParser() {
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
     * Store the DataSetMetaData Object to the registry.
     * 
     * @param metaDatas
     *            The DataSetMetaData Object to store.
     */
    @Override
    public void storeMetaData(final List<DataSetMetaData> metaDatas,
            final DataSet dataSet) {

        IDataSetMetaDataHandler handler = DataDeliveryHandlers
                .getDataSetMetaDataHandler();
        Iterator<DataSetMetaData> iter = metaDatas.iterator();
        int size = metaDatas.size();
        for (int i = 1; i <= size; i++) {
            statusHandler.info(String.format(
                    "Attempting store of DataSetMetaData[%s/%s]", i, size));
            final DataSetMetaData dsmd = iter.next();
            final String url = dsmd.getUrl();

            try {
                handler.update(dsmd);
                statusHandler.info("DataSetMetaData [" + url
                        + "] successfully stored in Registry");
            } catch (RegistryHandlerException e) {
                statusHandler.info("DataSetMetaData [" + url
                        + "] failed to store in Registry");

            }
        }
    }

}
