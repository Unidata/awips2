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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.google.common.collect.Maps;
import com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.util.DataSizeUtils;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.util.RetrievalPersistUtil;

/**
 * Implementation of {@link IRetrievedDataProcessor} that stores the plugin data
 * objects to the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013 1543       djohnson     Initial creation
 * Feb 12, 2013 1543       djohnson     Now handles the retrieval responses directly.
 * Feb 15, 2013 1543       djohnson     Retrieve the retrieval attributes from the database.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class StoreRetrievedData implements IRetrievalPluginDataObjectsProcessor {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StoreRetrievedData.class);

    private final String generalDestinationUri;

    private final IRetrievalDao retrievalDao;

    /**
     * Constructor.
     * 
     * @param generalDestinationUri
     *            the destination uri most plugin data will travel through
     */
    public StoreRetrievedData(String generalDestinationUri,
            IRetrievalDao retrievalDao) {
        this.generalDestinationUri = generalDestinationUri;
        this.retrievalDao = retrievalDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processRetrievedPluginDataObjects(
            RetrievalResponseXml retrievalPluginDataObjects)
            throws Exception {
        Map<String, PluginDataObject[]> pluginDataObjects = Maps.newHashMap();
        final RetrievalRequestRecord requestRecord = retrievalDao
                .getById(retrievalPluginDataObjects.getRequestRecord());

        final List<RetrievalResponseWrapper> retrievalAttributePluginDataObjects = retrievalPluginDataObjects
                .getRetrievalAttributePluginDataObjects();
        final Retrieval retrieval = requestRecord.getRetrievalObj();
        final Iterator<RetrievalAttribute> attributesIter = retrieval
                .getAttributes().iterator();
        final ServiceType serviceType = retrieval.getServiceType();
        final RetrievalAdapter serviceRetrievalAdapter = ServiceTypeFactory
                .retrieveServiceRetrievalAdapter(serviceType);

        for (RetrievalResponseWrapper pluginDataObjectEntry : retrievalAttributePluginDataObjects) {
            if (!attributesIter.hasNext()) {
                statusHandler
                        .warn("Did not find a RetrievalAttribute to match the retrieval response!  Skipping response...");
            }

            // Restore the attribute xml prior to processing the response
            final IRetrievalResponse retrievalResponse = pluginDataObjectEntry
                    .getRetrievalResponse();
            retrievalResponse.setAttribute(attributesIter.next());

            Map<String, PluginDataObject[]> value = serviceRetrievalAdapter
                    .processResponse(retrievalResponse);

            if (value == null || value.isEmpty()) {
                continue;
            }

            for (Entry<String, PluginDataObject[]> entry : value.entrySet()) {
                final String key = entry.getKey();
                final PluginDataObject[] objectsForEntry = entry.getValue();

                PluginDataObject[] objectsForPlugin = pluginDataObjects
                        .get(key);
                objectsForPlugin = CollectionUtil.combine(
                        PluginDataObject.class, objectsForPlugin,
                        objectsForEntry);

                pluginDataObjects.put(key, objectsForPlugin);
            }

            final RetrievalAttribute attXML = retrievalResponse.getAttribute();
            for (Entry<String, PluginDataObject[]> entry : pluginDataObjects
                    .entrySet()) {
                final String pluginName = entry.getKey();
                final PluginDataObject[] records = entry.getValue();

                if (records == null) {
                    statusHandler
                            .warn("The plugin data objects was a null array, the service retrieval adapter "
                                    + "should not return a null map of plugin data objects!");
                    continue;
                }

                statusHandler.info("Successfully processed: " + records.length
                        + " : " + serviceType + " Plugin : " + pluginName);
                DataRetrievalEvent event = new DataRetrievalEvent();
                event.setId(retrieval.getSubscriptionName());
                event.setOwner(retrieval.getOwner());
                event.setNetwork(retrieval.getNetwork().name());
                event.setPlugin(pluginName);
                event.setProvider(attXML.getProvider());
                event.setNumRecords(records.length);
                event.setBytes(DataSizeUtils.calculateSize(attXML, serviceType));

                EventBus.publish(event);

                sendToDestinationForStorage(requestRecord, records);
            }
        }
    }

    /**
     * Sends the plugin data objects to their configured destination for storage
     * to the database.
     */
    public void sendToDestinationForStorage(
            RetrievalRequestRecord requestRecord, PluginDataObject[] pdos) {
        String pluginName = pdos[0].getPluginName();

        if (pluginName != null) {
            RetrievalPersistUtil.routePlugin(generalDestinationUri, pluginName,
                    pdos);
        }

    }
}
