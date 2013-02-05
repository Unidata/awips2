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

import java.util.List;

import com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.util.DataSizeUtils;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;

/**
 * Performs processing on the retrieved plugin data objects, and then sends an
 * event about the retrieval.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 05, 2013 1580       mpduff       EventBus refactor.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class NotifyOfPluginDataObjectsDecorator implements
        IRetrievalPluginDataObjectsProcessor {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotifyOfPluginDataObjectsDecorator.class);

    private final IRetrievalPluginDataObjectsProcessor retrievedDataProcessor;

    public NotifyOfPluginDataObjectsDecorator(
            IRetrievalPluginDataObjectsProcessor retrievedDataProcessor) {
        this.retrievedDataProcessor = retrievedDataProcessor;
    }

    @Override
    public void processRetrievedPluginDataObjects(
            RetrievalPluginDataObjects retrievalPluginDataObjects)
            throws Exception {

        // TODO: What if one of the records fails to store or serialize, is that
        // already handled somewhere?
        retrievedDataProcessor
                .processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        final RetrievalRequestRecord requestRecord = retrievalPluginDataObjects
                .getRequestRecord();
        final List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects = retrievalPluginDataObjects
                .getRetrievalAttributePluginDataObjects();

        for (RetrievalAttributePluginDataObjects pluginDataObjectEntry : retrievalAttributePluginDataObjects) {
            RetrievalAttribute attXML = pluginDataObjectEntry.getAttributeXml();
            PluginDataObject[] value = pluginDataObjectEntry
                    .getPluginDataObjects();

            if (value.length == 0) {
                continue;
            }

            final String pluginName = value[0].getPluginName();
            Retrieval retrieval = requestRecord.getRetrievalObj();
            ServiceType serviceType = retrieval.getServiceType();

            statusHandler.info("Successfully processed: " + value.length
                    + " : " + serviceType + " Plugin : " + pluginName);
            DataRetrievalEvent event = new DataRetrievalEvent();
            event.setId(retrieval.getSubscriptionName());
            event.setOwner(retrieval.getOwner());
            event.setNetwork(retrieval.getNetwork().name());
            event.setPlugin(pluginName);
            event.setProvider(attXML.getProvider());
            event.setNumRecords(value.length);
            event.setBytes(DataSizeUtils.calculateSize(attXML, serviceType));

            EventBus.publish(event);
        }
    }
}
