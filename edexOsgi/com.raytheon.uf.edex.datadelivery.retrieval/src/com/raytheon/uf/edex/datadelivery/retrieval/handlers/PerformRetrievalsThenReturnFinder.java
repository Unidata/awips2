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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.event.status.DataDeliverySystemStatusDefinition;
import com.raytheon.uf.common.datadelivery.event.status.SystemStatusEvent;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;

/**
 * Performs the actual retrieval, and then returns the plugin data objects for
 * processing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 07, 2013 1543       djohnson     Expose process() for testing.
 * Feb 12, 2013 1543       djohnson     Retrieval responses are now passed further down the chain.
 * Feb 15, 2013 1543       djohnson     Retrieval responses are now xml.
 * Jul 16, 2013 1655       mpduff       Send a system status event based on the response from the provider.
 * Jan 15, 2014 2678       bgonzale     Retrieve RetrievalRequestRecords from a Queue for processing.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class PerformRetrievalsThenReturnFinder implements IRetrievalsFinder {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PerformRetrievalsThenReturnFinder.class);

    private final IRetrievalDao retrievalDao;

    private final ConcurrentLinkedQueue<RetrievalRequestRecordPK> retrievalQueue;

    /**
     * Constructor.
     * 
     * @param network
     */
    public PerformRetrievalsThenReturnFinder(
            ConcurrentLinkedQueue<RetrievalRequestRecordPK> retrievalQueue,
            IRetrievalDao retrievalDao) {
        this.retrievalQueue = retrievalQueue;
        this.retrievalDao = retrievalDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalResponseXml findRetrievals() throws Exception {
        RetrievalResponseXml retVal = null;

        ITimer timer = TimeUtil.getTimer();
        try {
            timer.start();
            RetrievalRequestRecordPK id = retrievalQueue.poll();
            if (id == null) {
                return null;
            }

            RetrievalRequestRecord request = retrievalDao.getById(id);
            if (request == null) {
                return null;
            }

            timer.stop();
            statusHandler.info("Activation of next retrieval took ["
                    + timer.getElapsedTime() + "] ms");

            timer.reset();
            timer.start();

            try {
                retVal = process(request);

                timer.stop();

                statusHandler.info("Retrieval Processing for ["
                        + request.getId() + "] took " + timer.getElapsedTime()
                        + " ms");

            } catch (Exception e) {
                statusHandler.error(
                        "Retrieval Processing failed: [" + request.getId()
                                + "]", e);
            }
        } catch (Exception e) {
            statusHandler
                    .error("Unable to look up next retrieval request at this time.",
                            e);
        }

        return retVal;
    }

    /**
     * The actual work gets done here.
     */
    @VisibleForTesting
    RetrievalResponseXml process(RetrievalRequestRecord requestRecord) {
        requestRecord.setState(State.FAILED);
        List<RetrievalResponseWrapper> retrievalAttributePluginDataObjects = new ArrayList<RetrievalResponseWrapper>();

        try {
            Retrieval retrieval = requestRecord.getRetrievalObj();

            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Starting Retrieval: Subscription: "
                        + retrieval.getSubscriptionName());
            }
            ServiceType serviceType = retrieval.getServiceType();

            RetrievalAdapter pra = ServiceTypeFactory
                    .retrieveServiceRetrievalAdapter(serviceType);
            pra.setProviderRetrievalXML(retrieval);

            // Perform the actual retrievals and transforms to plugin data
            // objects
            final List<RetrievalAttribute> attributes = retrieval
                    .getAttributes();

            for (RetrievalAttribute attXML : attributes) {
                IRetrievalRequestBuilder request = pra
                        .createRequestMessage(attXML);
                if (request != null) {

                    statusHandler
                            .info("Translated provider attribute Request XML: "
                                    + request.getRequest());

                    IRetrievalResponse response = pra.performRequest(request);

                    if (response != null) {
                        setCompletionStateFromResponse(requestRecord, response);

                        retrievalAttributePluginDataObjects
                                .add(new RetrievalResponseWrapper(response));
                    } else {
                        throw new IllegalStateException("No PDO's to store: "
                                + serviceType + " original: "
                                + attXML.toString());
                    }
                } else {
                    // null response
                    throw new IllegalStateException(
                            "Null response for service: " + serviceType
                                    + " original: " + attXML.toString());
                }
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
        }
        RetrievalResponseXml retrievalPluginDataObject = new RetrievalResponseXml(
                requestRecord.getId(), retrievalAttributePluginDataObjects);
        retrievalPluginDataObject
                .setSuccess(requestRecord.getState() == State.COMPLETED);

        // Create system status event
        SystemStatusEvent event = new SystemStatusEvent();
        event.setName(requestRecord.getProvider());
        event.setSystemType("Provider");
        if (requestRecord.getState() == State.COMPLETED
                || requestRecord.getState() == State.PENDING
                || requestRecord.getState() == State.RUNNING) {
            event.setStatus(DataDeliverySystemStatusDefinition.UP);
        } else if (requestRecord.getState() == State.FAILED) {
            event.setStatus(DataDeliverySystemStatusDefinition.DOWN);
        } else {
            event.setStatus(DataDeliverySystemStatusDefinition.UNKNOWN);
        }

        EventBus.publish(event);

        return retrievalPluginDataObject;
    }

    /**
     * Sets the {@link RetrievalRequestRecord} status based on the
     * {@link IRetrievalResponse}.
     * 
     * @param requestRecord
     *            the request record
     * @param response
     *            the response
     */
    @VisibleForTesting
    static void setCompletionStateFromResponse(
            RetrievalRequestRecord requestRecord, IRetrievalResponse response) {
        final State completionState = response.getPayLoad() == null ? State.FAILED
                : State.COMPLETED;
        requestRecord.setState(completionState);
    }

}
