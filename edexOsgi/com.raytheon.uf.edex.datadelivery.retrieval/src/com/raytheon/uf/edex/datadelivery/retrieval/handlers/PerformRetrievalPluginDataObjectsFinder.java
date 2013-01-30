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
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class PerformRetrievalPluginDataObjectsFinder implements
        IRetrievalPluginDataObjectsFinder {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PerformRetrievalPluginDataObjectsFinder.class);

    private final Network network;

    /**
     * Constructor.
     * 
     * @param network
     */
    public PerformRetrievalPluginDataObjectsFinder(Network network) {
        this.network = network;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalPluginDataObjects findRetrievalPluginDataObjects()
            throws Exception {
        RetrievalDao dao = new RetrievalDao();
        RetrievalPluginDataObjects retVal = null;

        ITimer timer = TimeUtil.getTimer();
        try {
            timer.start();
            RetrievalRequestRecord request = dao
                    .activateNextRetrievalRequest(network);

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
    private RetrievalPluginDataObjects process(
            RetrievalRequestRecord requestRecord) {
        requestRecord.setState(State.FAILED);
        List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects = new ArrayList<RetrievalAttributePluginDataObjects>();

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
                    .getAttribute();

            for (RetrievalAttribute attXML : attributes) {
                IRetrievalRequestBuilder request = pra
                        .createRequestMessage(attXML);
                if (request != null) {

                    statusHandler
                            .info("Translated provider attribute Request XML: "
                                    + request.getRequest());

                    IRetrievalResponse response = pra.performRequest(request);

                    if (response != null) {
                        Map<String, PluginDataObject[]> pdoHash = pra
                                .processResponse(response);
                        if (pdoHash != null && !pdoHash.isEmpty()) {
                            for (Entry<String, PluginDataObject[]> entry : pdoHash
                                    .entrySet()) {
                                retrievalAttributePluginDataObjects
                                        .add(new RetrievalAttributePluginDataObjects(
                                                attXML, entry.getValue()));
                            }
                            requestRecord.setState(State.COMPLETED);
                        } else {
                            throw new IllegalStateException(
                                    "No PDO's to store: " + serviceType
                                            + " original: " + attXML.toString());
                        }
                    } else {
                        // null response
                        throw new IllegalStateException(
                                "Null response for service: " + serviceType
                                        + " original: " + attXML.toString());
                    }
                }
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
        }
        RetrievalPluginDataObjects retrievalPluginDataObject = new RetrievalPluginDataObjects(
                requestRecord, retrievalAttributePluginDataObjects);
        return retrievalPluginDataObject;
    }

}
