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

import java.util.HashMap;
import java.util.Map.Entry;

import com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.util.DataSizeUtils;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter.TranslationException;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalHandler;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.util.RetrievalPersistUtil;
import com.raytheon.uf.edex.event.EventBus;

/**
 * Inner class to process individual retrievals.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011            dhladky      Initial creation
 * Aug 15, 2012 1022       djohnson     Moved from inner to class proper.
 * Aug 22, 2012 0743       djohnson     Continue processing retrievals until there are no more.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class RetrievalTask implements IRetrievalHandler, Runnable {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalTask.class);

    private static final EventBus EVENT_BUS = EventBus.getInstance();

    private final String generalDestinationUri;

    private final SubscriptionNotifyTask notifyTask;

    public RetrievalTask(final String generalDestinationUri,
            SubscriptionNotifyTask notifyTask) {
        this.generalDestinationUri = generalDestinationUri;
        this.notifyTask = notifyTask;
    }

    /**
     * The actual work gets done here. TODO: Should return a status object.
     */
    @Override
    public boolean process(Retrieval retrieval) {
        boolean success = true;
        statusHandler.debug("Starting Retrieval: Subscription: "
                + retrieval.getSubscriptionName());
        ServiceType serviceType = retrieval.getServiceType();

        try {
            RetrievalAdapter pra = ServiceTypeFactory
                    .retrieveServiceRetrievalAdapter(serviceType);
            pra.setProviderRetrievalXML(retrieval);

            // could have multiple retrievals
            for (RetrievalAttribute attXML : retrieval.getAttribute()) {
                IRetrievalRequestBuilder request = pra
                        .createRequestMessage(attXML);
                statusHandler
                        .info("Translated provider attribute Request XML: "
                                + request.getRequest());
                IRetrievalResponse response = null;

                if (request != null) {

                    response = pra.performRequest(request);
                    HashMap<String, PluginDataObject[]> pdoHash = null;

                    if (response != null) {

                        pdoHash = pra.processResponse(response);

                        if (pdoHash != null && pdoHash.size() > 0) {
                            // store all types
                            for (Entry<String, PluginDataObject[]> entry : pdoHash
                                    .entrySet()) {
                                PluginDataObject[] value = entry.getValue();
                                if (store(attXML, value)) {
                                    statusHandler.info("Successfully stored: "
                                            + value.length + " : "
                                            + serviceType + " Plugin : "
                                            + entry.getKey());
                                    DataRetrievalEvent event = new DataRetrievalEvent();
                                    event.setId(retrieval.getSubscriptionName());
                                    event.setOwner(retrieval.getOwner());
                                    event.setNetwork(retrieval.getNetwork()
                                            .name());
                                    event.setPlugin(entry.getKey());
                                    event.setProvider(attXML.getProvider());
                                    event.setNumRecords(value.length);
                                    event.setBytes(DataSizeUtils.calculateSize(
                                            attXML, serviceType));

                                    EVENT_BUS.publish(event);
                                } else {
                                    throw new IllegalStateException(
                                            "Unable to store " + value.length
                                                    + " PDOs to the database!");
                                }
                            }
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
        } catch (IllegalStateException e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            success = false;
        } catch (TranslationException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            success = false;
        }
        return success;
    }

    @Override
    public void run() {
        RetrievalDao dao = new RetrievalDao();
        try {
            while (true) {
                RetrievalRequestRecord request = null;

                ITimer timer = TimeUtil.getTimer();
                try {
                    timer.start();
                    request = dao.activateNextRetrievalRequest();
                } catch (Exception e) {
                    statusHandler
                            .error("Unable to look up next retrieval request at this time.",
                                    e);
                }

                // This forces the return from the while loop once there are
                // no more retrievals to process
                if (request == null) {
                    statusHandler.info("No Retrievals found.");
                    return;
                }

                timer.stop();
                statusHandler.info("Activation of next retrieval took ["
                        + timer.getElapsedTime() + "] ms");
                // process request
                boolean success = false;
                Retrieval retrieval = null;

                try {
                    retrieval = SerializationUtil.transformFromThrift(
                            Retrieval.class, request.getRetrieval());

                    timer.reset();
                    timer.start();
                    success = process(retrieval);
                    timer.stop();

                    statusHandler.info("Retrieval Processing for ["
                            + request.getId() + "] took "
                            + timer.getElapsedTime() + " ms");
                } catch (Exception e) {
                    statusHandler.error("Retrieval Processing failed: ["
                            + request.getId() + "]", e);
                }

                RetrievalRequestRecord.State state = success ? RetrievalRequestRecord.State.COMPLETED
                        : RetrievalRequestRecord.State.FAILED;
                request.setState(state);

                // update database
                try {
                    dao.completeRetrievalRequest(request);
                    notifyTask.checkNotify(request);
                } catch (DataAccessLayerException e) {
                    statusHandler.error(
                            "Unable to communicate with the database", e);
                }
            }
        } catch (Throwable e) {
            // so thread can't die
            statusHandler.error("Error caught in retrieval thread", e);
        }
    }

    /**
     * Store PDO's from Provider to EDEX
     */
    @Override
    public boolean store(RetrievalAttribute attXML, PluginDataObject[] pdos) {
        // do all of the PDO storage magic...
        boolean success = false;
        String pluginName = pdos[0].getPluginName();

        if (pluginName != null) {
            success = RetrievalPersistUtil.routePlugin(generalDestinationUri,
                    pluginName, pdos);
        }

        return success;
    }
}
