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

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;

/**
 * Process subscription retrievals.
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
 * Jan 30, 2013 1543       djohnson     Constrain to the network retrievals are pulled for.
 * Feb 15, 2013 1543       djohnson     Using xml for retrievals now.
 * Mar 05, 2013 1647       djohnson     Change no retrievals found message to debug.
 * Aug 09, 2013 1822       bgonzale     Added parameters to processRetrievedPluginDataObjects.
 * Oct 01, 2013 2267       bgonzale     Removed request parameter and IRetrievalDao field.
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class RetrievalTask implements Runnable {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalTask.class);

    private final Network network;

    private final IRetrievalPluginDataObjectsProcessor retrievedDataProcessor;

    private final IRetrievalResponseCompleter retrievalCompleter;

    private final IRetrievalsFinder retrievalDataFinder;
    
    private final SubscriptionRetrievalRequestWrapper retrievalRequestWrapper;

    public RetrievalTask(Network network,
            IRetrievalsFinder retrievalDataFinder,
            IRetrievalPluginDataObjectsProcessor retrievedDataProcessor,
            IRetrievalResponseCompleter retrievalCompleter,
            SubscriptionRetrievalRequestWrapper retrievalRequestWrapper) {
        
        this.network = network;
        this.retrievalDataFinder = retrievalDataFinder;
        this.retrievedDataProcessor = retrievedDataProcessor;
        this.retrievalCompleter = retrievalCompleter;
        this.retrievalRequestWrapper = retrievalRequestWrapper;
    }

    @Override
    public void run() {
        
        try {
            
            if (retrievalRequestWrapper.getRetrievalRequestWrappers() != null) {
                
                for (RetrievalRequestWrapper retrieval : retrievalRequestWrapper.getRetrievalRequestWrappers()) {
                    // process individual requests for this subscription
                    boolean success = false;
                    RetrievalRequestRecord request = null;

                    try {
                        // send this retrieval to be processed
                        RetrievalResponseXml retrievalResponse = retrievalDataFinder
                                .processRequest(retrieval);

                        if (retrievalResponse == null) {
                            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                                statusHandler.debug("No " + network
                                        + " retrievals found.");
                            }
                            continue;
                        }

                        success = retrievalResponse.isSuccess();
                        request = retrievedDataProcessor
                                .processRetrievedPluginDataObjects(retrievalResponse);

                    } catch (Exception e) {
                        statusHandler.error(network
                                + " retrieval processing error", e);
                    }

                    if (request != null) {
                        retrievalCompleter.completeRetrieval(request,
                                new RetrievalResponseStatus(success));
                    }
                }
            }
        } catch (Throwable e) {
            // so thread can't die
            statusHandler.error("Error caught in " + network
                    + " retrieval thread", e);
        } 
    }
}
