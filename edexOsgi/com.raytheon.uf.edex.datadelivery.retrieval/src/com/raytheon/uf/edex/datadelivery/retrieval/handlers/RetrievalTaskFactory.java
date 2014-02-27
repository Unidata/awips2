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
/**
 * Factory to create RetrievalTasks for a specific Subscription Retrieval Request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 04, 2014  2686       dhladky      Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class RetrievalTaskFactory {

    private final Network network;

    private final IRetrievalPluginDataObjectsProcessor retrievedDataProcessor;

    private final IRetrievalResponseCompleter retrievalCompleter;

    private final IRetrievalsFinder retrievalDataFinder;

    public RetrievalTaskFactory(Network network,
            IRetrievalsFinder retrievalDataFinder,
            IRetrievalPluginDataObjectsProcessor retrievedDataProcessor,
            IRetrievalResponseCompleter retrievalCompleter) {
        this.network = network;
        this.retrievalDataFinder = retrievalDataFinder;
        this.retrievedDataProcessor = retrievedDataProcessor;
        this.retrievalCompleter = retrievalCompleter;
    }
    
    /**
     * RetrievalTask creator, Factory method
     * @param retrievalRequest
     * @return
     */
    public RetrievalTask create(SubscriptionRetrievalRequestWrapper retrievalRequest) {
        return new RetrievalTask(network, retrievalDataFinder, retrievedDataProcessor, retrievalCompleter, retrievalRequest);
    }
}