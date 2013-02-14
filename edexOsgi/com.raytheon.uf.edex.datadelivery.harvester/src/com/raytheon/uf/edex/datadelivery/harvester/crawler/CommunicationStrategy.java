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
package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.edex.datadelivery.harvester.config.CrawlAgent;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.config.ProtoCollection;
import com.raytheon.uf.edex.datadelivery.retrieval.ProviderCollectionLinkStore;

/**
 * Defines a communication strategy between the Crawler and the Harvester.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2012 1022       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface CommunicationStrategy {

    /**
     * Retrieve errors encountered by the Crawler.
     * 
     * @return the errors
     */
    List<Throwable> getErrors();

    /**
     * Retrieve next available link store.
     * 
     * @return the link store
     */
    ProviderCollectionLinkStore getNextLinkStore();

    void processCollections(HarvesterConfig hconfig,
            Map<String, ProtoCollection> collections, Provider provider,
            CrawlAgent agent);

    /**
     * Communicate any exceptions that occured.
     * 
     * @param e
     *            the exception
     */
    void sendException(Exception e);

    /**
     * Send the link store.
     * 
     * @param providerCollectionLinkStore
     */
    void sendLinkStore(ProviderCollectionLinkStore providerCollectionLinkStore);

    /**
     * Signals the composing class wishes the strategy to shutdown.
     */
    void shutdown();
}
