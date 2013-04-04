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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.harvester.config.CrawlAgent;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.config.ProtoCollection;
import com.raytheon.uf.edex.datadelivery.retrieval.ProviderCollectionLinkStore;

/**
 * Decorates a {@link CommunicationStrategy} for seed scan
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 4 Oct,   2012 1038       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class SeedCommunicationStrategyDecorator implements
        CommunicationStrategy {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SeedCommunicationStrategyDecorator.class);

    private static final long SHUTDOWN_TIMEOUT_IN_MINUTES = 1;

    private final ExecutorService taskExecutor;

    private final CommunicationStrategy decorated;

    public SeedCommunicationStrategyDecorator(CommunicationStrategy decorated) {
        this(decorated, Executors.newSingleThreadExecutor());
    }

    public SeedCommunicationStrategyDecorator(CommunicationStrategy decorated,
            ExecutorService taskExecutor) {
        this.decorated = decorated;
        this.taskExecutor = taskExecutor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Throwable> getErrors() {
        return decorated.getErrors();
    }

    // do nothing
    @Override
    public ProviderCollectionLinkStore getNextLinkStore() {
        return null;
    }

    @Override
    public void processCollections(HarvesterConfig hconfig,
            Map<String, ProtoCollection> collections, Provider provider,
            CrawlAgent agent) {

        decorated.processCollections(hconfig, collections, provider, agent);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendException(final Exception e) {
        taskExecutor.execute(new Runnable() {
            @Override
            public void run() {
                decorated.sendException(e);
            }
        });
    }

    // do nothing
    @Override
    public void sendLinkStore(
            ProviderCollectionLinkStore providerCollectionLinkStore) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdown() {
        try {
            decorated.shutdown();
        } finally {
            taskExecutor.shutdown();
            try {
                taskExecutor.awaitTermination(SHUTDOWN_TIMEOUT_IN_MINUTES,
                        TimeUnit.MINUTES);
            } catch (InterruptedException e) {
                statusHandler
                        .handle(Priority.WARN,
                                "Timeout occurred waiting for tasks to finish processing!",
                                e);
            }
        }
    }
}
