package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;

/**
 * 
 * A retrieval agent.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2012 0726       djohnson     Add SW history, use generics, 
 *                                      separate work method from loop control.
 * Nov 09, 2012 1286       djohnson     Add ability to kill the threads when BandwidthManager instance is replaced.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 * @param <ALLOCATION_TYPE>
 */
public abstract class RetrievalAgent<ALLOCATION_TYPE extends BandwidthAllocation>
        extends Thread {

    private static final int SLEEP_TIME = 300000;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalAgent.class);

    private final Object notifier;

    protected final Network network;

    protected final String destinationUri;

    protected final RetrievalManager retrievalManager;
    
    private boolean dead;

    /**
     * Constructor.
     * 
     * @param network
     *            the network this retrieval agent utilizes
     * @param destinationUri
     *            the destination uri to send objects
     * @param notifier
     *            the object used to signal the agent that data is available
     * @param retrievalManager
     *            the retrieval manager
     */
    public RetrievalAgent(Network network, String destinationUri,
            final Object notifier, RetrievalManager retrievalManager) {
        this.network = network;
        this.destinationUri = destinationUri;
        this.notifier = notifier;
        this.retrievalManager = retrievalManager;
    }

    @Override
    public void run() {
        try {
            // don't start immediately
            Thread.sleep(60000);
        } catch (InterruptedException e) {
            // ignore
        }

        while (!dead) {
            try {
                doRun();
            } catch (Throwable e) {
                // so thread can't die
                statusHandler
                        .error("Unable to look up next retrieval request.  Sleeping for 1 minute before trying again.",
                                e);
                try {
                    Thread.sleep(60000);
                } catch (InterruptedException e1) {
                    // ignore
                }
            }
        }
    }

    /**
     * Do the actual work.
     * 
     * @throws EdexException
     */
    public void doRun() throws EdexException {
        statusHandler.info("Checking for bandwidth allocations to process...");
        BandwidthAllocation reservation = retrievalManager.nextAllocation(
                network, getAgentType());

        if (reservation == RetrievalManager.POISON_PILL) {
            statusHandler
                    .info("Received kill request, this thread is shutting down...");
            dead = true;
            return;
        }

        if (reservation != null) {
            ALLOCATION_TYPE allocation = getAllocationTypeClass().cast(
                    reservation);
            statusHandler.info("Processing allocation id ["
                    + allocation.getId() + "]");

            processAllocation(allocation);
        } else {
            synchronized (notifier) {
                try {
                    statusHandler.info("None found, sleeping for ["
                            + SLEEP_TIME + "]");

                    notifier.wait(SLEEP_TIME);
                } catch (InterruptedException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Return the concrete class type that the agent handles.
     * 
     * @return the class reference
     */
    abstract Class<ALLOCATION_TYPE> getAllocationTypeClass();

    /**
     * Return the agent type this retrieval agent processes.
     * 
     * @return the agent type
     */
    abstract String getAgentType();

    /**
     * Process the {@link BandwidthAllocation} retrieved.
     * 
     * @param allocation
     *            the allocation
     * @throws EdexException
     *             on error processing the allocation
     */
    abstract void processAllocation(ALLOCATION_TYPE allocation)
            throws EdexException;
}
