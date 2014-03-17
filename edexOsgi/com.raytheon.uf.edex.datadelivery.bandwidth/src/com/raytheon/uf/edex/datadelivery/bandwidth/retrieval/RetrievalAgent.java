package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
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
 * Mar 05, 2013 1647       djohnson     Sleep one minute between checks.
 * Jan 30, 2014   2686     dhladky      refactor of retrieval.
 * Feb 10, 2014  2678      dhladky      Prevent duplicate allocations.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 * @param <ALLOCATION_TYPE>
 */
public abstract class RetrievalAgent<ALLOCATION_TYPE extends BandwidthAllocation>
        extends Thread {

    private static final long SLEEP_TIME = TimeUtil.MILLIS_PER_MINUTE;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalAgent.class);
    
    public static final String SUBSCRIPTION_AGENT = "SubscriptionAgent";

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
            Thread.sleep(SLEEP_TIME);
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
                    Thread.sleep(SLEEP_TIME);
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

        statusHandler.info(network
                + ": Checking for bandwidth allocations to process...");
        List<BandwidthAllocation> allocationReservations = retrievalManager
                .getRecentAllocations(network, getAgentType());

        if (allocationReservations != null) {

            List<ALLOCATION_TYPE> allocations = new ArrayList<ALLOCATION_TYPE>(
                    allocationReservations.size());

            for (BandwidthAllocation bandwidthAlloc : allocationReservations) {
                if (bandwidthAlloc == RetrievalManager.POISON_PILL) {
                    statusHandler
                            .info("Received kill request, this thread is shutting down...");
                    dead = true;
                    return;
                }
                // cast to type class
                ALLOCATION_TYPE allocation = (ALLOCATION_TYPE) getAllocationTypeClass()
                        .cast(bandwidthAlloc);
                allocations.add(allocation);
                statusHandler.info(network + ": Processing allocation["
                        + allocation.getId() + "]");
            }
            
            processAllocations(allocations);

        } else {
            synchronized (notifier) {
                try {
                    statusHandler.info(network + ": None found, sleeping for ["
                            + SLEEP_TIME + "]");

                    notifier.wait(SLEEP_TIME);
                } catch (InterruptedException e) {
                    statusHandler.handle(Priority.WARN,
                            "Interrupted while waiting for notification.", e);
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
     * @param allocations
     *            the allocations
     * @throws EdexException
     *             on error processing the allocation
     */
    abstract void processAllocations(List<ALLOCATION_TYPE> allocations)
            throws EdexException;
    /**
     * Get the network
     * @return
     */
    public Network getNetwork() {
        return network;
    }
}