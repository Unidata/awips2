package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.event.retrieval.SubscriptionRetrievalEvent;
import com.raytheon.uf.common.datadelivery.event.retrieval.SubscriptionRetrievalEvent.Status;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval.SubscriptionType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalManagerNotifyEvent;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.event.EventBus;

/**
 * 
 * Handles subscription notification.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2012  1022       djohnson     No longer extends Thread, simplify {@link SubscriptionDelay}.
 * Oct 10, 2012 0726       djohnson     Use the subRetrievalKey for notifying the retrieval manager.
 * Nov 25, 2012  1268      dhladky      Added additional fields to process subscription tracking
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionNotifyTask implements Runnable {
    @VisibleForTesting
    static class SubscriptionDelay implements Delayed {
        final String subName;

        final Long subRetrievalKey;

        final String owner;

        final String plugin;

        final String provider;

        final SubscriptionType subscriptionType;

        final Network network;

        final long delayedUntilMillis;

        final String key;

        SubscriptionDelay(String subName, String owner, String plugin,
                SubscriptionType subscriptionType, Network network,
                String provider,
                Long subRetrievalKey, long delayedUntilMillis) {
            this.subName = subName;
            this.owner = owner;
            this.plugin = plugin;
            this.provider = provider;
            this.subscriptionType = subscriptionType;
            this.subRetrievalKey = subRetrievalKey;
            this.network = network;
            this.delayedUntilMillis = delayedUntilMillis;
            key = subName + "_" + owner;
        }

        @Override
        public int compareTo(Delayed o) {
            int rval = 0;
            if (o instanceof SubscriptionDelay) {
                SubscriptionDelay oSub = (SubscriptionDelay) o;

                rval = Long.valueOf(delayedUntilMillis).compareTo(
                        Long.valueOf(oSub.delayedUntilMillis));

                if (rval == 0) {
                    rval = subName.compareTo(oSub.subName);
                }

                if (rval == 0) {
                    rval = owner.compareTo(oSub.subName);
                }
            }

            return rval;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof SubscriptionDelay) {
                SubscriptionDelay other = (SubscriptionDelay) obj;

                EqualsBuilder eqBuilder = new EqualsBuilder();
                eqBuilder.append(subName, other.subName);
                eqBuilder.append(owner, other.owner);
                eqBuilder.append(plugin, other.plugin);
                eqBuilder.append(subscriptionType, other.subscriptionType);
                eqBuilder.append(network, other.network);
                eqBuilder.append(provider, other.provider);
                eqBuilder.append(subRetrievalKey, other.subRetrievalKey);
                return eqBuilder.isEquals();
            }
            return super.equals(obj);
        }

        @Override
        public long getDelay(TimeUnit unit) {
            long currentTime = System.currentTimeMillis();
            long remainingDelayMillis = getRemainingDelay(currentTime);

            return unit.convert(remainingDelayMillis, TimeUnit.MILLISECONDS);
        }

        public String getKey() {
            return key;
        }

        /**
         * Returns how many milliseconds are remaining until the delayed time
         * has passed.
         * 
         * @param currentTimeMillis
         *            the millis to see how much delay is remaining
         * 
         * @return how many more milliseconds of delay remaining, or 0 if the
         *         delay has expired
         */
        @VisibleForTesting
        long getRemainingDelay(long currentTimeMillis) {
            return Math.max(delayedUntilMillis - currentTimeMillis, 0);
        }

        @Override
        public int hashCode() {
            HashCodeBuilder hcBuilder = new HashCodeBuilder();
            hcBuilder.append(subName);
            hcBuilder.append(owner);
            hcBuilder.append(plugin);
            hcBuilder.append(subscriptionType);
            hcBuilder.append(network);
            hcBuilder.append(provider);
            hcBuilder.append(subRetrievalKey);

            return hcBuilder.toHashCode();
        }

        @Override
        public String toString() {
            return getKey();
        }
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionNotifyTask.class);

    private static final EventBus eventBus = EventBus.getInstance();

    /**
     * Creates a SubscriptionDelay delayed for 11 seconds.
     * 
     * @param record
     *            the record to base from
     * @param startTime
     *            the start time to which 11 seconds should be added
     * @return
     */
    @VisibleForTesting
    static SubscriptionDelay createSubscriptionDelay(
            RetrievalRequestRecord record, long startTime) {
        // 11 seconds from start time
        return new SubscriptionDelay(record.getId().getSubscriptionName(),
                record.getOwner(), record.getPlugin(),
 record
.getSubscriptionType(), record.getNetwork(),
                record.getProvider(),
                record.getSubRetrievalKey(), startTime + 11000);
    }

    // set written to by other threads
    private ConcurrentMap<String, SubscriptionDelay> waitingSubscriptions = new ConcurrentHashMap<String, SubscriptionDelay>();

    // set used for draining all entries, while other queue being written to
    private ConcurrentMap<String, SubscriptionDelay> subscriptionsInProcess = new ConcurrentHashMap<String, SubscriptionDelay>(
            64);

    private final DelayQueue<SubscriptionDelay> subscriptionQueue = new DelayQueue<SubscriptionDelay>();

    public void checkNotify(RetrievalRequestRecord record) {
        SubscriptionDelay subDelay = createSubscriptionDelay(record,
                System.currentTimeMillis());
        waitingSubscriptions.put(subDelay.getKey(), subDelay);
    }

    @Override
    public void run() {
        RetrievalDao dao = null;

        statusHandler.info("SubscriptionNotifyTask() - Running...");
        try {
            SubscriptionDelay nextSub = subscriptionQueue.peek();

            if (nextSub != null) {
                try {
                    // wait an extra second for a few more to accumulate
                    long timeToWait = nextSub.getDelay(TimeUnit.MILLISECONDS) + 1000;
                    Thread.sleep(timeToWait);
                } catch (InterruptedException e) {
                    // ignore
                }
            }

            // switch set references
            if (!waitingSubscriptions.isEmpty()) {
                ConcurrentMap<String, SubscriptionDelay> tmp = subscriptionsInProcess;
                subscriptionsInProcess = waitingSubscriptions;
                waitingSubscriptions = tmp;
            }

            // don't check if complete until haven't received a new
            // retrieval for 10 seconds
            if (!subscriptionsInProcess.isEmpty()) {
                subscriptionQueue.addAll(subscriptionsInProcess.values());
                subscriptionsInProcess.clear();
            }

            SubscriptionDelay subToCheck = subscriptionQueue.poll();
            while (subToCheck != null) {
                if (dao == null) {
                    dao = new RetrievalDao();
                }

                Map<RetrievalRequestRecord.State, Integer> stateCounts = dao
                        .getSubscriptionStateCounts(subToCheck.subName);
                Integer numPending = stateCounts
                        .get(RetrievalRequestRecord.State.PENDING);
                Integer numRunning = stateCounts
                        .get(RetrievalRequestRecord.State.RUNNING);
                if ((numPending == null || numPending.intValue() == 0)
                        && (numRunning == null || numRunning.intValue() == 0)) {
                    SubscriptionRetrievalEvent event = new SubscriptionRetrievalEvent();
                    event.setId(subToCheck.subName);
                    event.setOwner(subToCheck.owner);
                    event.setPlugin(subToCheck.plugin);
                    event.setProvider(subToCheck.provider);
                    event.setSubscriptionType(subToCheck.subscriptionType
                            .name());
                    event.setNetwork(subToCheck.network.name());

                    RetrievalManagerNotifyEvent retrievalManagerNotifyEvent = new RetrievalManagerNotifyEvent();
                    retrievalManagerNotifyEvent.setId(Long
                            .toString(subToCheck.subRetrievalKey));

                    // check if any of the retrievals failed
                    Integer numFailed = stateCounts
                            .get(RetrievalRequestRecord.State.FAILED);
                    Integer numComplete = stateCounts
                            .get(RetrievalRequestRecord.State.COMPLETED);
                    if (numFailed == null || numFailed.intValue() == 0) {
                        event.setStatus(Status.SUCCESS);
                    } else {
                        if (numComplete != null && numComplete.intValue() > 0) {
                            event.setStatus(Status.PARTIAL_SUCCESS);
                        } else {
                            event.setStatus(Status.FAILED);
                        }

                        // generate message
                        List<RetrievalRequestRecord> failedRecs = dao
                                .getFailedRequests(subToCheck.subName);
                        StringBuffer sb = new StringBuffer(300);
                        try {
                            sb.append("Failed parameters: ");
                            for (RetrievalRequestRecord failedRec : failedRecs) {
                                Retrieval retrieval = SerializationUtil
                                        .transformFromThrift(Retrieval.class,
                                                failedRec.getRetrieval());
                                for (RetrievalAttribute att : retrieval
                                        .getAttribute()) {
                                    sb.append(att.getParameter().getName()
                                            + ", ");
                                }
                            }
                            sb.delete(sb.length() - 2, sb.length());
                            event.setFailureMessage(sb.toString());
                        } catch (SerializationException e) {
                            sb.append("Failed parameters: Unable to determine the parameters that failed due to serialization errors.");
                            statusHandler
                                    .error("Error occurred unmarshalling retrieval object for determining failed parameters.",
                                            e);
                        }
                    }
                    if (numComplete == null) {
                        event.setNumComplete(0);
                    } else {
                        event.setNumComplete(numComplete);
                    }
                    if (numFailed == null) {
                        event.setNumFailed(0);
                    } else {
                        event.setNumFailed(numFailed);
                    }
                    eventBus.publish(event);
                    eventBus.publish(retrievalManagerNotifyEvent);
                    dao.removeSubscription(subToCheck.subName);
                }

                subToCheck = subscriptionQueue.poll();
            }
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to contact the database", e);
        }
    }
}
