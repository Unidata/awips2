package com.raytheon.uf.edex.datadelivery.retrieval;

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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;

/**
 * Handle Retrieval creation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 09, 2012            dhladky      Initial creation
 * Jul 25, 2012 955        djohnson     Use {@link ServiceTypeFactory}.
 * Oct 10, 2012 0726       djohnson     Pass -1 for subscription retrieval id, since not bandwidth managed.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class RetrievalGenerationHandler implements IGenerateRetrieval {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalGenerationHandler.class);

    public RetrievalGenerationHandler() {

    }

    @Override
    public List<String> generateRetrieval(List<SubscriptionBundle> bundles) {

        if (bundles != null) {
            RetrievalDao dao = new RetrievalDao();
            ArrayList<String> names = new ArrayList<String>(bundles.size());

            for (SubscriptionBundle bundle : bundles) {
                statusHandler.info("Bundle: " + bundle.getBundleId()
                        + " Create Retrieval Messages....");

                // process the bundle into a retrieval
                RetrievalGenerator rg = ServiceTypeFactory
                        .retrieveServiceFactory(bundle.getProvider())
                        .getRetrievalGenerator();

                final String subscriptionName = bundle.getSubscription()
                        .getName();
                statusHandler.info("Subcription: " + subscriptionName
                        + " Being Processed for Retrieval...");

                List<Retrieval> retrievals = rg.buildRetrieval(bundle);

                if (!CollectionUtil.isNullOrEmpty(retrievals)) {
                    // need to track in case something fails early
                    int numFinished = 0;

                    String owner = bundle.getSubscription().getOwner();
                    String provider = bundle.getSubscription().getProvider();
                    int priority = 3;
                    Integer bundlePriority = bundle.getPriority();
                    if (bundlePriority != null) {
                        priority = bundlePriority.intValue();
                    }
                    Date insertTime = Calendar.getInstance().getTime();

                    List<RetrievalRequestRecord> requestRecords = new ArrayList<RetrievalRequestRecord>(
                            retrievals.size());
                    long cumultTime1 = 0;
                    int index = 0;
                    for (Retrieval retrieval : retrievals) {

                        RetrievalRequestRecord rec = new RetrievalRequestRecord(
                                subscriptionName, index++, -1L);
                        rec.setOwner(owner);
                        rec.setPlugin(retrieval.getProviderType().getPlugin());
                        rec.setProvider(provider);
                        rec.setSubscriptionType(retrieval.getSubscriptionType());
                        rec.setNetwork(retrieval.getNetwork());
                        rec.setPriority(priority);
                        rec.setInsertTime(insertTime);

                        try {
                            long t1 = System.currentTimeMillis();
                            rec.setRetrieval(SerializationUtil
                                    .transformToThrift(retrieval));
                            long t2 = System.currentTimeMillis();
                            cumultTime1 += t2 - t1;
                            rec.setState(State.PENDING);
                        } catch (Exception e) {
                            statusHandler.error("Subcription: "
                                    + subscriptionName
                                    + " Failed to serialize request ["
                                    + retrieval + "]", e);
                            numFinished++;
                            rec.setRetrieval(new byte[0]);
                            rec.setState(State.FAILED);
                        }

                        requestRecords.add(rec);
                    }

                    statusHandler.info("Cumulative time to serialize "
                            + requestRecords.size() + " requests: thrift ["
                            + cumultTime1 + "] ms");

                    try {
                        long t1 = System.currentTimeMillis();
                        dao.persistAll(requestRecords);
                        statusHandler.info("Time to persist requests to db ["
                                + (System.currentTimeMillis() - t1) + "] ms");
                        names.add(subscriptionName);
                    } catch (Exception e) {
                        statusHandler.warn("Subscription: " + subscriptionName
                                + " Failed to store to retrievals.");
                        // this should send notification
                    }
                } else {
                    statusHandler.warn("Subscription: " + subscriptionName
                            + " Did not generate any retrieval messages");
                    // should this send notification
                }
            }
            return names;
        } else {
            statusHandler.info("NO VALID SUBCSRIPTIONS NEEDING RETRIEVAL....");
        }

        return null;

    }
}
