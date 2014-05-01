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
package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import com.google.common.collect.Sets;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;

/**
 * * DAO that handles {@link SubscriptionRetrieval} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Initial creation
 * Feb 22, 2013 1543       djohnson     Made public as YAJSW doesn't like Spring exceptions.
 * 4/9/2013     1802       bphillip     Changed to use new query method signatures in SessionManagedDao
 * Jun 03, 2013 2038       djohnson     Add method to get subscription retrievals by provider, dataset, and status.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionRetrievalDao extends
        BaseBandwidthAllocationDao<SubscriptionRetrieval> implements
        ISubscriptionRetrievalDao {

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET = "from SubscriptionRetrieval sr where "
            + "sr.bandwidthSubscription.provider = :provider and "
            + "sr.bandwidthSubscription.dataSetName = :dataSetName";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET
            + " and sr.bandwidthSubscription.baseReferenceTime = :baseReferenceTime)";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_STATUS = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET
            + " and sr.status = :status order by sr.startTime";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_DATASET_STATUS_AND_DATES = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET
            + " and sr.status = :status"
            + " and sr.startTime >= :startDate"
            + " and sr.startTime <= :endDate"
            + " order by sr.startTime ";

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<SubscriptionRetrieval> getEntityClass() {
        return SubscriptionRetrieval.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getByProviderDataSetReferenceTime(
            String provider, String dataSetName, Calendar baseReferenceTime) {
        return query(
                GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME,
                "provider", provider, "dataSetName", dataSetName,
                "baseReferenceTime", baseReferenceTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getByProviderDataSet(String provider,
            String dataSetName) {
        return query(GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET,
                    "provider", provider, "dataSetName", dataSetName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SortedSet<SubscriptionRetrieval> getByProviderDataSetAndStatus(
            String provider, String dataSetName, RetrievalStatus status) {

        final List<SubscriptionRetrieval> results = query(
                GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_STATUS,
                "provider", provider, "dataSetName", dataSetName, "status",
                status);

        return orderByStart(results);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SortedSet<SubscriptionRetrieval> getByProviderDataSetStatusAndDateRange(
            String provider, String dataSetName, RetrievalStatus status,
            Date earliestDate, Date latestDate) {

        final Calendar startDate = Calendar.getInstance();
        startDate.setTime(earliestDate);

        final Calendar endDate = Calendar.getInstance();
        endDate.setTime(latestDate);

        final List<SubscriptionRetrieval> results = query(
                GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_DATASET_STATUS_AND_DATES,
                "provider", provider, "dataSetName", dataSetName, "status",
                status, "startDate", startDate, "endDate", endDate);

        return orderByStart(results);
    }

    /**
     * Returns a {@link SortedSet} that orders the subscription retrievals by
     * start date.
     * 
     * @param results
     *            the results
     * @return the set
     */
    private SortedSet<SubscriptionRetrieval> orderByStart(
            List<SubscriptionRetrieval> results) {
        final TreeSet<SubscriptionRetrieval> treeSet = Sets
                .newTreeSet(new Comparator<SubscriptionRetrieval>() {
                    @Override
                    public int compare(SubscriptionRetrieval o1,
                            SubscriptionRetrieval o2) {
                        return o1.getStartTime().compareTo(o2.getStartTime());
                    }
                });
        treeSet.addAll(results);
        return treeSet;
    }

}
