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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.jdbc.Work;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionRetrievalDao extends
        BaseBandwidthAllocationDao<SubscriptionRetrieval> implements ISubscriptionRetrievalDao {

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE = "from SubscriptionRetrieval sr where "
            + " sr.bandwidthSubscription.id in ("
            + "  select sub.id from BandwidthSubscription sub where "
            + "  sub.provider = :provider and "
            + "  sub.dataSetName = :dataSetName";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE
            + ")";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE
            + " and sub.baseReferenceTime = :baseReferenceTime)";

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
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("provider", provider);
        params.put("dataSetName", dataSetName);
        params.put("baseReferenceTime", baseReferenceTime);
        return query(
                GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME,
                params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getByProviderDataSet(
            String provider, String dataSetName) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("provider", provider);
        params.put("dataSetName", dataSetName);
        return query(GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    // TODO: Remove the requirement of this method
    public void doWork(Work work) {
        template.getSessionFactory().getCurrentSession().doWork(work);
    }
}
