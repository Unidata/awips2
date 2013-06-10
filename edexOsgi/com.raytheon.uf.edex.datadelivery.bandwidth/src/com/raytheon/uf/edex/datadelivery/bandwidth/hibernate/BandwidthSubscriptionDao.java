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

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;

/**
 * Data access object for {@link BandwidthSubscription} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2013 1543       djohnson     Initial creation
 * Feb 22, 2013 1543       djohnson     Made public as YAJSW doesn't like Spring exceptions.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthSubscriptionDao extends
        SessionManagedDao<Long, BandwidthSubscription> implements
        IBandwidthSubscriptionDao {

    private static final String GET_SUBSCRIPTIONDAO_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = "from BandwidthSubscription sub where "
            + "  sub.provider = :provider and "
            + "  sub.dataSetName = :dataSetName and "
            + "  sub.baseReferenceTime = :baseReferenceTime";

    private static final String GET_SUBSCRIPTIONDAO_BY_REGISTRY_ID_AND_BASEREFERENCETIME = "from BandwidthSubscription sub where "
            + "sub.registryId = :registryId and "
            + "sub.baseReferenceTime = :baseReferenceTime";

    private static final String GET_SUBSCRIPTIONDAO_BY_REGISTRYID = "from BandwidthSubscription sub where "
            + "sub.registryId = :registryId";

    private static final String GET_SUBSCRIPTIONDAO_BY_SUBSCRIPTION = "from BandwidthSubscription sub where "
            + "sub.owner = :owner and "
            + "sub.provider = :provider and "
            + "sub.name = :name and " + "sub.dataSetName = :dataSetName";

    /**
     * Constructor.
     */
    public BandwidthSubscriptionDao() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<BandwidthSubscription> getEntityClass() {
        return BandwidthSubscription.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription getByRegistryIdReferenceTime(
            String registryId, Calendar baseReferenceTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("registryId", registryId);
        params.put("baseReferenceTime", baseReferenceTime);
        return uniqueResult(
                GET_SUBSCRIPTIONDAO_BY_REGISTRY_ID_AND_BASEREFERENCETIME,
                params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBySubscription(
            Subscription subscription) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("owner", subscription.getOwner());
        params.put("provider", subscription.getProvider());
        params.put("name", subscription.getName());
        params.put("dataSetName", subscription.getDataSetName());
        return query(GET_SUBSCRIPTIONDAO_BY_SUBSCRIPTION, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getByRegistryId(String registryId) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("registryId", registryId);
        return query(GET_SUBSCRIPTIONDAO_BY_REGISTRYID, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getByProviderDataSetReferenceTime(
            String provider, String dataSetName, Calendar baseReferenceTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("provider", provider);
        params.put("dataSetName", dataSetName);
        params.put("baseReferenceTime", baseReferenceTime);
        return query(
                GET_SUBSCRIPTIONDAO_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME,
                params);
    }
}
