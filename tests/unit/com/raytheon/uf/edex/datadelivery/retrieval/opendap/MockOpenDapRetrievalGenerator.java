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
package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Ignore;

import com.raytheon.uf.common.datadelivery.registry.GriddedCoverageFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;

/**
 * Overrides methods in {@link OpenDAPRetrievalGenerator} which require external
 * systems (e.g. databases, spring, etc.).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class MockOpenDapRetrievalGenerator extends OpenDAPRetrievalGenerator {

    /**
     * {@inheritDoc}
     */
    @Override
    protected RetrievalAdapter getServiceRetrievalAdapter() {
        return new MockOpenDapRetrievalAdapter();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Retrieval> buildRetrieval(SubscriptionBundle bundle) {
        final Subscription subscription = bundle.getSubscription();

        Retrieval retrieval = new Retrieval();
        retrieval.setConnection(bundle.getConnection());
        retrieval.setNetwork(subscription.getRoute());
        retrieval.setOwner(subscription.getOwner());
        retrieval.setProviderType(bundle.getProvider().getProviderType()
                .iterator().next());
        retrieval.setServiceType(this.getServiceType());
        retrieval.setSubscriptionName(subscription.getName());
        retrieval.setSubscriptionType(getSubscriptionType(subscription));

        final ArrayList<RetrievalAttribute> attributes = new ArrayList<RetrievalAttribute>();
        attributes.add(getAttribute(bundle));
        attributes.add(getAttribute(bundle));
        retrieval.setAttributes(attributes);

        return Arrays.asList(retrieval);
    }

    /**
     * @param bundle
     * @return
     */
    private RetrievalAttribute getAttribute(SubscriptionBundle bundle) {
        RetrievalAttribute attribute = new RetrievalAttribute();
        attribute.setCoverage(GriddedCoverageFixture.INSTANCE.get());
        final Subscription subscription = bundle.getSubscription();
        attribute.setSubName(subscription.getName());
        attribute.setProvider(bundle.getProvider().getName());
        attribute.setParameter(subscription.getParameter().get(0));
        attribute.setTime(subscription.getTime());
        attribute.setPlugin("grid");
        return attribute;
    }
}
