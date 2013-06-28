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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import java.util.Random;

import com.raytheon.uf.common.datadelivery.registry.GriddedCoverageFixture;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval.SubscriptionType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.AbstractFixture;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.MockOpenDapServiceFactory;

/**
 * Fixture for {@link RetrievalRequestRecord}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013 1543       djohnson     Initial creation
 * Feb 15, 2013 1543       djohnson     Set coverage on retrieval attributes.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RetrievalRequestRecordFixture extends
        AbstractFixture<RetrievalRequestRecord> {

    public static final RetrievalRequestRecordFixture INSTANCE = new RetrievalRequestRecordFixture();

    /**
     * Private constructor.
     */
    private RetrievalRequestRecordFixture() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalRequestRecord getInstance(long seedValue, Random random) {
        Subscription subscription = SiteSubscriptionFixture.INSTANCE.get(seedValue);
        final Provider provider = ProviderFixture.INSTANCE.get(seedValue);

        SubscriptionBundle bundle = new SubscriptionBundle();
        bundle.setBundleId(subscription.getSubscriptionId());
        bundle.setPriority(1);
        bundle.setProvider(provider);
        bundle.setConnection(provider.getConnection());
        bundle.setSubscription(subscription);

        RetrievalRequestRecord rec = new RetrievalRequestRecord(
                subscription.getName(), 0, seedValue);

        rec.setId(new RetrievalRequestRecordPK(subscription.getName(), 0));
        rec.setOwner(subscription.getOwner());
        rec.setPriority(1);
        rec.setInsertTime(TimeUtil.newDate());
        rec.setNetwork(subscription.getRoute());
        rec.setProvider(subscription.getProvider());
        rec.setPlugin("grib");
        rec.setSubscriptionType(SubscriptionType.SUBSCRIBED);
        rec.setState(RetrievalRequestRecord.State.PENDING);

        try {
            final Retrieval retrieval = new MockOpenDapServiceFactory(provider)
                    .getRetrievalGenerator().buildRetrieval(bundle).iterator()
                    .next();

            for (RetrievalAttribute attribute : retrieval.getAttributes()) {
                attribute.setCoverage(GriddedCoverageFixture.INSTANCE
                        .get(seedValue));
            }

            rec.setRetrieval(SerializationUtil.transformToThrift(retrieval));
        } catch (SerializationException e) {
            throw new RuntimeException(e);
        }

        return rec;
    }

}
