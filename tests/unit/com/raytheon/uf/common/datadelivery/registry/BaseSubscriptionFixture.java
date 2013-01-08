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
package com.raytheon.uf.common.datadelivery.registry;

import java.util.Date;
import java.util.Random;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.AbstractFixture;

/**
 * Move in reusable code from {@link SubscriptionFixture}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1187       djohnson     Initial creation
 * Oct 16, 2012 0726       djohnson     Use other fixtures to get appropriate values.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseSubscriptionFixture<T extends Subscription> extends
        AbstractFixture<T> {

    /**
     * {@inheritDoc}
     */
    @Override
    public T get(long seedValue) {
        Random random = new Random(seedValue);

        T subscription = getSubscription();
        subscription.setActive(random.nextBoolean());
        subscription.setActivePeriodStart(TimeUtil.newDate());
        subscription.setActivePeriodEnd(new Date(subscription
                .getActivePeriodStart().getTime() + seedValue));
        // TODO: Create coverage fixture
        // subscription.setCoverage(coverage)
        subscription
                .setDataSetName(OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                        .get(seedValue).getDataSetName());
        subscription.setDataSetSize(seedValue);
        subscription.setDataSetType(AbstractFixture.randomEnum(DataType.class,
                random));
        subscription.setDeleted(random.nextBoolean());
        subscription.setDescription("description" + random.nextInt());
        subscription.setFullDataSet(random.nextBoolean());
        subscription.setGroupName("group" + random.nextInt());
        subscription.setName("name" + random.nextInt());
        subscription.setNotify(random.nextBoolean());
        subscription.setOfficeID("officeID" + random.nextInt());
        subscription.setOwner("owner" + random.nextInt());
        subscription.setParameter(Lists.<Parameter> newArrayList());
        // Same priority for all, individual tests needing to test specific
        // priorities should set it manually anyway
        subscription.setPriority(1);
        subscription.setProvider(ProviderFixture.INSTANCE.get(seedValue)
                .getName());
        subscription.setSubscriptionStart(subscription.getActivePeriodStart());
        subscription.setSubscriptionEnd(null);
        subscription.setSubscriptionId("subscriptionId" + random.nextInt());
        subscription.setTime(TimeFixture.INSTANCE.get(seedValue));
        subscription.setUrl("http://someurl/" + random.nextInt());

        subscription.setId(RegistryUtil.getRegistryObjectKey(subscription));

        return subscription;
    }

    /**
     * @return
     */
    protected abstract T getSubscription();
}
