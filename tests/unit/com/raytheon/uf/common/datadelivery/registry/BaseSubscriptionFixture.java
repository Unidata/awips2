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

import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Move in reusable code from {@link SiteSubscriptionFixture}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1187       djohnson     Initial creation
 * Oct 16, 2012 0726       djohnson     Use other fixtures to get appropriate values.
 * Jan 30, 2013 1543       djohnson     Add coverage/parameter data.
 * Mar 28, 2013 1841       djohnson     Subscription is now UserSubscription.
 * Apr 08, 2013 1826       djohnson     Remove delivery options.
 * May 15, 2013 1040       mpduff       Office Ids are now a list.
 * Oct 2   2013 1797       dhladky      subscription and time generics
 * Oct 21, 2013   2292     mpduff       Implement multiple data types
 * Jan 14, 2014   2459     mpduff       Change subscription status code
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseSubscriptionFixture<M extends Subscription> {
    private static long DEFAULT_SEED = 1L;

    /**
     * Retrieve an instance using the default seed value.
     * 
     * @return the instance
     */
    public final M get(DataType dataType) {
        return get(DEFAULT_SEED, dataType);
    }

    /**
     * Retrieve an instance based on the specified seed value.
     * 
     * @param seedValue
     *            the seed value
     * @return the instance based on the seed value
     */
    public final M get(long seedValue, DataType dataType) {
        Random random = new Random(seedValue);

        return getInstance(seedValue, random, dataType);
    }

    public M getInstance(long seedValue, Random random, DataType dataType) {
        M subscription = getSubscription();
        subscription.setActivePeriodStart(TimeUtil.newDate());
        subscription.setActivePeriodEnd(new Date(subscription
                .getActivePeriodStart().getTime() + seedValue));
        if (dataType == DataType.GRID) {
            subscription.setCoverage(GriddedCoverageFixture.INSTANCE
                    .get(seedValue));
            subscription
                    .setDataSetName(OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                            .get(seedValue).getDataSetName());
            subscription.setDataSetType(DataType.GRID);
            subscription.setTime(GriddedTimeFixture.INSTANCE.get(seedValue));
        } else if (dataType == DataType.POINT) {
            subscription.setDataSetType(DataType.POINT);
            subscription.setCoverage(CoverageFixture.INSTANCE.get(seedValue));
            subscription.setDataSetName(PointDataSetMetaDataFixture.INSTANCE
                    .get(seedValue).getDataSetName());
            subscription.setTime(PointTimeFixture.INSTANCE.get(seedValue));
        }
        subscription.setDataSetSize(seedValue);
        subscription.setDeleted(random.nextBoolean());
        subscription.setDescription("description" + random.nextInt());
        subscription.setFullDataSet(random.nextBoolean());
        subscription.setGroupName("group" + random.nextInt());
        subscription.setName("name" + seedValue);
        subscription.addOfficeID("officeID" + random.nextInt());
        subscription.addParameter(ParameterFixture.INSTANCE.get());
        // Same priority for all, individual tests needing to test specific
        // priorities should set it manually anyway
        subscription.setPriority(SubscriptionPriority.NORMAL);
        subscription.setProvider(ProviderFixture.INSTANCE.get(seedValue)
                .getName());
        subscription.setSubscriptionStart(subscription.getActivePeriodStart());
        subscription.setSubscriptionEnd(null);
        subscription.setSubscriptionId("subscriptionId" + random.nextInt());
        subscription.setUrl("http://someurl/" + random.nextInt());

        subscription.setId(RegistryUtil.getRegistryObjectKey(subscription));

        return subscription;
    }

    /**
     * @return
     */
    protected abstract M getSubscription();
}
