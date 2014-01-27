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

import java.util.Random;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;

/**
 * Adds attributes specific to {@link SiteSubscription} types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2013 1841       djohnson     Initial creation
 * Oct 2,  2013 1797       dhladky      Updated to work with generics
 * Oct 21, 2013   2292     mpduff      Implement multiple data types
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseSiteSubscriptionFixture<M extends SiteSubscription>
        extends BaseSubscriptionFixture<M> {

    /**
     * {@inheritDoc}
     */
    @Override
    public M getInstance(long seedValue, Random random, DataType dataType) {
        M subscription = super.getInstance(seedValue, random, dataType);

        subscription.setOwner("owner" + random.nextInt());
        subscription.setId(RegistryUtil.getRegistryObjectKey(subscription));

        return subscription;
    }

    /**
     * @return
     */
    @Override
    protected abstract M getSubscription();
}
