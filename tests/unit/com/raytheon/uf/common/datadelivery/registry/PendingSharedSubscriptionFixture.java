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


/**
 * Fixture for {@link PendingSharedSubscription} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013 1650       djohnson     Initial creation
 * Oct 2,  2013 1797       dhladky      Updated to work with generics
 * Oct 21, 2013   2292     mpduff       Implement multiple data types
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class PendingSharedSubscriptionFixture
        extends
        BaseSharedSubscriptionFixture<PendingSharedSubscription<Time, Coverage>> {

    public static final PendingSharedSubscriptionFixture INSTANCE = new PendingSharedSubscriptionFixture();

    /**
     * Constructor.
     */
    private PendingSharedSubscriptionFixture() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PendingSharedSubscription<Time, Coverage> getInstance(
            long seedValue, Random random, DataType dataType) {
        PendingSharedSubscription<Time, Coverage> sub = super.getInstance(
                seedValue, random, dataType);
        sub.setChangeReqId("change" + seedValue);

        return sub;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PendingSharedSubscription<Time, Coverage> getSubscription() {
        return new PendingSharedSubscription<Time, Coverage>();
    }

}
