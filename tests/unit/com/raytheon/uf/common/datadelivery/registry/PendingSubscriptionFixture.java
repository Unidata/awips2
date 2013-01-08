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

import com.raytheon.uf.common.util.AbstractFixture;

/**
 * {@link AbstractFixture} implementation for {@link PendingSubscription}
 * objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2012 1187       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class PendingSubscriptionFixture extends
        BaseSubscriptionFixture<PendingSubscription> {

    public static final PendingSubscriptionFixture INSTANCE = new PendingSubscriptionFixture();

    /**
     * Disabled constructor.
     */
    private PendingSubscriptionFixture() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PendingSubscription get(long seedValue) {
        PendingSubscription sub = super.get(seedValue);
        sub.setChangeReqId("change" + seedValue);

        return sub;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PendingSubscription getSubscription() {
        return new PendingSubscription();
    }

}
