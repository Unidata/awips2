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
package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.util.Random;

import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.AbstractFixture;

/**
 * Fixture for {@link SubscriptionRetrievalAttributes}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013 2106       djohnson     Initial creation
 * Oct 21, 2013   2292     mpduff       Implement multiple data types.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionRetrievalAttributesFixture extends
        AbstractFixture<SubscriptionRetrievalAttributes> {

    public static final SubscriptionRetrievalAttributesFixture INSTANCE = new SubscriptionRetrievalAttributesFixture();

    /**
     * Private.
     */
    private SubscriptionRetrievalAttributesFixture() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrievalAttributes getInstance(long seedValue,
            Random random) {
        SubscriptionRetrieval retrieval = SubscriptionRetrievalFixture.INSTANCE
                .get(seedValue);

        SubscriptionRetrievalAttributes entity = new SubscriptionRetrievalAttributes();
        entity.setSubscriptionRetrieval(retrieval);
        try {
            entity.setSubscription(SiteSubscriptionFixture.INSTANCE.get(
                    seedValue, DataType.GRID));
        } catch (SerializationException e) {
            throw new RuntimeException(e);
        }

        return entity;
    }

}
