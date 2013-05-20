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
package com.raytheon.uf.edex.datadelivery.bandwidth.notification;

import java.util.Arrays;
import java.util.List;

import com.google.common.eventbus.EventBus;

/**
 * A synchronous event bus factory for the bandwidth manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Initial creation
 * May 28, 2013 1650       djohnson     Add getEventBuses.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class BandwidthSyncEventBusFactory implements BandwidthEventBusFactory {

    private final EventBus dataSetBus = new EventBus();

    private final EventBus subscriptionBus = new EventBus();

    private final EventBus retrievalBus = new EventBus();

    /**
     * {@inheritDoc}
     */
    @Override
    public EventBus getSubscriptionBus() {
        return subscriptionBus;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventBus getRetrievalBus() {
        return retrievalBus;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventBus getDataSetBus() {
        return dataSetBus;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<EventBus> getEventBuses() {
        return Arrays.<EventBus> asList(dataSetBus, retrievalBus,
                subscriptionBus);
    }
}
