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
import java.util.concurrent.Executors;

import com.google.common.eventbus.AsyncEventBus;
import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Creates asynchronous Google event buses.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012 1286       djohnson     Initial creation
 * Feb 06, 2013 1543       djohnson     Changes to correspond with EventBus changes.
 * May 28, 2013 1650       djohnson     Changes to match functionality in general event bus handling.
 * Jul 09, 2013 2106       djohnson     No Spring required to get thread pool sizes, remove subscriptionBus.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthAsyncEventBusFactory implements BandwidthEventBusFactory {
    private final AsyncEventBus dataSetBus;

    private final AsyncEventBus retrievalBus;

    public BandwidthAsyncEventBusFactory() {
        BandwidthEventBusConfig config = new BandwidthEventBusConfig();

        final int dataSetMetaDataPoolSize = config.getDataSetMetaDataPoolSize();
        final int retrievalPoolSize = config.getRetrievalPoolSize();

        UFStatus.getHandler(BandwidthAsyncEventBusFactory.class)
                .info(String
                        .format("Creating event bus with dataSetMetaDataPoolSize [%s] retrievalPoolSize [%s].",
                                dataSetMetaDataPoolSize, retrievalPoolSize));

        dataSetBus = new AsyncEventBus(
                Executors.newFixedThreadPool(dataSetMetaDataPoolSize));
        retrievalBus = new AsyncEventBus(
                Executors.newFixedThreadPool(retrievalPoolSize));
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
    public EventBus getRetrievalBus() {
        return retrievalBus;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<EventBus> getEventBuses() {
        return Arrays.<EventBus> asList(dataSetBus, retrievalBus);
    }

}
