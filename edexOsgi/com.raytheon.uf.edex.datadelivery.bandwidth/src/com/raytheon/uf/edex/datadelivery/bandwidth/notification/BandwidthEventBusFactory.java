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

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.edex.event.GoogleEventBusFactory;

/**
 * Interface for the bandwidth event bus factory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012            djohnson     Initial creation
 * May 28, 2013 1650       djohnson     Returns the event buses required by extending GoogleEventBusFactory.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

interface BandwidthEventBusFactory extends GoogleEventBusFactory {

    /**
     * Get the data set bus.
     */
    EventBus getDataSetBus();

    /**
     * Get the subscription bus.
     */
    EventBus getSubscriptionBus();

    /**
     * Get the retrieval bus.
     */
    EventBus getRetrievalBus();

}
