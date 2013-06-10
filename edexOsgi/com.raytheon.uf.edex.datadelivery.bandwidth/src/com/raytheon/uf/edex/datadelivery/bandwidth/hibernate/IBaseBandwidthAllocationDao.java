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
package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.edex.database.dao.ISessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;

/**
 * Base DAO interface for bandwidth allocations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 * @param <ENTITY>
 */
interface IBaseBandwidthAllocationDao<ENTITY extends BandwidthAllocation>
        extends ISessionManagedDao<Long, ENTITY> {

    /**
     * Get by the subscription id.
     * 
     * @param subscriptionId
     *            the subscription id
     * @return
     */
    List<ENTITY> getBySubscriptionId(Long subscriptionId);

    /**
     * Get by the network.
     * 
     * @param network
     * @return
     */
    List<ENTITY> getByNetwork(Network network);

    /**
     * Get by retrieval status.
     * 
     * @param state
     * @return
     */
    List<ENTITY> getByState(RetrievalStatus state);

    /**
     * Get deferred bandwidth allocations for the network and end time.
     * 
     * @param network
     * @param endTime
     * @return
     */
    List<ENTITY> getDeferred(Network network, Calendar endTime);

}