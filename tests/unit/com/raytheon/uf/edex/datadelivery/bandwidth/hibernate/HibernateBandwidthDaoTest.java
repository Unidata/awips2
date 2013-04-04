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

import static org.junit.Assert.assertEquals;

import java.util.Calendar;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.edex.database.dao.DatabaseUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.SubscriptionRetrievalAgent;

/**
 * Test {@link HibernateBandwidthDaoTest}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2012 0726       djohnson     Initial creation
 * Oct 26, 2012 1286       djohnson     Renamed.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class HibernateBandwidthDaoTest {

    @Before
    public void setUp() throws Exception {
        DatabaseUtil.start();
    }

    @After
    public void tearDown() throws Exception {
        DatabaseUtil.shutdown();
    }

    @Test
    public void testPersistence() throws Exception {

        final IBandwidthDao dao = HibernateBandwidthDao.getInstance();

        BandwidthAllocation allocation = new BandwidthAllocation();
        allocation.setAgentType(SubscriptionRetrievalAgent.SUBSCRIPTION_AGENT);
        allocation.setEndTime(Calendar.getInstance());
        allocation.setEstimatedSize(-1L);
        allocation.setPriority(1.0D);
        allocation.setNetwork(Network.OPSNET);
        allocation.setStartTime(Calendar.getInstance());

        dao.store(allocation);
        List<BandwidthAllocation> allocations = dao
                .getBandwidthAllocations(Network.OPSNET);
        assertEquals("Expected to find one persisted entity!", 1,
                allocations.size());
    }

    @Test
    public void testLookup() throws Exception {
        final IBandwidthDao dao = HibernateBandwidthDao.getInstance();

        List<BandwidthAllocation> allocations = dao
                .getBandwidthAllocations(Network.OPSNET);
        assertEquals("Expected not to find a persisted entity!", 0,
                allocations.size());
    }
}
