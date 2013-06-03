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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.HibernateBandwidthDao;

/**
 * Implementation of {@link AbstractBandwidthDaoTest} for
 * {@link HibernateBandwidthDao}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_DAOS_XML,
        SpringFiles.RETRIEVAL_DATADELIVERY_DAOS_XML })
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
public class HibernateBandwidthDaoTest extends
        AbstractBandwidthDaoTest<IBandwidthDao> {

    @Autowired
    private IBandwidthDao dao;

    /**
     * {@inheritDoc}
     */
    @Override
    protected IBandwidthDao getDao() {
        return dao;
    }

}
