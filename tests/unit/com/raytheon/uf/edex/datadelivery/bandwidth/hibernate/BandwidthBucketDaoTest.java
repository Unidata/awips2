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

import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.TransactionConfiguration;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.bandwidth.AbstractBandwidthBucketDaoTest;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;

/**
 * Test {@link BandwidthBucketDao}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2013 2106       djohnson     Initial creation
 * Sep 17, 2013 2383       bgonzale     Added test for same start and end time.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.UNIT_TEST_DB_BEANS_XML,
        "bandwidthBucketDaoTest.xml" })
@TransactionConfiguration(transactionManager = TestUtil.METADATA_TX_MANAGER, defaultRollback = true)
@Transactional
public class BandwidthBucketDaoTest extends AbstractBandwidthBucketDaoTest {

    @Autowired
    private BandwidthBucketDao hibernateDao;

    // Not sure why, but the transaction is not picked up unless I override all
    // of the superclass test methods. I wish I didn't have to, but here we
    // are....

    /**
     * {@inheritDoc}
     */
    @Override
    public void createdBandwidthBucketIsRetrievable() {
        super.createdBandwidthBucketIsRetrievable();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void bandwidthBucketIsUpdatable() {
        super.bandwidthBucketIsUpdatable();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteEmptyBucketsUpToTimeRemovesEmptyBucketsBeforeTime()
            throws DataAccessLayerException {
        super.deleteEmptyBucketsUpToTimeRemovesEmptyBucketsBeforeTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteEmptyBucketsUpToTimeRemovesEmptyBucketAtTime()
            throws DataAccessLayerException {
        super.deleteEmptyBucketsUpToTimeRemovesEmptyBucketAtTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteEmptyBucketsUpToTimeDoesNotRemoveNonEmptyBucketsBeforeTime()
            throws DataAccessLayerException {
        super.deleteEmptyBucketsUpToTimeDoesNotRemoveNonEmptyBucketsBeforeTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteEmptyBucketsUpToTimeDoesNotRemoveNonEmptyBucketsAtTime()
            throws DataAccessLayerException {
        super.deleteEmptyBucketsUpToTimeDoesNotRemoveNonEmptyBucketsAtTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteEmptyBucketsUpToTimeDoesNotRemoveEmptyBucketsAfterTime()
            throws DataAccessLayerException {
        super.deleteEmptyBucketsUpToTimeDoesNotRemoveEmptyBucketsAfterTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getAllForNetworkReturnsAllBucketsInNetwork() {
        super.getAllForNetworkReturnsAllBucketsInNetwork();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getAllForNetworkDoesNotReturnBucketsNotInNetwork() {
        super.getAllForNetworkDoesNotReturnBucketsNotInNetwork();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getLastBucketForNetworkReturnsLastBucket() {
        super.getLastBucketForNetworkReturnsLastBucket();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getWhereStartTimeIsLessThanOrEqualToReturnsAllBucketsBeforeGivenTime() {
        super.getWhereStartTimeIsLessThanOrEqualToReturnsAllBucketsBeforeGivenTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getWhereStartTimeIsLessThanOrEqualToReturnsBucketAtGivenTime() {
        super.getWhereStartTimeIsLessThanOrEqualToReturnsBucketAtGivenTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getWhereStartTimeIsLessThanOrEqualToDoesNotReturnBucketAfterGivenTime() {
        super.getWhereStartTimeIsLessThanOrEqualToDoesNotReturnBucketAfterGivenTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getFirstBucketForNetworkReturnsFirstBucket() {
        super.getFirstBucketForNetworkReturnsFirstBucket();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getByStartTimeReturnsBucketWithGivenStartTime() {
        super.getByStartTimeReturnsBucketWithGivenStartTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getBucketsInWindowsReturnsBucketsBetweenTimes() {
        super.getBucketsInWindowsReturnsBucketsBetweenTimes();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getBucketsInWindowsReturnsBucketAtStartTime() {
        super.getBucketsInWindowsReturnsBucketAtStartTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getBucketsInWindowsReturnsBucketAtEndTime() {
        super.getBucketsInWindowsReturnsBucketAtEndTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getBucketsInWindowsWhereStartTimeEqualsEndTime() {
        super.getBucketsInWindowsWhereStartTimeEqualsEndTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void copyStateCopiesAllBuckets() {
        super.copyStateCopiesAllBuckets();
    }

    @Override
    public IBandwidthBucketDao getBandwidthBucketDao() {
        return hibernateDao;
    }
}
