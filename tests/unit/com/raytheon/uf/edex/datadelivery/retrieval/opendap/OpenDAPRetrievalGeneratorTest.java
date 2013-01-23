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
package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaDataFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.registry.RegistryManagerTest;

/**
 * Test {@link OpenDAPRetrievalGenerator}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2012 955        djohnson     Initial creation
 * Aug 10, 2012 1022       djohnson     Remove generics from {@link GriddedDataSetMetaData}.
 * Aug 20, 2012 0743       djohnson     Use {@link RegistryManagerTest} to set the handler.
 * Sep 24, 2012 1209       djohnson     Test for NO_CYCLE metadatas and subscriptions.
 * Oct 17, 2012 0726       djohnson     Remove unused code.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class OpenDAPRetrievalGeneratorTest {

    @Test
    public void testSatisfiesSubscriptionCriteriaReturnsFalseWhenMetaDataIsForNonSubscribedCycle() {
        Time time = new Time();
        time.setCycleTimes(Arrays.asList(0, 12));

        Subscription subscription = new Subscription();
        subscription.setTime(time);
        
        GriddedDataSetMetaData metaData = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        metaData.setCycle(6);

        assertFalse(OpenDAPRetrievalGenerator.satisfiesSubscriptionCriteria(
                subscription, metaData));
    }

    @Test
    public void testSatisfiesSubscriptionCriteriaReturnsFalseWhenMetaDataDoesNotSpecifyCycle() {
        Time time = new Time();
        time.setCycleTimes(Arrays.asList(0, 12));

        Subscription subscription = new Subscription();
        subscription.setTime(time);

        GriddedDataSetMetaData metaData = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        metaData.setCycle(GriddedDataSetMetaData.NO_CYCLE);

        assertFalse(OpenDAPRetrievalGenerator.satisfiesSubscriptionCriteria(
                subscription, metaData));
    }

    @Test
    public void testSatisfiesSubscriptionCriteriaReturnsTrueWhenMetaDataIsForSubscribedCycle() {
        Time time = new Time();
        time.setCycleTimes(Arrays.asList(0, 12));

        Subscription subscription = new Subscription();
        subscription.setTime(time);

        GriddedDataSetMetaData metaData = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        metaData.setCycle(12);

        assertTrue(OpenDAPRetrievalGenerator.satisfiesSubscriptionCriteria(
                subscription, metaData));
    }

    @Test
    public void testSatisfiesSubscriptionCriteriaReturnsTrueWhenMetaDataIsForDailyModel() {
        Time time = new Time();
        time.setCycleTimes(Arrays
                .<Integer> asList(GriddedDataSetMetaData.NO_CYCLE));

        Subscription subscription = new Subscription();
        subscription.setTime(time);

        GriddedDataSetMetaData metaData = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        metaData.setCycle(GriddedDataSetMetaData.NO_CYCLE);

        assertTrue(OpenDAPRetrievalGenerator.satisfiesSubscriptionCriteria(
                subscription, metaData));
    }
}
