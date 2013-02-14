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
package com.raytheon.uf.edex.datadelivery.service.verify;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetFixture;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.ParameterFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;

/**
 * Test {@link BaseSubscriptionVerifier}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2012 1104       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class BaseSubscriptionVerifierTest {

    private final BaseSubscriptionVerifier verifier = new BaseSubscriptionVerifier();

    private final OpenDapGriddedDataSet dataSet = OpenDapGriddedDataSetFixture.INSTANCE
            .get();

    private final Subscription subscription = SubscriptionFixture.INSTANCE
            .get();


    @Before
    public void setUp() {
        Map<String, Parameter> dataSetParams = new HashMap<String, Parameter>();
        Parameter dataSetParameter = ParameterFixture.INSTANCE.get(1);
        dataSetParams.put(dataSetParameter.getName(), dataSetParameter);
        dataSet.setParameters(dataSetParams);
    }

    @Test
    public void verifySucceedsWhenDataSetStillContainsSubscriptionParameters() {
        // Uses the same seed value, so "equal" to the first one
        subscription.addParameter(ParameterFixture.INSTANCE.get(1));

        assertFalse("Should not have failed verification!",
                verifier.verify(dataSet, subscription).hasFailedVerification());
    }

    @Test
    public void verifyFailsWhenDataSetDoesNotContainSubscriptionParameters() {
        // Uses different seed value, so "not equal" to the first one
        subscription.addParameter(ParameterFixture.INSTANCE.get(2));

        assertTrue("Should have failed verification!",
                verifier.verify(dataSet, subscription).hasFailedVerification());
    }

}
