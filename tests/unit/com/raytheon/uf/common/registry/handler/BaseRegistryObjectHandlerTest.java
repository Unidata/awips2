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
package com.raytheon.uf.common.registry.handler;

import java.util.Arrays;

import org.junit.Test;

import com.raytheon.uf.common.registry.MockRegistryObject;
import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryResponse;

/**
 * Test {@link BaseRegistryObjectHandler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 1169       djohnson     Initial creation
 * Oct 17, 2012 0726       djohnson     Simplified test.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class BaseRegistryObjectHandlerTest {

    @Test(expected = RegistryHandlerException.class)
    public void testFailedStoreResponseThrowsException()
            throws RegistryHandlerException {
        RegistryResponse<MockRegistryObject> response = new RegistryResponse<MockRegistryObject>();
        response.setRegistryObjects(Arrays.asList(new MockRegistryObject()));
        response.setStatus(OperationStatus.FAILED);

        MockRegistryObjectHandler.checkResponse(response,
                new MockRegistryObject(), "store");
    }

    @Test
    public void testSuccessfulStoreResponseDoesNotThrowException()
            throws RegistryHandlerException {
        RegistryResponse<MockRegistryObject> response = new RegistryResponse<MockRegistryObject>();
        response.setRegistryObjects(Arrays.asList(new MockRegistryObject()));
        response.setStatus(OperationStatus.SUCCESS);

        MockRegistryObjectHandler.checkResponse(response,
                new MockRegistryObject(), "store");
    }
}
