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
package com.raytheon.uf.edex.registry.ebxml.util;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.Arrays;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.registry.IRegistryRequest;
import com.raytheon.uf.common.registry.IRegistryRequest.Action;
import com.raytheon.uf.common.registry.RegistryManagerTest;
import com.raytheon.uf.common.status.IUFStatusHandler;

/**
 * Test {@link EDEXRegistryManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 1169       djohnson     Tests for more than one item provided to store/storeOrReplace.
 * Oct 17, 2012 0726       djohnson     Use {@link RegistryManagerTest#setMockInstance()}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EDEXRegistryManagerTest {

    private final IUFStatusHandler statusHandler = mock(IUFStatusHandler.class);

    @BeforeClass
    public static void classSetUp() {
        RegistryManagerTest.setMockInstance();
    }

    @Test
    public void testErrorMessageOnMoreThanOneItemForStore() throws Exception {
        EDEXRegistryManager.statusHandler = statusHandler;

        IRegistryRequest<String> request = new IRegistryRequest<String>();
        request.setAction(Action.STORE);
        request.setObjects(Arrays.asList("one", "two"));

        new EDEXRegistryManager()
                .handleRequest(request);

        verify(statusHandler).error(
                EDEXRegistryManager.CAN_ONLY_STORE_SINGLE_OBJECT);
    }

    @Test
    public void testErrorMessageOnMoreThanOneItemForStoreOrReplace()
            throws Exception {
        EDEXRegistryManager.statusHandler = statusHandler;

        IRegistryRequest<String> request = new IRegistryRequest<String>();
        request.setAction(Action.STORE_OR_REPLACE);
        request.setObjects(Arrays.asList("one", "two"));

        new EDEXRegistryManager()
                .handleRequest(request);

        verify(statusHandler).error(
                EDEXRegistryManager.CAN_ONLY_STORE_SINGLE_OBJECT);
    }

    @Test
    public void testNoErrorMessageOnOneItemForStore() throws Exception {
        EDEXRegistryManager.statusHandler = statusHandler;

        IRegistryRequest<String> request = new IRegistryRequest<String>();
        request.setAction(Action.STORE);
        request.setObjects(Arrays.asList("one"));

        new EDEXRegistryManager()
                .handleRequest(request);

        verify(statusHandler, never()).error(
                EDEXRegistryManager.CAN_ONLY_STORE_SINGLE_OBJECT);
    }

    @Test
    public void testNoErrorMessageOnOneItemForStoreOrReplace() throws Exception {
        EDEXRegistryManager.statusHandler = statusHandler;

        IRegistryRequest<String> request = new IRegistryRequest<String>();
        request.setAction(Action.STORE_OR_REPLACE);
        request.setObjects(Arrays.asList("one"));

        new EDEXRegistryManager()
                .handleRequest(request);

        verify(statusHandler, never()).error(
                EDEXRegistryManager.CAN_ONLY_STORE_SINGLE_OBJECT);
    }

    @Test
    public void testNoErrorMessageOnMoreThanOneItemForRemove() throws Exception {
        EDEXRegistryManager.statusHandler = statusHandler;

        IRegistryRequest<String> request = new IRegistryRequest<String>();
        request.setAction(Action.REMOVE);
        request.setObjects(Arrays.asList("one", "two"));

        new EDEXRegistryManager()
                .handleRequest(request);

        verify(statusHandler, never()).error(
                EDEXRegistryManager.CAN_ONLY_STORE_SINGLE_OBJECT);
    }
}
