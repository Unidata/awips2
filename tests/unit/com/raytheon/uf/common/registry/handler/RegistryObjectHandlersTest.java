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

import static org.junit.Assert.assertSame;

import org.junit.Before;
import org.junit.Test;

/**
 * Test {@link RegistryObjectHandlers}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 1169       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RegistryObjectHandlersTest {


    /**
     * Clears out all current handler registrations.
     */
    public static void clear() {
        RegistryObjectHandlers.clear();
    }

    @Before
    public void setUp() {
        RegistryObjectHandlersTest.clear();
    }

    @Test
    public void testRegisteredHandlerIsRetrievable() {
        final MockRegistryObjectHandler handler = new MockRegistryObjectHandler();

        RegistryObjectHandlers.register(IMockRegistryObjectHandler.class,
                handler);

        assertSame(handler,
                RegistryObjectHandlers.get(IMockRegistryObjectHandler.class));
    }

    @Test(expected = IllegalStateException.class)
    public void testAssociatingTwoHandlersWithSameInterfaceThrowsException() {
        RegistryObjectHandlers.register(IMockRegistryObjectHandler.class,
                new MockRegistryObjectHandler());
        RegistryObjectHandlers.register(IMockRegistryObjectHandler.class,
                new MockRegistryObjectHandler());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNoHandlerRegisteredThrowsExceptionOnGet() {
        RegistryObjectHandlers.get(IMockRegistryObjectHandler.class);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRegisterWithNonInterfaceArgumentThrowsException() {
        RegistryObjectHandlers.register(MockRegistryObjectHandler.class,
                new MockRegistryObjectHandler());
    }
}
