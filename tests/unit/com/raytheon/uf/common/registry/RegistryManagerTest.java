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
package com.raytheon.uf.common.registry;

import org.junit.Ignore;
import org.mockito.Mockito;

/**
 * Allows setting a specific {@link RegistryHandler} instance for test purposes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012 0743       djohnson     Initial creation
 * Oct 17, 2012 0726       djohnson     Remove MockRegistryHandler.
 * Nov 15, 2012 1286       djohnson     Set handler instance via package-level constructor.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class RegistryManagerTest {

    /**
     * Sets the {@link RegistryManager#instance} to be a mock instance.
     * 
     * @return the mock {@link RegistryHandler}.
     */
    public static RegistryHandler setMockInstance() {
        RegistryHandler mock = Mockito.mock(RegistryHandler.class);
        RegistryManagerTest.setInstance(mock);
        return mock;
    }

    /**
     * Allows setting of the RegistyHandler instance on RegistryManager. Only
     * allowed from tests. This is useful if you want to use a mocking library
     * version of the handler, rather than an actual object.
     * 
     * @param handler
     *            the handler to use
     */
    public static void setInstance(RegistryHandler handler) {
        new RegistryManager(handler);
    }
}
