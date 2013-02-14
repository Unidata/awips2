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
package dods.dap;

import static org.junit.Assert.assertEquals;

import java.io.FileNotFoundException;

import org.junit.Test;

import com.raytheon.dods.DodsOriginalHttpConnectStrategy;

/**
 * Test {@link DConnect}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 9, 2012  634        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DConnectTest {

    @Test
    public void testSystemPropertyTimeoutsAreSetOnHttpStrategy()
            throws FileNotFoundException {
        final int connectionTimeout = 10;
        final int socketTimeout = 11;

        System.setProperty(DConnect.DODS_CONNECTION_TIMEOUT_MILLISECONDS,
                Integer.toString(connectionTimeout));
        System.setProperty(DConnect.DODS_SOCKET_TIMEOUT_MILLISECONDS,
                Integer.toString(socketTimeout));

        MockHttpConnectStrategy mockHttpStrategy = new MockHttpConnectStrategy();
        new DConnect("file://", null, null, true,
                mockHttpStrategy);

        assertEquals(connectionTimeout, mockHttpStrategy.connectionTimeout);
        assertEquals(socketTimeout, mockHttpStrategy.socketTimeout);
    }

    private class MockHttpConnectStrategy extends
            DodsOriginalHttpConnectStrategy {

        int connectionTimeout;

        int socketTimeout;

        @Override
        public void setConnectionTimeout(int connectionTimeout) {
            this.connectionTimeout = connectionTimeout;
        }

        @Override
        public void setSocketTimeout(int socketTimeout) {
            this.socketTimeout = socketTimeout;
        }

    }
}
