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
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;

import org.junit.Test;

import com.raytheon.dods.DodsOriginalHttpConnectStrategy;
import com.raytheon.uf.common.util.TestUtil;

import dods.dap.parser.ParseException;

/**
 * Test {@link DConnect}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 09, 2012 0634       djohnson     Initial creation
 * Feb 05, 2013 1543       djohnson     Moved to tests project, add tests for reading dods files.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DConnectTest {

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
        new DConnect("file://", null, null, true, mockHttpStrategy);

        assertEquals(connectionTimeout, mockHttpStrategy.connectionTimeout);
        assertEquals(socketTimeout, mockHttpStrategy.socketTimeout);
    }

    @Test
    public void ableToReadCompressedFormat() throws MalformedURLException,
            DDSException, IOException, ParseException, DODSException {
        DConnect dconnect = new DConnect(new ByteArrayInputStream(
                TestUtil.readResource(DConnectTest.class,
                        "/datadelivery/opendap/compressed_rap_dataset.dods")));
        final DataDDS data = dconnect.getData(null);

        assertNotNull(data);
    }

    @Test
    public void ableToReadUncompressedFormat() throws MalformedURLException,
            DDSException, IOException, ParseException, DODSException {
        DConnect dconnect = new DConnect(new ByteArrayInputStream(
                TestUtil.readResource(DConnectTest.class,
                        "/datadelivery/opendap/uncompressed_rap_dataset.dods")));
        final DataDDS data = dconnect.getData(null);

        assertNotNull(data);
    }
}
