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
package com.raytheon.uf.edex.auth;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.localization.msgs.GetServersRequest;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.auth.req.ServerPrivilegedRequestHandler.ServerPrivilegedRequest;

/**
 * Test {@link RemoteServerRequestRouter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class RemoteServerRequestRouterTest {
    
    private static final IBandwidthRequest PRIVILEGED_REQUEST = new IBandwidthRequest();

    private static final GetServersRequest UNPRIVILEGED_REQUEST = new GetServersRequest();

    private static final String HTTP_ADDRESS = "http://someHttpAddress";

    private byte[] serializedData;

    private final RemoteServerRequestRouter router = new RemoteServerRequestRouter(
            HTTP_ADDRESS) {
        /**
         * {@inheritDoc}
         */
        @Override
        byte[] sendSerializedRequest(byte[] message)
                throws CommunicationException, Exception {
            serializedData = message;
            return null;
        }
    };

    @Test
    public void testWrapsAbstractPrivilegedRequestInServerPrivilegedRequest()
            throws Exception {
        router.route(PRIVILEGED_REQUEST);

        assertNotNull("The serialized data should not be null", serializedData);

        Object restored = SerializationUtil.transformFromThrift(Object.class,
                serializedData);

        assertEquals("Incorrect serialized request instance",
                ServerPrivilegedRequest.class, restored.getClass());
    }

    @Test
    public void testDoesNotWrapUnprivilegedRequestInServerPrivilegedRequest()
            throws Exception {
        router.route(UNPRIVILEGED_REQUEST);

        assertNotNull("The serialized data should not be null", serializedData);

        Object restored = SerializationUtil.transformFromThrift(Object.class,
                serializedData);

        assertEquals("Incorrect serialized request instance",
                GetServersRequest.class, restored.getClass());
    }
}
