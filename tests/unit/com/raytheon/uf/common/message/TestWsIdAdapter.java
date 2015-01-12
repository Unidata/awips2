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
package com.raytheon.uf.common.message;

import java.net.InetAddress;
import java.net.UnknownHostException;
//import java.util.Random;

import org.junit.Assert;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationMetadata;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TestWsIdAdapter test
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2013            bclement    Initial creation
 * Aug 26, 2014 3365       ccody       Separate Data Delivery tests out of AWIPS 2 baseline.
 *                                     DynamicSerializationManager API Has changed since this test was created.
 *                                     Test functionality that is no longer supported by the API (i.e. will not compile)
 *                                     has been "deactivated" (commented out).
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TestWsIdAdapter {
    //private Random rnd = new Random();

    @DynamicSerialize
    public static class Test {
        @DynamicSerializeElement
        private WsId wsId;

        /**
         * @return the wsId
         */
        public WsId getWsId() {
            return wsId;
        }

        /**
         * @param wsId
         *            the wsId to set
         */
        public void setWsId(WsId wsId) {
            this.wsId = wsId;
        }

    }

    @org.junit.Test
    public void testBasicFunctionality() {
        InetAddress addr = null;
        try {
            addr = InetAddress.getLocalHost();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = System.getProperty("user.name");
        String program = "TestWsId";

        WsId wsId = new WsId(addr, user, program);

        Test inTest = new Test();
        inTest.setWsId(wsId);

        DynamicSerializationManager dmgr = DynamicSerializationManager
                .getManager(SerializationType.Thrift);

        SerializationMetadata sm1 = DynamicSerializationManager.getSerializationMetadata(inTest.getClass());
        //REMOVED DynamicSerializationManager.inspect(inTest.getClass());
        SerializationMetadata sm2 = DynamicSerializationManager.getSerializationMetadata(wsId.getClass());
        //REMOVED DynamicSerializationManager.inspect(wsId.getClass());
        byte[] bdata = null;
        try {
            bdata = dmgr.serialize(inTest);
            System.out.println("Serialized data of size: " + bdata.length);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Test outTest = null;

        try {
            outTest = (Test) dmgr.deserialize(bdata);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        // Make sure somehow we didn't get the same exact objects
        Assert.assertNotSame(inTest, outTest);

        // Verify results
        Assert.assertEquals(inTest.getWsId(), outTest.getWsId());

    }
}
