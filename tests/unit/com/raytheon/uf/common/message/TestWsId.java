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

import java.lang.management.ManagementFactory;
import java.net.InetAddress;
import java.net.UnknownHostException;

import org.junit.Assert;
import org.junit.Test;

public class TestWsId {
    private int pid = Integer.parseInt(ManagementFactory.getRuntimeMXBean()
            .getName().split("@")[0]);

    private long threadId = Thread.currentThread().getId();

    @Test
    public void testWsIdString() {
        InetAddress addr = null;
        try {
            addr = InetAddress.getLocalHost();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = System.getProperty("user.name");
        String program = "TestWsId";

        WsId wsId1 = new WsId(addr, user, program);

        WsId wsId2 = new WsId(wsId1.toString());
        Assert.assertEquals(wsId1, wsId2);
    }

    @Test
    public void testWsIdInetAddressStringStringInt() {
        InetAddress addr = null;
        try {
            addr = InetAddress.getLocalHost();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = System.getProperty("user.name");
        String program = "TestWsId";
        WsId wsId = new WsId(addr, user, program);
        WsId wsId2 = new WsId(null, user, program);
        System.out.println(wsId2.getNetworkId());

        Assert.assertEquals(addr, wsId.getNetworkId());
        Assert.assertEquals(user, wsId.getUserName());
        Assert.assertEquals(program, wsId.getProgName());
        Assert.assertEquals(pid, wsId.getPid());
        Assert.assertEquals(threadId, wsId.getThreadId());
    }

    @Test
    public void testWsIdInetAddressStringStringIntLong() {
        InetAddress addr = null;
        try {
            addr = InetAddress.getLocalHost();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = System.getProperty("user.name");
        String program = "TestWsId";

        WsId wsId = new WsId(addr, user, program);

        Assert.assertEquals(addr, wsId.getNetworkId());
        Assert.assertEquals(user, wsId.getUserName());
        Assert.assertEquals(program, wsId.getProgName());
        Assert.assertEquals(pid, wsId.getPid());
        Assert.assertEquals(threadId, wsId.getThreadId());
    }

    @Test
    public void testToString() {
        InetAddress addr = null;
        try {
            addr = InetAddress.getByAddress(new byte[] { 123, 45, 67, 89 });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = "user";
        String program = "TestWsId";

        WsId wsId1 = new WsId(addr, user, program);
        String s = wsId1.toString();
        String expected = "1497574779:user:TestWsId:" + this.pid + ":"
                + this.threadId;
        Assert.assertEquals(expected, s);
    }

    @Test
    public void testToPrettyString() {
        InetAddress addr = null;
        try {
            addr = InetAddress.getByAddress(new byte[] { 123, 45, 67, 89 });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = "user";
        String program = "TestWsId";

        WsId wsId1 = new WsId(addr, user, program);
        String s = wsId1.toPrettyString();
        String expected = "user@123.45.67.89:TestWsId:" + this.pid + ":"
                + this.threadId;
        Assert.assertEquals(expected, s);
    }

    @Test
    public void testEqualsObject() {
        try {
            final InetAddress addr1 = InetAddress.getLocalHost();
            final InetAddress addr2 = InetAddress.getByAddress(new byte[] {
                    123, 45, 67, 89 });
            final String user = System.getProperty("user.name");
            final String program = "TestWsId";
            WsId wsId1 = new WsId(addr1, user, program);
            WsId wsId2 = new WsId(addr1, user, program);
            WsId wsId3 = new WsId(addr2, user, program);
            WsId wsId4 = new WsId(addr1, "bogus", program);
            WsId wsId5 = new WsId(addr1, user, "bogus");
            WsId wsId6 = new WsId(addr1, "bogus", program);

            Assert.assertTrue(wsId1.equals(wsId1));
            Assert.assertTrue(wsId1.equals(wsId2));
            Assert.assertTrue(wsId2.equals(wsId1));
            Assert.assertFalse(wsId1.equals(wsId3));
            Assert.assertFalse(wsId1.equals(wsId4));
            Assert.assertFalse(wsId1.equals(wsId5));
            Assert.assertFalse(wsId1.equals(wsId6));

            final WsId[] wsId7 = new WsId[1];
            Thread thread = new Thread(new Runnable() {
                @Override
                public void run() {
                    wsId7[0] = new WsId(addr1, user, program);
                }
            });

            thread.start();
            thread.join();
            Assert.assertFalse(wsId1.equals(wsId7[0]));

        } catch (UnknownHostException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
