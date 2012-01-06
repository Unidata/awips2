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
import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

public class TestWsId {
    private Random rnd = new Random();

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
        int pid = rnd.nextInt();
        long key = System.currentTimeMillis();

        WsId wsId1 = new WsId(addr, user, program, pid, key);

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
        int pid = rnd.nextInt();
        WsId wsId = new WsId(addr, user, program, pid);
        WsId wsId2 = new WsId(null, user, program, pid);
        System.out.println(wsId2.getNetworkId());

        Assert.assertEquals(addr, wsId.getNetworkId());
        Assert.assertEquals(user, wsId.getUserName());
        Assert.assertEquals(program, wsId.getProgName());
        Assert.assertEquals(pid, wsId.getPid());
        Assert.assertEquals(0, wsId.getLockKey());
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
        int pid = rnd.nextInt();
        long key = System.currentTimeMillis();

        WsId wsId = new WsId(addr, user, program, pid, key);

        Assert.assertEquals(addr, wsId.getNetworkId());
        Assert.assertEquals(user, wsId.getUserName());
        Assert.assertEquals(program, wsId.getProgName());
        Assert.assertEquals(pid, wsId.getPid());
        Assert.assertEquals(key, wsId.getLockKey());
    }

    @Test
    public void testEqualForLockComparison() {
        InetAddress addr1 = null;
        InetAddress addr2 = null;
        try {
            addr1 = InetAddress.getLocalHost();
            addr2 = InetAddress.getByAddress(new byte[] { 123, 45, 67, 89 });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = System.getProperty("user.name");
        String program = "TestWsId";
        int pid = rnd.nextInt();
        long key = System.currentTimeMillis();

        WsId wsId1 = new WsId(addr1, user, program, pid, key);
        WsId wsId2 = new WsId(addr2, "bogus", "bogus", rnd.nextInt(), key);

        Assert.assertFalse(wsId1.equals(wsId2));
        Assert.assertTrue(wsId1.equalForLockComparison(wsId2));
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
        int pid = 987654321;
        long key = 1234567890l;

        WsId wsId1 = new WsId(addr, user, program, pid, key);
        String s = wsId1.toString();
        Assert.assertEquals("1497574779:user:TestWsId:987654321:1234567890", s);
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
        int pid = 987654321;
        long key = 1234567890l;

        WsId wsId1 = new WsId(addr, user, program, pid, key);
        String s = wsId1.toPrettyString();
        Assert.assertEquals("user@123.45.67.89:TestWsId:987654321:1234567890",
                s);
    }

    @Test
    public void testEqualsObject() {
        InetAddress addr1 = null;
        InetAddress addr2 = null;
        try {
            addr1 = InetAddress.getLocalHost();
            addr2 = InetAddress.getByAddress(new byte[] { 123, 45, 67, 89 });
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        String user = System.getProperty("user.name");
        String program = "TestWsId";
        int pid = rnd.nextInt();
        long key = System.currentTimeMillis();

        WsId wsId1 = new WsId(addr1, user, program, pid, key);
        WsId wsId2 = new WsId(addr1, user, program, pid, key);
        WsId wsId3 = new WsId(addr2, user, program, pid, key);
        WsId wsId4 = new WsId(addr1, "bogus", program, pid, key);
        WsId wsId5 = new WsId(addr1, user, "bogus", pid, key);
        WsId wsId6 = new WsId(addr1, user, program, rnd.nextInt(), key);
        WsId wsId7 = new WsId(addr1, "bogus", program, pid,
                System.currentTimeMillis());

        Assert.assertTrue(wsId1.equals(wsId1));
        Assert.assertTrue(wsId1.equals(wsId2));
        Assert.assertTrue(wsId2.equals(wsId1));
        Assert.assertFalse(wsId1.equals(wsId3));
        Assert.assertFalse(wsId1.equals(wsId4));
        Assert.assertFalse(wsId1.equals(wsId5));
        Assert.assertFalse(wsId1.equals(wsId6));
        Assert.assertFalse(wsId1.equals(wsId7));
    }

}
