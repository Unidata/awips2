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
package com.raytheon.edex.plugin.gfe.watch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.edex.plugin.gfe.watch.WCLWatchSrv;
import com.raytheon.edex.plugin.gfe.watch.WatchProductUtil;
import com.raytheon.edex.plugin.gfe.watch.WclInfo;

public class TestWCLWatchSrv {

    private WclInfo wclInfoA;

    private WclInfo wclInfoB;

    private List<String> linesA;

    private List<String> linesB;

    private WCLWatchSrv wclWatchSrv;

    @Before
    public void setUp() throws Exception {
        linesA = new ArrayList<String>();
        linesA.add("NEB002-004-005-008-");
        linesA.add("KAN101-103-105-020315-");
        linesA.add("");
        linesA.add("Don't panic, this is a ....TORNADO WATCH");
        linesA.add("ATTN...WFO...ICT...MFL...OAX");
        wclInfoA = new WclInfo(0L, "IAMTHECAPTAIN", linesA, Boolean.TRUE);
        linesB = new ArrayList<String>();
        wclInfoB = new WclInfo(0L, "OFTHEPINAFORE", linesB, Boolean.TRUE);
        wclWatchSrv = new WCLWatchSrv();
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testAttnWFOs() {
        Set<String> wfosExpected = new HashSet<String>();
        wfosExpected.add("OAX");
        wfosExpected.add("MFL");
        wfosExpected.add("ICT");
        Collection<String> wfos = WatchProductUtil.findAttnWFOs(linesA);
        assertEquals("LinesA", wfosExpected, wfos);

        wfosExpected.clear();
        wfos = WatchProductUtil.findAttnWFOs(linesB);
        assertEquals("LinesB", wfosExpected, wfos);
    }

    @Test
    public void testHandleWclWatch() {
        // For testing the master method, we need to override the methods that
        // depend on CAVE and GFE configuration and localization.
        wclWatchSrv = new WCLWatchSrv() {
            @Override
            protected Set<String> getSiteIDs() {
                Set<String> rtnSet = new HashSet<String>();
                return rtnSet;
            }

            @Override
            protected File getWclDir(String siteID) {
                String home = System.getenv("HOME");
                File fakeDir = new File(home);
                return fakeDir;
            }
        };
    }

    /**
     * This test just supplies "nice" data and verifies that the method under
     * test handles it properly. In our development environment, $HOME is mapped
     * to a network drive, so the /tmp/ to $HOME change exercises the
     * copy-and-delete code.
     * 
     * TODO: add better tests: pass nulls, dirs w/o write permission, etc.
     * 
     * @throws Exception
     *             when thrown by file objects
     */
    @Test
    public void testMakePermanent() throws Exception {
        String home = System.getenv("HOME");
        String completePIL = "IAMTHECAPTAIN";
        File expectedFile = new File(home, completePIL);
        File temp = null;
        // makePermanent() calls getWclDir() to figure out where to put
        // the permanent file. Use an anonymous subclass to bypass CAVE
        // localization.
        wclWatchSrv = new WCLWatchSrv() {
            @Override
            protected File getWclDir(String siteID) {
                String home = System.getenv("HOME");
                return new File(home);
            }
        };
        try {
            temp = File.createTempFile("xxx", null, null);
            PrintWriter pw = new PrintWriter(temp);
            pw.println("Testing");
            pw.close();
            Collection<String> dummy = Collections.emptySet();
            wclWatchSrv.makePermanent(temp, completePIL, dummy);
            assertTrue("expetedFile exists", expectedFile.exists());
            assertTrue("expectedFile isFile", expectedFile.isFile());
            assertFalse("temp exists", temp.exists());
            expectedFile.delete();
        } finally {
            if (temp != null && temp.exists()) {
                temp.delete();
            }
            if (expectedFile.exists()) {
                expectedFile.delete();
            }
        }
    }

    @Test
    public void testCreateTempWclFile() throws Exception {
        File wclFile = wclWatchSrv
                .createTempWclFile("captain=\"Rafe\"\nfirst_officer='Buttercup'\n");
        assertNotNull("wclFile", wclFile);
        assertTrue("wclFile exists", wclFile.exists());
        assertTrue("wclFile isFile", wclFile.isFile());
        try {
            assertEquals("wclFile length", 42, wclFile.length());
            File expectedDir = new File(File.separator + "tmp");
            assertEquals("wclFile directory", expectedDir,
                    wclFile.getParentFile());
        } finally {
            wclFile.delete();
        }
    }

    @Test
    public void testMakeWclStr() {
        List<String> finalUGCList = new ArrayList<String>();
        finalUGCList.add("ICT");
        finalUGCList.add("MFL");
        finalUGCList.add("OAX");
        Date expireTime = new Date(12345678999L);
        Date issueTime = new Date(35791113001L);
        String watchType = "SV.A";
        String result = wclWatchSrv.makeWclStr(finalUGCList, expireTime,
                issueTime, watchType);
        String expectedResult = "watchtype = \"SV.A\"\n"
                + "finalUGCList = [\"ICT\",\"MFL\",\"OAX\"]\n"
                + "expTime =12345678\n" + "issueTime =35791113\n";
        assertEquals("result", expectedResult, result);

        expireTime = null;
        result = wclWatchSrv.makeWclStr(finalUGCList, expireTime, issueTime,
                watchType);
        expectedResult = "watchtype = \"SV.A\"\n"
                + "finalUGCList = [\"ICT\",\"MFL\",\"OAX\"]\n"
                + "expTime =None\n" + "issueTime =35791113\n";
        assertEquals("result-no expTime", expectedResult, result);

        expireTime = new Date(12345678999L);
        issueTime = null;
        result = wclWatchSrv.makeWclStr(finalUGCList, expireTime, issueTime,
                watchType);
        expectedResult = "watchtype = \"SV.A\"\n"
                + "finalUGCList = [\"ICT\",\"MFL\",\"OAX\"]\n"
                + "expTime =12345678\n" + "issueTime =None\n";
        assertEquals("result-no issueTime", expectedResult, result);

        issueTime = new Date(35791113001L);
        finalUGCList.clear();
        result = wclWatchSrv.makeWclStr(finalUGCList, expireTime, issueTime,
                watchType);
        expectedResult = "watchtype = \"SV.A\"\n" + "finalUGCList = []\n"
                + "expTime =12345678\n" + "issueTime =35791113\n";
        assertEquals("result-empty UGC list", expectedResult, result);
    }

    @Test
    public void testGetWatchType() {
        String watchType;
        watchType = wclWatchSrv.getWatchType(wclInfoA);
        assertEquals("watch A type", "TO.A", watchType);

        watchType = wclWatchSrv.getWatchType(wclInfoB);
        assertEquals("watch B type", "SV.A", watchType);
    }

    @Test
    public void testGetExpireTime() {
        Date result;
        // Override currentTime() to break dependence on SimulatedTime
        wclWatchSrv = new WCLWatchSrv() {
            @Override
            protected Date currentTime() {
                return new Date();
            }
        };
        result = wclWatchSrv.getExpireTime(wclInfoA);
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        cal.set(Calendar.DAY_OF_MONTH, 2);
        cal.set(Calendar.HOUR_OF_DAY, 3);
        cal.set(Calendar.MINUTE, 15);
        cal.set(Calendar.MILLISECOND, 0);
        Date expectedResult;
        expectedResult = cal.getTime();
        assertEquals("getExpireTime A", expectedResult, result);

        result = wclWatchSrv.getExpireTime(wclInfoB);
        assertNull("getExpireTime B", result);
    }

    @Test
    public void testGetUGCs() {
        List<String> UGCs;
        UGCs = wclWatchSrv.getUGCs(wclInfoA);
        List<String> expectedUGCs = new ArrayList<String>();
        expectedUGCs.add("NEB002");
        expectedUGCs.add("NEB004");
        expectedUGCs.add("NEB005");
        expectedUGCs.add("NEB008");
        expectedUGCs.add("KAN101");
        expectedUGCs.add("KAN103");
        expectedUGCs.add("KAN105");
        assertEquals("UGCs A", expectedUGCs, UGCs);

        UGCs = wclWatchSrv.getUGCs(wclInfoB);
        expectedUGCs.clear();
        assertEquals("UGCs B", expectedUGCs, UGCs);
    }
}
