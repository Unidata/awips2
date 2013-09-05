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
package com.raytheon.viz.gfe.ui.runtimeui;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.FakeDataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.internal.MockParmManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.Parm.CreateFromScratchMode;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2008            wdougherty   Initial creation
 * Sep 05, 2013  #2307     dgilling     Code cleanup
 * </pre>
 * 
 * @author wdougherty
 * @version 1.0
 */

@Ignore
// FIXME: This test case is currently broken because of a NullPOinterException
// in AbstractSpatialDisplayManager
// Activator.getDefault() is returning null at the moment.
public class TestSmartScript {

    private static final String SCRIPT_FILE_PATH = new File(
            "./python/gfe/TestSmartScript.py").getPath();

    private PythonScript testScript = null;

    private DataManager dataMgr;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        PathManagerFactoryTest.initLocalization();
    }

    @Before
    public void setUp() throws Exception {
        FakeDataManager fakeDataMgr = new FakeDataManager();
        IParmManager pm = new MockParmManager(fakeDataMgr);
        fakeDataMgr.setParmManager(pm);
        dataMgr = fakeDataMgr;

        String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getUtilitiesIncludePath());
        try {
            testScript = new PythonScript(SCRIPT_FILE_PATH, includePath, this
                    .getClass().getClassLoader());
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @After
    public void tearDown() throws Exception {
        if (testScript != null) {
            testScript.dispose();
        }
    }

    @Test
    public void testSortUglyStr() throws Exception {
        Map<String, Object> argmap = new HashMap<String, Object>(2, 1.0f);
        argmap.put("uglyStr", "I^AM^THE^CAPTAIN^OF^PINAFORE");
        argmap.put("dataMgr", dataMgr);
        String outstr = null;
        try {
            outstr = (String) testScript.execute("testSortUglyStr", null,
                    argmap);
        } catch (JepException e) {
            throw new Exception(e);
        }
        assertEquals("AM^CAPTAIN^I^OF^PINAFORE^THE", outstr);
    }

    @Test
    public void testGetIndex() throws Exception {
        Map<String, Object> argmap = new HashMap<String, Object>(3, 1.0f);
        argmap.put("dataMgr", dataMgr);

        String key1 = "Twas^Brillig^and^the^slithy^toves";
        String key2 = "Did^gyre^and^gimbol^in^the^wabe";
        String key3 = "Twas^Brillig^and";
        String key4 = "and^slithy^the^toves^Brillig^Twas";
        List<String> keyList = Arrays.asList(key1, key2, key3, key4);
        argmap.put("keyList", keyList);

        for (int i = 0; i < keyList.size(); i++) {
            Integer outInt = null;
            argmap.put("uglyStr", keyList.get(i));
            try {
                outInt = (Integer) testScript.execute("testGetIndex", null,
                        argmap);
                assertEquals("key" + (i + 1), i, outInt.intValue());
            } catch (JepException e) {
                throw new Exception(e);
            }
        }
    }

    @Test
    public void testGetGridShape() throws Exception {
        try {
            Map<String, Object> argmap = new HashMap<String, Object>(1, 1.0f);
            argmap.put("dataMgr", dataMgr);
            Boolean testResult = (Boolean) testScript.execute(
                    "testGetGridShape", null, argmap);
            assertTrue("Tuples should match", testResult);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void testGetGridInfo() throws Exception {
        Map<String, Object> argmap = new HashMap<String, Object>(5, 1.0f);
        argmap.put("dataMgr", dataMgr);
        argmap.put("model", "Fcst");
        argmap.put("element", "Hazards");
        argmap.put("level", "SFC");
        Parm parm = null;
        try {
            parm = (Parm) testScript.execute("getParm", null, argmap);
        } catch (JepException e) {
            throw new Exception(e);
        }
        assertNotNull("Retrieved Hazards_SFC parm.", parm);

        TimeRange tr = new TimeRange(new Date(), 1000L);
        boolean created = parm.createFromScratchTR(tr,
                CreateFromScratchMode.DEFAULT, 1200, 5);
        assertTrue("createFromScratchTR", created);

        parm.insertNewGrid(new TimeRange[] { tr },
                CreateFromScratchMode.DEFAULT);

        System.out
                .println("Class of parm = " + parm.getClass().getSimpleName());

        tr = TimeRange.allTimes();
        argmap.put("timeRange", tr);
        try {
            Boolean passedTest = (Boolean) testScript.execute(
                    "testGetGridInfo", null, argmap);
            assertTrue("Tests in Python passed", passedTest);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }
}
