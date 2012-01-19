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

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import jep.JepException;
import junit.framework.TestCase;

import org.junit.Test;

import com.raytheon.uf.common.python.PythonMapScript;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.FakeDataManager;
import com.raytheon.viz.gfe.core.internal.AbstractParmManager;
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
 * Jul 24, 2008            wdougherty     Initial creation
 * </pre>
 * 
 * @author wdougherty
 * @version 1.0
 */

public class TestSmartScript extends TestCase {

    protected static final String testScriptName = "ROOT/build/static/common/cave/etc/gfe/userPython/tests/TestSmartScript.py";

    protected static final String smartScriptPath = "ROOT/build/static/common/cave/etc/gfe/userPython/utilities"
            + ":ROOT/build"
            + ":ROOT/AWIPSEdex/opt/utility/common_static/base/python/gfe"
            + ":ROOT/AWIPSEdex/opt/utility/common_static/base/python"
            + ":ROOT/AWIPSEdex/extensions/plugin-gfe/src";

    /**
     * A PythonScript. All PythonScript's constructors are protected, so we have
     * to create this subclass in order to obtain an instance. While we're at
     * it, create a modified execute() method and a helper method for setting
     * groups of script variables.
     * 
     */

    PythonMapScript testScript;

    Map<String, Object> argmap;

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    @Override
    public void setUp() throws Exception {
        try {
            // Create a DataManager to pass to the SmartScript constructor.
            DataManager dataManager = null;
            Activator activator = Activator.getDefault();
            if (activator == null) {
                FakeDataManager fakeDataManager = new FakeDataManager();
                AbstractParmManager pm = new MockParmManager(fakeDataManager);
                fakeDataManager.setParmManager(pm);
                dataManager = fakeDataManager;
            } else {
                dataManager = DataManager.getInstance(null);
            }

            // Set up an interpreter that can run Python with Java classes.
            // Hopefully, using user.home will make this portable to any
            // developer.
            String root = System.getProperty("user.home") + File.separator
                    + "workspace";
            ClassLoader classLoader = dataManager.getClass().getClassLoader();
            testScript = new PythonMapScript(testScriptName.replaceAll("ROOT",
                    root), smartScriptPath.replaceAll("ROOT", root),
                    classLoader);

            // Put the data manager in the interpreter
            Map<String, Object> setupMap = new HashMap<String, Object>();
            setupMap.put("dataManager", dataManager);
            testScript.setArgs(setupMap);

            // Create a SmartScript instance for use by the tests.
            boolean evalResult;
            evalResult = testScript
                    .eval("smartScript = SmartScript.SmartScript(dataManager)");

            // If eval somehow failed without throwing a JepException, fail.
            assertTrue(evalResult);

            // Create an argmap to pass parameters in.
            argmap = new HashMap<String, Object>();
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#tearDown()
     */
    @Override
    public void tearDown() {
        // Free any resources the previous interpreter used.
        testScript.dispose();
    }

    /**
     * Test the createGrid method of SmartScript.py when creating a discrete
     * grid.
     * 
     * NOTE: This test currently throws an exception because one of the objects
     * supplied by MockParmManager always has a grid type of "SCALAR", which
     * leads to a ClassCastException in the grid cycler when SmartScript passes
     * a byte grid and keys to the grid cycler. If the code in the grid cycler
     * is hacked to force the grid type to "DISCRETE" for the test, it completes
     * successfully.
     */
    @Test
    public void testCreateGridDiscrete() throws Exception {
        argmap.put("model", "Fcst");
        argmap.put("element", "hazXXX1234");
        argmap.put("elementType", "DISCRETE");
        argmap.put("timeRange", new TimeRange(new Date(), 10 * 3600 * 1000L));
        argmap.put("descriptiveName", "Discrete Test Grid");
        argmap.put("::timeConstraints", "tcTuple");
        argmap.put("units", "");
        argmap.put("::discreteKeys", "dkList");
        argmap.put("discreteOverlap", Boolean.FALSE);
        argmap.put("discreteAuxDataLength", Integer.valueOf(4));
        argmap.put("defaultColorTable", "YesNo");
        argmap.put("::numericGrid", "discreteGrid");
        Object obj = null;
        try {
            if (!testScript.eval("tcTuple = (1000, 1200, 600)")) {
                throw new Exception("eval(\"tcTuple... failed.");
            }
            if (!testScript.eval("dkList = ['One', ('Singular', 'Sensation')]")) {
                throw new Exception("eval(\"dklist = ... failed.");
            }
            obj = testScript.execute("createGrid", "smartScript", argmap);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    /**
     * @throws Exception
     */
    @Test
    public void testSortUglyStr() throws Exception {
        argmap.put("uglyStr", "I^AM^THE^CAPTAIN^OF^PINAFORE");
        String outstr = null;
        try {
            outstr = (String) testScript.execute("sortUglyStr", "smartScript",
                    argmap);
        } catch (JepException e) {
            throw new Exception(e);
        }
        assertEquals("AM^CAPTAIN^I^OF^PINAFORE^THE", outstr);
    }

    @Test
    public void testGetIndex() throws Exception {
        String key1 = "Twas^Brillig^and^the^slithy^toves";
        String key2 = "Did^gyre^and^gimbol^in^the^wabe";
        String key3 = "Twas^Brillig^and";
        String key4 = "and^slithy^the^toves^Brillig^Twas";

        Integer outInt = null;
        try {
            testScript.eval("keyList = []");
            argmap.put("::keys", "keyList");
            argmap.put("uglyStr", key1);
            outInt = (Integer) testScript.execute("getIndex", "smartScript",
                    argmap);
            assertEquals("key1", 0, outInt.intValue());

            argmap.put("uglyStr", key2);
            outInt = (Integer) testScript.execute("getIndex", "smartScript",
                    argmap);
            assertEquals("key2", 1, outInt.intValue());

            argmap.put("uglyStr", key3);
            outInt = (Integer) testScript.execute("getIndex", "smartScript",
                    argmap);
            assertEquals("key3", 2, outInt.intValue());

            argmap.put("uglyStr", key4);
            outInt = (Integer) testScript.execute("getIndex", "smartScript",
                    argmap);
            assertEquals("key4", 0, outInt.intValue());

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void testGetGridShape() throws Exception {
        try {
            Boolean testResult = (Boolean) testScript.execute(
                    "testGetGridShape", null, argmap);
            assertTrue("Tuples should match", testResult);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void testGetGridInfo() throws Exception {
        argmap.put("model", "Fcst");
        argmap.put("element", "Hazards");
        argmap.put("level", "SFC");

        Parm parm = null;
        try {
            parm = (Parm) testScript.execute("getParm", "smartScript", argmap);
        } catch (JepException e1) {
            throw new Exception(e1);
        }

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
