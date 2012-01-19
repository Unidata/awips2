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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
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

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2008            wdougherty     Initial creation
 * </pre>
 * 
 * @author wdougherty
 * @version 1.0
 */
public class TestHazardUtils extends TestCase {

    public static final String JTIMERANGE_PAT = "\\(\\w{3} \\w{3} \\d{2} \\d{2}\\:\\d{2}\\:\\d{2} GMT\\+\\d{2}\\:\\d{2} \\d{4}\\, \\w{3} \\w{3} \\d{2} \\d{2}\\:\\d{2}\\:\\d{2} GMT\\+\\d{2}\\:\\d{2} \\d{4}\\)";

    public static final String CREATE_PAT_A = "createGrid\\(\\): defaultColorTable='YesNo' discreteAuxDataLength=4 discreteKeys=\\[\\('<None>', '<None>'\\), \\('AF.Y', 'ASHFALL ADVISORY'\\)\\] discreteOverlap=0 element='hazAFY' elementType='DISCRETE' model='Fcst' numericGrid=\\(<type 'numpy.ndarray'>\\(145, 145\\), \\['<None>'\\]\\) rateParm=0 timeRange="
            + JTIMERANGE_PAT;

    public static final String CREATE_PAT_B = "createGrid\\(\\): discreteAuxDataLength=4 discreteOverlap=1 element='Hazards' elementType='DISCRETE' model='Fcst' numericGrid=\\(<type 'numpy.ndarray'>\\(145, 145\\), \\['<None>'\\]\\) rateParm=0 timeRange="
            + JTIMERANGE_PAT;

    public static final String CREATE_STR = "createGrid(): discreteAuxDataLength=4 discreteKeys=['TWAS^BRILLIG^AND^THE', ('SLITHY', 'NONSENSE WORD')] discreteOverlap=1 element='Hazards' elementType='DISCRETE' model='Fcst' numericGrid=(<type 'numpy.ndarray'>(145, 145), ['<None>']) rateParm=0 timeRange=(Thu Jan 01 00:00:00 GMT+00:00 1970, Sat Jan 03 13:27:01 GMT+00:00 1970)";

    public static final String CREATE_TEMP_STR = "createGrid(): defaultColorTable='YesNo' discreteAuxDataLength=4 discreteKeys=[('<None>', '<None>'), ('AF.Y', 'ASHFALL ADVISORY')] discreteOverlap=0 element='hazAF.Y' elementType='DISCRETE' model='Fcst' numericGrid=(<type 'numpy.ndarray'>(145, 145), ['<None>']) rateParm=0 timeRange=(Thu Jan 01 00:00:00 GMT+00:00 1970, Sat Jan 03 13:27:01 GMT+00:00 1970)";

    public static final String CREATE_CONS_PAT = "createGrid\\(\\): discreteAuxDataLength=4 discreteOverlap=1 element='hazAFY' elementType='DISCRETE' model='Fcst' numericGrid=\\(<type 'numpy.ndarray'>\\(145, 145\\), \\['AF.Y'\\]\\) rateParm=0 timeRange="
            + JTIMERANGE_PAT;

    public static final String CONSOL_PAT = "\\QcreateGrid(): discreteAuxDataLength=4 discreteOverlap=1 element='hazAFY' elementType='DISCRETE' model='Fcst' numericGrid=(<type 'numpy.ndarray'>(145, 145), ['AF.Y']) rateParm=0 timeRange=\\E"
            + JTIMERANGE_PAT
            + "\\QcreateGrid(): discreteAuxDataLength=4 discreteOverlap=1 element='hazHTY' elementType='DISCRETE' model='Fcst' numericGrid=(<type 'numpy.ndarray'>(145, 145), ['HT.Y']) rateParm=0 timeRange=\\E"
            + JTIMERANGE_PAT;

    public static final String ADDHAZ_PAT_2 = "createGrid\\(\\): defaultColorTable='YesNo' discreteAuxDataLength=4 discreteKeys=\\[\\('<None>', '<None>'\\), \\('AF\\.Y', 'ASHFALL ADVISORY'\\)\\] discreteOverlap=0 element='hazAFY1234' elementType='DISCRETE' model='Fcst' numericGrid=\\(<type 'numpy\\.ndarray'>\\(145, 145\\), \\['<None>'\\]\\) rateParm=0 timeRange="
            + JTIMERANGE_PAT
            + "parm\\(hazAFY1234\\):deleteTR\\(<PyJobject object at 0x\\p{XDigit}+>\\)\n";

    public static final String ADDHAZ_PAT = "createGrid\\(\\): discreteAuxDataLength=4 discreteOverlap=1 element='Hazards' elementType='DISCRETE' model='Fcst' numericGrid=\\(<type 'numpy\\.ndarray'>\\(145, 145\\), \\['<None>'\\]\\) rateParm=0 timeRange="
            + JTIMERANGE_PAT
            + "createGrid\\(\\): discreteAuxDataLength=4 discreteOverlap=1 element='Hazards' elementType='DISCRETE' model='Fcst' numericGrid=\\(<type 'numpy.ndarray'>\\(145, 145\\), \\['<None>', '<None>\\^EH\\.Y', 'AF\\.Y:1234\\^EH.Y'\\]\\) rateParm=0 timeRange="
            + JTIMERANGE_PAT;

    protected static final String hazardUtilsPath = "ROOT/build/static/common/cave/etc/gfe/userPython/utilities"
            + ":ROOT/AWIPSEdex/opt/utility/common_static/base/python/gfe"
            + ":ROOT/AWIPSEdex/opt/utility/common_static/base/python"
            + ":ROOT/build/static/common/cave/etc/gfe/userPython/textUtilities/regular"
            + ":ROOT/build/static/common/cave/etc/gfe/userPython/tests";

    public static final String JAVA_TR_STR = "[<PyJobject object at ";

    public static final String PYTHON_TR_PAIR_PAT = "\\[<TimeRange.TimeRange instance at 0x\\p{XDigit}+>\\, <TimeRange.TimeRange instance at 0x\\p{XDigit}+>\\]";

    public static final String PYTHON_TR_PAT = "\\[<TimeRange.TimeRange instance at 0x\\p{XDigit}+>\\]";

    public static final String PYTHON_TR_STR = "[<TimeRange.TimeRange instance at ";

    public static final String PYTHON_BARE_TR_PAT = "<TimeRange.TimeRange instance at 0x\\p{XDigit}+>";

    public static final String START_EDIT_PAT = "\\Q(<MockParm.MockParm object at 0x\\E\\p{XDigit}+\\Q>, [<MockGridData.MockGridData object at 0x\\E\\p{XDigit}+>\\]\\)";

    public static final String SEP_HAZ_PAT = "\\QcreateGrid(): defaultColorTable='YesNo' discreteAuxDataLength=4 discreteKeys=[('<None>', '<None>'), ('AF.Y', 'ASHFALL ADVISORY')] discreteOverlap=0 element='hazAFY' elementType='DISCRETE' model='Fcst' numericGrid=(<type 'numpy.ndarray'>(145, 145), ['<None>']) rateParm=0 timeRange=\\E"
            + JTIMERANGE_PAT
            + "\\Qparm(hazAFY):deleteTR(<PyJobject object at 0x\\E\\p{XDigit}+\\Q>)\ncreateGrid(): defaultColorTable='YesNo' discreteAuxDataLength=4 discreteKeys=[('<None>', '<None>'), ('EH.A', 'EXCESSIVE HEAT WATCH')] discreteOverlap=0 element='hazEHA' elementType='DISCRETE' model='Fcst' numericGrid=(<type 'numpy.ndarray'>(145, 145), ['<None>']) rateParm=0 timeRange=\\E"
            + JTIMERANGE_PAT
            + "\\Qparm(hazEHA):deleteTR(<PyJobject object at 0x\\E\\p{XDigit}+\\Q>)\ncreateGrid(): defaultColorTable='YesNo' discreteAuxDataLength=4 discreteKeys=[('<None>', '<None>'), ('EH.A', 'EXCESSIVE HEAT WATCH')] discreteOverlap=0 element='hazEHA' elementType='DISCRETE' model='Fcst' numericGrid=(<type 'numpy.ndarray'>(145, 145), ['<None>']) rateParm=0 timeRange=\\E"
            + JTIMERANGE_PAT
            + "\\Qparm(hazEHA):deleteTR(<PyJobject object at 0x\\E\\p{XDigit}+>\\)\n";

    protected static final String testScriptName = "ROOT/build/static/common/cave/etc/gfe/userPython/tests/TestHazardUtils.py";

    Map<String, Object> argmap;

    /**
     * A PythonScript. All PythonScript's constructors are protected, so we have
     * to create this subclass in order to obtain an instance. While we're at
     * it, create a modified execute() method and a helper method for setting
     * groups of script variables.
     * 
     */

    PythonMapScript testScript;

    /**
     * Helper method for passing Python expressions to testScript. If the
     * expression doesn't parse, the script will fail and throw an exception
     * that has the line with the problem as the message.
     * 
     * @param pythonExpr
     *            the Python expression to evaluate.
     * @throws Exception
     *             if eval() throws a JepException.
     */
    public void assertEval(String pythonExpr) throws JepException, Exception {
        assertTrue(pythonExpr, testScript.eval(pythonExpr));
    }

    /**
     * Version of assertMatches for when no additional message is required.
     * 
     * @param pattern
     *            Regexp pattern string to match
     * @param result
     *            String value to to match
     */
    public void assertMatches(String pattern, String result) {
        assertMatches("", pattern, result);
    }

    /**
     * Helper method, like assertEqual except that expected value is regexp
     * pattern instead of a string constant.
     * 
     * @param msg
     *            Message label for the test
     * @param pattern
     *            Regexp pattern string to match
     * @param result
     *            String value to to match
     */
    public void assertMatches(String msg, String pattern, String result) {
        if (!result.matches(pattern)) {
            StringBuilder sb = new StringBuilder();
            if ("" != msg) {
                sb.append(msg).append(": ");

            }
            fail(String.format("%sExpected match for '%s', result was '%s'", sb
                    .toString(), pattern, result));
        }
    }

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
                    root), hazardUtilsPath.replaceAll("ROOT", root),
                    classLoader);

            // Put the data manager in the interpreter
            Map<String, Object> setupMap = new HashMap<String, Object>();
            setupMap.put("dataManager", dataManager);
            testScript.setArgs(setupMap);

            // Create a HazardUtils instance for use by the tests.
            // Make it a MockHazardUtils instance so the only way we use
            // the data manager and parm manager is in the constructor.
            boolean evalResult;
            evalResult = testScript
                    .eval("hazardUtils = MockHazardUtils.MockHazardUtils(dataManager, None)");

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

    @Test
    public void test_addHazardDesc() throws Exception {
        try {
            assertEval("keys = ['asdf', 'qwer:uiop', 'AF.Y', 'EC.A:Denver']");
            argmap.put("::keys", "keys");
            String str = (String) testScript.execute("_addHazardDesc",
                    "hazardUtils", argmap);
            assertEquals(
                    "output",
                    "[('asdf', 'asdf'), ('qwer', 'qwer'), ('AF.Y', 'ASHFALL ADVISORY'), ('EC.A', 'EXTREME COLD WATCH')]",
                    str);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_combinedKey() throws Exception {
        String str = null;
        try {
            // Empty inputs should give empty output
            argmap.put("subKeys", "");
            argmap.put("newKey", "");
            str = (String) testScript.execute("_combinedKey", "hazardUtils",
                    argmap);
            assertEquals("empty", "", str);

            // New key w/ no '.' should just be appended
            argmap.put("subKeys", "I^AM^THE^CAPTAIN");
            argmap.put("newKey", "OF");
            str = (String) testScript.execute("_combinedKey", "hazardUtils",
                    argmap);
            assertEquals("Adding 'OF'", "I^AM^THE^CAPTAIN^OF", str);

            // New key starting with "TO." should just be appended
            argmap.put("subKeys", "I^TO.A^THE^CAPTAIN");
            argmap.put("newKey", "TO.W");
            str = (String) testScript.execute("_combinedKey", "hazardUtils",
                    argmap);
            assertEquals("Adding 'TO.W'", "I^TO.A^THE^CAPTAIN^TO.W", str);

            // New key starting with something else should keep 'Y' over 'A'.
            argmap.put("subKeys", "I^AM.A^THE^CAPTAIN");
            argmap.put("newKey", "AM.Y");
            str = (String) testScript.execute("_combinedKey", "hazardUtils",
                    argmap);
            assertEquals("Adding 'AM.Y'", "I^AM.Y^THE^CAPTAIN", str);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_getGaps() throws Exception {

        // An empty map so we don't have to clear and re-init argmap to call
        // no-arg getter methods in Python scripts.
        Map<String, Object> emptyMap = new HashMap<String, Object>();

        // A string for results from Python
        String str = null;

        // Confirm that when the hazards inventory is empty, we get the whole
        // time range of interest back.
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.HOUR_OF_DAY, -1);
        long oneHourAgo = cal.getTimeInMillis();
        cal.add(Calendar.HOUR_OF_DAY, 2);
        long oneHourFromNow = cal.getTimeInMillis();

        try {
            assertEval("tr0 = TR(" + oneHourAgo + ", " + oneHourFromNow + ")");
            assertEval("trList = [ tr0 ]");
            argmap.put("WEName", "Hazards");
            argmap.put("::trList", "trList");
            str = (String) testScript
                    .execute("_getGaps", "hazardUtils", argmap);
            assertMatches("tr0 should be in gap list", PYTHON_TR_PAT, str);
            // Confirming that the list has a single time range isn't enough.
            // We need to check the start and end times.
            assertEval("resultTRL = result"); // used knowledge of
            // PythonScript internals to
            // cheat a little here
            TimeRange result = (TimeRange) testScript.execute("javaTimeRange",
                    "resultTRL[0]", emptyMap);
            TimeRange tr0 = (TimeRange) testScript.execute("clone", "tr0",
                    emptyMap);
            assertEquals("Result should be same as tr0", tr0, result);
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Confirm that when the inventory contains a time that overlaps the
        // start, we get the rest of the timerange.
        cal.setTimeInMillis(oneHourAgo);
        cal.add(Calendar.MINUTE, -5);
        long backTime = cal.getTimeInMillis();
        cal.add(Calendar.MINUTE, 10);
        long frontTime = cal.getTimeInMillis();

        try {
            assertEval("lowRange = TR(" + backTime + ", " + frontTime + ")");
            assertEval("loSlice = MockSlice.MockSlice(lowRange)");
            assertEval("loData = MockGridData.MockGridData(loSlice)");
            assertEval("mockHazParm = MockParm.MockParm('Hazards')");
            assertEval("mockHazParm.gridInventory = [ loData ]");
            assertEval("hazardUtils.parms['Hazards'] = mockHazParm");
            str = (String) testScript
                    .execute("_getGaps", "hazardUtils", argmap);
            assertMatches("Overlap-start", PYTHON_TR_PAT, str);
            assertEval("resultTRL = result"); // cheat again
            TimeRange result = (TimeRange) testScript.execute("javaTimeRange",
                    "resultTRL[0]", emptyMap);
            // expected time is truncated to nearest second
            TimeRange expected = new TimeRange((frontTime + 1) / 1000 * 1000,
                    oneHourFromNow / 1000 * 1000);
            assertEquals("", expected, result);
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Confirm that when the inventory contains a time that overlaps the
        // end, we get the rest of the timerange.
        cal.setTimeInMillis(oneHourFromNow);
        cal.add(Calendar.MINUTE, -5);
        long foreTime = cal.getTimeInMillis();
        cal.add(Calendar.MINUTE, 10);
        long farTime = cal.getTimeInMillis();

        try {
            assertEval("hiRange = TR(" + foreTime + ", " + farTime + ")");
            assertEval("hiSlice = MockSlice.MockSlice(hiRange)");
            assertEval("hiData = MockGridData.MockGridData(hiSlice)");
            assertEval("mockHazParm.gridInventory = [ hiData ]");
            str = (String) testScript
                    .execute("_getGaps", "hazardUtils", argmap);
            assertMatches("Overlap-end", PYTHON_TR_PAT, str);
            assertEval("resultTRL = result"); // cheat again
            TimeRange result = (TimeRange) testScript.execute("javaTimeRange",
                    "resultTRL[0]", emptyMap);
            // expected time is truncated to nearest second
            TimeRange expected = new TimeRange(oneHourAgo / 1000 * 1000,
                    (foreTime - 1) / 1000 * 1000);
            assertEquals("", expected, result);
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Combine the low-overlap and hi-overlap tests and put a
        // short timerange in the middle.
        cal.setTimeInMillis(oneHourAgo);
        cal.add(Calendar.MINUTE, 55);
        long nowLessFiveMin = cal.getTimeInMillis();
        cal.add(Calendar.MINUTE, 10);
        long nowPlusFiveMin = cal.getTimeInMillis();

        try {
            assertEval("midRange = TR(" + nowLessFiveMin + ", "
                    + nowPlusFiveMin + ")");
            assertEval("midSlice = MockSlice.MockSlice(midRange)");
            assertEval("midData = MockGridData.MockGridData(midSlice)");
            assertEval("mockHazParm.gridInventory = [ loData, midData, hiData]");
            str = (String) testScript
                    .execute("_getGaps", "hazardUtils", argmap);
            // Confirm exactly 2 Python TimeRanges
            assertMatches("Overlap-trio", PYTHON_TR_PAIR_PAT, str);
            assertEval("resultTRL = result"); // cheat again
            // pull the results from the script
            TimeRange result0 = (TimeRange) testScript.execute("javaTimeRange",
                    "resultTRL[0]", emptyMap);
            TimeRange result1 = (TimeRange) testScript.execute("javaTimeRange",
                    "resultTRL[1]", emptyMap);
            // generate the timeranges we expected
            TimeRange expected0 = new TimeRange(frontTime / 1000 * 1000,
                    nowLessFiveMin / 1000 * 1000);
            TimeRange expected1 = new TimeRange(nowPlusFiveMin / 1000 * 1000,
                    foreTime / 1000 * 1000);
            // confirm that expected values match results
            assertEquals("expected[0]", expected0, result0);
            assertEquals("expected[1]", expected1, result1);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_getWEInventory() throws Exception {
        // Make sure an empty inventory returns an empty list
        try {
            argmap.put("WEName", "Brillig");
            Object obj = testScript.execute("_getWEInventory", "hazardUtils",
                    argmap);
            if (obj instanceof List) {
                @SuppressWarnings("unchecked")
                List<Object> list = (List<Object>) obj;
                assertEquals("Inventory should be empty", 0, list.size());
            } else if (obj instanceof String) {
                String str = (String) obj;
                assertEquals("", "[]", str);
            } else {
                fail("_getWEInventory returned "
                        + obj.getClass().getSimpleName());
            }
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Confirm that data is returned when data is present
        Date now = new Date();
        StringBuilder sb = new StringBuilder();
        sb.append("jtr = TR(0L,");
        sb.append(now.getTime());
        sb.append(")");

        try {
            assertEval("jtr = TR(0L, 221221221L)");
            assertEval("msl = MockSlice.MockSlice(jtr)");
            assertEval("mgd = MockGridData.MockGridData(msl)");
            assertEval("mp = MockParm.MockParm('mp')");
            assertEval("mp.gridInventory = [ mgd ]");
            assertEval("hazardUtils.parms['Brillig'] = mp");

            Object obj = testScript.execute("_getWEInventory", "hazardUtils",
                    argmap);
            if (obj instanceof String) {
                String str = (String) obj;
                str = str.substring(0, Math.min(str.length(), PYTHON_TR_STR
                        .length()));
                assertEquals("Return should be Python TimeRanges",
                        PYTHON_TR_STR, str);
            } else {
                fail("_getWEInventory returned "
                        + obj.getClass().getSimpleName());
            }
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Confirm that a Java TimeRange is returned when asJava is true
        argmap.put("::asJava", "True");
        try {
            Object obj = testScript.execute("_getWEInventory", "hazardUtils",
                    argmap);
            if (obj instanceof String) {
                String str = (String) obj;
                str = str.substring(0, Math.min(str.length(), JAVA_TR_STR
                        .length()));
                assertEquals("Return should be Java TimeRanges", JAVA_TR_STR,
                        str);
            } else {
                fail("_getWEInventory returned "
                        + obj.getClass().getSimpleName());
            }
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_makeEmptyHazardGrid() throws Exception {
        try {
            // set up our MockHazardUtils to log to a StringIO
            assertEval("hazardUtils.log = StringIO()");
            // create some discrete keys (strings and tuples) for "Hazards"
            assertEval("hazardUtils.discreteKeys['Hazards']=['TWAS^BRILLIG^AND^THE', ('SLITHY','NONSENSE WORD')]");
            // create a Java TimeRange to put in the argument map
            assertEval("jtr = TR(0L, 221221221L)");
            argmap.put("weName", "Hazards");
            argmap.put("::timeRange", "jtr");
            String str = (String) testScript.execute("_makeEmptyHazardGrid",
                    "hazardUtils", argmap);
            assertNull("__makeEmptyHazardGrid should not return a value", str);
            argmap.clear();
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    argmap);
            assertEquals("create", CREATE_STR, str);

            // erase the log
            assertEval("hazardUtils.log.truncate(0)");
            assertEval("hazardUtils.log.seek(0)");

            // try with a temp element
            assertEval("hazardUtils.discreteKeys['AF.Y'] = ['AF.Y']");
            argmap.put("weName", "hazAF.Y");
            argmap.put("::timeRange", "jtr");
            str = (String) testScript.execute("_makeEmptyHazardGrid",
                    "hazardUtils", argmap);
            assertNull("__makeEmptyHazardGrid should not return a value", str);
            argmap.clear();
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    argmap);
            assertEquals("create_temp", CREATE_TEMP_STR, str);

        } catch (JepException e) {
            throw new Exception(e);
        }

    }

    @Test
    public void test_makeNewKey() throws Exception {
        String str = null;
        try {

            // Test when oldkey is empty but phenSig is not
            argmap.put("oldKey", "");
            argmap.put("phenSig", "CAPTAIN");
            str = (String) testScript.execute("_makeNewKey", "hazardUtils",
                    argmap);
            assertEquals("Captain", "CAPTAIN", str);

            // Test both input strings empty
            argmap.put("oldKey", "");
            argmap.put("phenSig", "");
            str = (String) testScript.execute("_makeNewKey", "hazardUtils",
                    argmap);
            assertEquals("Empty", "", str);

            // Test that new key is in sorted order
            argmap.put("oldKey", "I^AM^THE^CAPTAIN^OF");
            argmap.put("phenSig", "PINAFORE");
            str = (String) testScript.execute("_makeNewKey", "hazardUtils",
                    argmap);
            assertEquals("Pinafore", "AM^CAPTAIN^I^OF^PINAFORE^THE", str);

            // Test that subkeys with equal phens take the more important sig
            argmap.put("oldKey", "I^AM^THE^CAPTAIN^OF^AND.A");
            argmap.put("phenSig", "AND.W");
            str = (String) testScript.execute("_makeNewKey", "hazardUtils",
                    argmap);
            assertEquals("And.a", "AM^AND.W^CAPTAIN^I^OF^THE", str);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_removeOldGrids() throws Exception {

        try {
            // Create a StringIO to serve as a logfile.
            assertEval("log = StringIO()");

            // Create a couple of MockParms using log
            assertEval("hazXYZParm = MockParm.MockParm('hazXYZ', log)");
            assertEval("fcstParm = MockParm.MockParm('fcst', log)");

            // put them in hazardUtils for getParm() override
            assertEval("hazardUtils.parms['first'] = hazXYZParm");
            assertEval("hazardUtils.parms['second'] = fcstParm");

            argmap.put("weName", "first");
            String str = (String) testScript.execute("_removeOldGrids",
                    "hazardUtils", argmap);

            assertNull("_removeOldGrids should return null", str);

            // 
            argmap.clear();
            str = (String) testScript.execute("getvalue", "log", argmap);
            assertTrue(
                    "output log:" + str,
                    str
                            .matches("parm\\(hazXYZ\\):deleteTR\\(<PyJobject object at 0x[0-9a-fA-F]+>\\)\\s"));

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_setupHazardsInventory() throws Exception {
        Calendar cal = Calendar.getInstance();
        String str = null;
        Map<String, Object> emptyMap = new HashMap<String, Object>();

        // **** Calculate some millisecond values for time ranges ****
        long now = cal.getTimeInMillis();
        int nowHour = cal.get(Calendar.HOUR_OF_DAY);
        cal.add(Calendar.MINUTE, 5);
        int nowP5Hour = cal.get(Calendar.HOUR_OF_DAY);
        if (nowHour == nowP5Hour) {
            // We don't much care when "now" is, but now+5 minutes shouldn't
            // break an hourly boundardy. If it does, just add 5 minutes to
            // both.
            now = cal.getTimeInMillis();
            cal.add(Calendar.MINUTE, 5);
        }
        long nowPlusFiveMin = cal.getTimeInMillis();

        cal.setTimeInMillis(now);
        cal.add(Calendar.HOUR_OF_DAY, -1);
        cal.add(Calendar.MINUTE, -5);
        long earlier = cal.getTimeInMillis();

        cal.setTimeInMillis(nowPlusFiveMin);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        long later = cal.getTimeInMillis();

        cal.add(Calendar.HOUR_OF_DAY, 1);
        cal.add(Calendar.MINUTE, 5);
        long muchLater = cal.getTimeInMillis();

        try {
            assertEval("hazardUtils.log = StringIO()");
            assertEval("tr0 = TR(" + earlier + ", " + now + ")");
            assertEval("tr1 = TR(" + nowPlusFiveMin + ", " + later + ")");
            assertEval("tr2 = TR(" + later + ", " + muchLater + ")");
            assertEval("trList = [tr0, tr1, tr2]");
            assertEval("slice0 = MockSlice.MockSlice(tr0)");
            assertEval("slice1 = MockSlice.MockSlice(tr1)");
            assertEval("slice2 = MockSlice.MockSlice(tr2)");
            assertEval("mockData0 = MockGridData.MockGridData(slice0)");
            assertEval("mockData1 = MockGridData.MockGridData(slice1)");
            assertEval("mockData2 = MockGridData.MockGridData(slice2)");
            assertEval("mockHazParm = MockParm.MockParm('Hazards')");
            assertEval("mockHazParm.gridInventory = [mockData0, mockData1, mockData2]");
            assertEval("hazardUtils.parms['Hazards'] = mockHazParm");

        } catch (JepException e) {
            throw new Exception(e);
        }

        // Confirm operation when WE does not exist
        try {
            argmap.put("weName", "hazAFY");
            argmap.put("::trList", "trList");
            str = (String) testScript.execute("_setupHazardsInventory",
                    "hazardUtils", argmap);
            assertNull(
                    "_setupHazardsInventory('hazAFY', trList) should return null",
                    str);
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    emptyMap);
            assertMatches("create_pat_a", CREATE_PAT_A, str);

        } catch (JepException e) {
            throw new Exception(e);
        }

        // Confirm w/ existing WE and TR w/start,end in same hour
        argmap.put("weName", "Hazards");
        try {
            // Clear the log
            assertEval("hazardUtils.log.truncate(0)");
            assertEval("hazardUtils.log.seek(0)");
            try {
                str = (String) testScript.execute("_setupHazardsInventory",
                        "hazardUtils", argmap);
            } catch (JepException je) {
                // Since we have a gap that only spans 5 minutes, a JepException
                // should be raised.
                // Since JepException is a catch-all, make sure it contains the
                // right value in its message.
                if (!je.getMessage().contains("Cannot split grid for")) {
                    fail(String.format("Bad exception message: %s", je
                            .getMessage()));
                }
            }
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Confirm w/ existing WE and TR w/start, end in different hours
        try {
            // Clear the log
            assertEval("hazardUtils.log.truncate(0)");
            assertEval("hazardUtils.log.seek(0)");

            // Drop the middle time range so remaining ranges are widely
            // separated
            assertEval("trList.remove(tr1)");
            assertEval("mockHazParm.gridInventory.remove(mockData1)");
            // Call the method. It should not throw.
            str = (String) testScript.execute("_setupHazardsInventory",
                    "hazardUtils", argmap);
            assertNull(
                    "_setupHazardsInventory('Hazards', [a,b]) should return null",
                    str);
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    emptyMap);
            assertMatches("Other create", CREATE_PAT_B, str);
        } catch (JepException e) {
            throw new Exception(e);
        }

    }

    @Test
    public void test_makeTimeRange() throws Exception {
        String str = null;

        Calendar cal = Calendar.getInstance();
        Date now = cal.getTime();
        cal.add(Calendar.HOUR_OF_DAY, 1);
        Date then = cal.getTime();
        argmap.put("start", now.getTime() / 1000);
        argmap.put("end", then.getTime() / 1000);
        try {
            str = (String) testScript.execute("_makeTimeRange", "hazardUtils",
                    argmap);
            assertMatches("_makeTimeRange()<1>", JTIMERANGE_PAT, str);

        } catch (JepException e) {
            throw new Exception(e);
        }

        argmap.put("start", now);
        argmap.put("end", then);
        try {
            str = (String) testScript.execute("_makeTimeRange", "hazardUtils",
                    argmap);
            assertMatches("_makeTimeRange()<2>", JTIMERANGE_PAT, str);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_createConsolidatedGrid() throws Exception {
        Calendar cal = Calendar.getInstance();
        String str = null;
        Map<String, Object> emptyMap = new HashMap<String, Object>();

        long now = cal.getTimeInMillis();
        int intNow = (int) (now / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 2);
        long later = cal.getTimeInMillis();
        int intLater = (int) (later / 1000);
        argmap.put("groupStart", intNow);
        argmap.put("groupEnd", intLater);
        argmap.put("weName", "hazAFY");
        try {
            assertEval("hazardUtils.log = StringIO()");
            // Generate a 145x145 byte-grid
            assertEval("byteGrid = np.zeros((145, 145), dtype='int8')");
            // Generate a hazard key
            assertEval("afyKey = 'AF.Y'");
            // stuff the byte-grid and key into a tuple
            assertEval("gdTuple = (byteGrid, [afyKey])");
            // cram the tuple into hazardUtils.grids[elem]
            assertEval("hazardUtils.grids['hazAFY'] = gdTuple");
            str = (String) testScript.execute("_createConsolidatedGrid",
                    "hazardUtils", argmap);
            assertNull("_createConsolidatedGrid should return null", str);
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    emptyMap);
            assertMatches("_createConsolidatedGrid", CREATE_CONS_PAT, str);
        } catch (JepException e) {
            throw new Exception(e);
        }

    }

    @Test
    public void test_consecutiveIdenticalGrids() throws Exception {
        Calendar cal = Calendar.getInstance();
        int now = (int) (cal.getTimeInMillis() / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        int later = (int) (cal.getTimeInMillis() / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        int muchLater = (int) (cal.getTimeInMillis() / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        int evenLater = (int) (cal.getTimeInMillis() / 1000);

        try {
            // create some byte grids
            assertEval("gridZero = np.zeros((145, 145), dtype='int8')");
            assertEval("gridOne = np.array(gridZero)");
            assertEval("gridTwo = np.array(gridZero)"); // identical, but
            // distinct
            assertEval("gridTwo[2,3] = 1");
            // create some time ranges
            assertEval("trngZero = hazardUtils._makeTimeRange(" + now + ", "
                    + later + ")");
            assertEval("trngOne = hazardUtils._makeTimeRange(" + later + ", "
                    + muchLater + ")");
            assertEval("trngTwo = hazardUtils._makeTimeRange(" + muchLater
                    + ", " + evenLater + ")");
            // create one key to use for all the time ranges
            assertEval("key = ['AF.Y']");
            // put them in hazardUtils.grids for MockHazardUtils.getGrids()
            assertEval("hazardUtils.grids['Hazards'] = (gridZero, key)");
            assertEval("startZero = trngZero.javaTimeRange().getStartTime().getTime()");
            assertEval("hazardUtils.grids[('Hazards', startZero)] = (gridOne, key)");
            argmap.put("weName", "Hazards");
            // when the time ranges are consecutive and the grids are identical,
            // the result should be true.
            argmap.put("::timeRange1", "trngZero");
            argmap.put("::timeRange2", "trngOne");
            int result = (Integer) testScript.execute(
                    "_consecutiveIdenticalGrids", "hazardUtils", argmap);
            assertEquals("_consecutiveIdenticalGrids should be 1(True)", 1,
                    result);

            // when the time ranges are not consecutive, the grids don't matter:
            // the result should be false.
            argmap.put("::timeRange2", "trngTwo");
            result = (Integer) testScript.execute("_consecutiveIdenticalGrids",
                    "hazardUtils", argmap);
            assertEquals("non-consecutive time ranges should return 0 (false)",
                    0, result);

            // when the time ranges are consecutive but the grids are not
            // identical, result should be false.
            assertEval("startOne = trngOne.javaTimeRange().getStartTime().getTime()");
            assertEval("hazardUtils.grids[('Hazards', startOne)] = (gridTwo, key)");
            argmap.put("::timeRange2", "trngOne");
            result = (Integer) testScript.execute("_consecutiveIdenticalGrids",
                    "hazardUtils", argmap);
            if (result != 0) {
                argmap.clear();
                String infoLog = (String) testScript.execute("getvalue",
                        "hazardUtils.infoLog", argmap);
                fail("non-identical grids returned 1:\n" + infoLog);
            }
            assertEquals("non-identical grids should return 0 (false)", 0,
                    result);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_consolidateTimes() throws Exception {
        Calendar cal = Calendar.getInstance();
        int now = (int) (cal.getTimeInMillis() / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        int later = (int) (cal.getTimeInMillis() / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        int muchLater = (int) (cal.getTimeInMillis() / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        int evenLater = (int) (cal.getTimeInMillis() / 1000);
        cal.add(Calendar.HOUR_OF_DAY, 1);
        int stillLater = (int) (cal.getTimeInMillis() / 1000);

        try {
            assertEval("hazardUtils.log = StringIO()");
            // create some time ranges
            assertEval("trngZero = hazardUtils._makeTimeRange(" + now + ", "
                    + later + ")");
            assertEval("trngOne = hazardUtils._makeTimeRange(" + later + ", "
                    + muchLater + ")");
            assertEval("trngTwo = hazardUtils._makeTimeRange(" + muchLater
                    + ", " + evenLater + ")");
            assertEval("trngThree = hazardUtils._makeTimeRange(" + evenLater
                    + ", " + stillLater + ")");
            // and some Java equivalents
            assertEval("trZero = trngZero.javaTimeRange()");
            assertEval("trOne = trngOne.javaTimeRange()");
            assertEval("trTwo = trngTwo.javaTimeRange()");
            assertEval("trThree = trngThree.javaTimeRange()");
            // Create some byte grids
            assertEval("gridZero = np.zeros((145, 145), dtype='int8')");
            assertEval("gridOne = np.array(gridZero)"); // identical but
            // distinct
            assertEval("gridTwo = np.ones((145, 145), dtype='int8')");
            assertEval("gridThree = np.array(gridTwo)");
            assertEval("gridThree[2,2] = 0");
            // Create some element names like temporary hazard elements
            assertEval("elemA = 'hazAFY'"); // ashfall
            assertEval("elemB = 'hazHTY'"); // heat
            assertEval("elemC = 'hazSMY'"); // smoke
            // Create a key for each element
            assertEval("keyA = ['AF.Y']");
            assertEval("keyB = ['HT.Y']");
            assertEval("keyC = ['SM.Y']");
            // Create a Python MockParm for each element
            assertEval("parmA = MockParm.MockParm('parmA', outObj = hazardUtils.log)");
            assertEval("parmB = MockParm.MockParm('parmB', outObj = hazardUtils.log)");
            assertEval("parmC = MockParm.MockParm('parmC', outObj = hazardUtils.log)");
            // Create a MockSlice for each java time range for each element
            assertEval("sliceZeroA = MockSlice.MockSlice(trZero)");
            assertEval("sliceOneA = MockSlice.MockSlice(trOne)");
            assertEval("sliceTwoA = MockSlice.MockSlice(trTwo)");
            assertEval("sliceThreeA = MockSlice.MockSlice(trThree)");
            assertEval("sliceZeroB = MockSlice.MockSlice(trZero)");
            assertEval("sliceOneB = MockSlice.MockSlice(trOne)");
            assertEval("sliceTwoB = MockSlice.MockSlice(trTwo)");
            assertEval("sliceThreeB = MockSlice.MockSlice(trThree)");
            assertEval("sliceZeroC = MockSlice.MockSlice(trZero)");
            assertEval("sliceOneC = MockSlice.MockSlice(trOne)");
            assertEval("sliceTwoC = MockSlice.MockSlice(trTwo)");
            assertEval("sliceThreeC = MockSlice.MockSlice(trThree)");
            // Put each gridSlice in a MockGridData
            assertEval("dataZeroA = MockGridData.MockGridData(sliceZeroA)");
            assertEval("dataOneA = MockGridData.MockGridData(sliceOneA)");
            assertEval("dataTwoA = MockGridData.MockGridData(sliceTwoA)");
            assertEval("dataThreeA = MockGridData.MockGridData(sliceThreeA)");
            assertEval("dataZeroB = MockGridData.MockGridData(sliceZeroB)");
            assertEval("dataOneB = MockGridData.MockGridData(sliceOneB)");
            assertEval("dataTwoB = MockGridData.MockGridData(sliceTwoB)");
            assertEval("dataThreeB = MockGridData.MockGridData(sliceThreeB)");
            assertEval("dataZeroC = MockGridData.MockGridData(sliceZeroC)");
            assertEval("dataOneC = MockGridData.MockGridData(sliceOneC)");
            assertEval("dataTwoC = MockGridData.MockGridData(sliceTwoC)");
            assertEval("dataThreeC = MockGridData.MockGridData(sliceThreeC)");
            // Put the MockGridDatas in as the parm inventories.
            // Leave some out to exercise different chunks of code.
            assertEval("parmA.gridInventory = [dataZeroA, dataOneA, dataTwoA, dataThreeA]");
            assertEval("parmB.gridInventory = [dataZeroB, dataOneB,           dataThreeB]");
            assertEval("parmC.gridInventory = [           dataOneC, dataTwoC, dataThreeC]");
            // put the parms in hazardUtils
            assertEval("hazardUtils.parms[elemA] = parmA");
            assertEval("hazardUtils.parms[elemB] = parmB");
            assertEval("hazardUtils.parms[elemC] = parmC");
            // create tuples to serve as discrete byte/key pair "grids".
            assertEval("gridZeroA = (gridZero, keyA)");
            assertEval("gridOneA = (gridOne, keyA)");
            assertEval("gridTwoA = (gridZero, keyA)");
            assertEval("gridThreeA = (gridOne, keyA)");
            assertEval("gridZeroB = (gridZero, keyB)");
            assertEval("gridOneB = (gridOne, keyB)");
            assertEval("gridThreeB = (gridThree, keyB)");
            assertEval("gridOneC = (gridOne, keyC)");
            assertEval("gridTwoC = (gridTwo, keyC)");
            assertEval("gridThreeC = (gridThree, keyC)");
            // put the tuples in hazardUtils for mockHazardUtils.getGrids().
            assertEval("hazardUtils.grids[(elemA, trZero.getStartTime().getTime())] = gridZeroA");
            assertEval("hazardUtils.grids[(elemA, trOne.getStartTime().getTime())] = gridOneA");
            assertEval("hazardUtils.grids[(elemA, trTwo.getStartTime().getTime())] = gridTwoA");
            assertEval("hazardUtils.grids[(elemA, trThree.getStartTime().getTime())] = gridThreeA");
            assertEval("hazardUtils.grids[(elemB, trZero.getStartTime().getTime())] = gridZeroB");
            assertEval("hazardUtils.grids[(elemB, trOne.getStartTime().getTime())] = gridOneB");
            assertEval("hazardUtils.grids[(elemB, trThree.getStartTime().getTime())] = gridThreeB");
            assertEval("hazardUtils.grids[(elemC, trOne.getStartTime().getTime())] = gridOneC");
            assertEval("hazardUtils.grids[(elemC, trTwo.getStartTime().getTime())] = gridTwoC");
            assertEval("hazardUtils.grids[(elemC, trThree.getStartTime().getTime())] = gridThreeC");
            // put the element names in a list
            assertEval("elemList = [elemA, elemB, elemC]");
            // Finally, everything is in place. We can call the method we really
            // wanted to test.
            argmap.put("::weNameList", "elemList");
            Object obj = testScript.execute("_consolidateTimes", "hazardUtils",
                    argmap);
            assertNull("_consolidateTimes should return None", obj);
            argmap.clear();
            String str = (String) testScript.execute("getvalue",
                    "hazardUtils.log", argmap);
            assertMatches("Consolidate times", CONSOL_PAT, str);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_GetUniqueKeys() throws Exception {
        try {
            // When all grid points are zero, we should get '<None>'.
            assertEval("inputKeys = ['<None>', 'AF.Y', 'HT.Y', 'SM.Y', 'CF.A', 'CF.W', 'EH.A', 'EH.W']");
            assertEval("dataGrid = np.zeros((145, 145), dtype='int8')");
            argmap.put("::keys", "inputKeys");
            argmap.put("::byteGrid", "dataGrid");
            String str = (String) testScript.execute("_getUniqueKeys",
                    "hazardUtils", argmap);
            assertEquals("all zeros", "['<None>']", str);

            // With a '3' point value, keys[3] should also be kept.
            assertEval("dataGrid[3,3]=3");
            str = (String) testScript.execute("_getUniqueKeys", "hazardUtils",
                    argmap);
            assertEquals("one three", "['<None>', 'SM.Y']", str);

            // masking the '3' off should give '<None>' again.
            assertEval("bitMask = np.zeros((145, 145), dtype='bool')");
            assertEval("bitMask[4:,4:] = True");
            argmap.put("::mask", "bitMask");
            str = (String) testScript.execute("_getUniqueKeys", "hazardUtils",
                    argmap);
            assertEquals("simple mask", "['<None>']", str);

            // masking every point should give an empty key list
            assertEval("bitMask[:,:] = False");
            str = (String) testScript.execute("_getUniqueKeys", "hazardUtils",
                    argmap);
            assertEquals("fully masked", "[]", str);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_getSubKeys() throws Exception {
        try {
            argmap.put("key", "<None>");
            String str = (String) testScript.execute("_getSubKeys",
                    "hazardUtils", argmap);
            assertEquals("keyArg of '<None>'", "[]", str);

            argmap.put("key", "<None>^AF.Y");
            str = (String) testScript.execute("_getSubKeys", "hazardUtils",
                    argmap);
            assertEquals("<None> and AF.Y'", "['AF.Y']", str);

            argmap.put("key", "AF.Y^AF.Y^EH.Y");
            str = (String) testScript.execute("_getSubKeys", "hazardUtils",
                    argmap);
            assertEquals("repeated", "['AF.Y', 'AF.Y', 'EH.Y']", str);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_printTime() throws Exception {

        try {
            argmap.put("t", 0);
            String str = (String) testScript.execute("_printTime",
                    "hazardUtils", argmap);
            assertEquals("epoch", "19700101_0000", str);

        } catch (JepException e) {
            throw new Exception(e);
        }

        Date now = new Date();
        int intNow = (int) (now.getTime() / 1000);

        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd_HHmm");
        String nowStr = sdf.format(now);

        try {
            argmap.put("t", intNow);
            String str = (String) testScript.execute("_printTime",
                    "hazardUtils", argmap);
            assertEquals("now", nowStr, str);

        } catch (JepException e) {
            throw new Exception(e);
        }

    }

    @Test
    public void test_makeTempWEName() throws Exception {

        argmap.put("key", "AF.Y");
        String str;
        try {
            str = (String) testScript.execute("_makeTempWEName", "hazardUtils",
                    argmap);
            assertEquals("temp WE name for AF.Y", "hazAFY", str);

            argmap.put("key", "EH.Y:2468");
            str = (String) testScript.execute("_makeTempWEName", "hazardUtils",
                    argmap);
            assertEquals("temp WE name for EH.Y:2468", "hazEHY2468", str);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_addHazard() throws Exception {
        Object obj = null;
        String str = null;
        Map<String, Object> emptyMap = new HashMap<String, Object>();
        Calendar cal = Calendar.getInstance();
        long now = cal.getTimeInMillis();

        // Set up a log so we can tell whether we call SmartScript methods
        // that MockHazardUtils overrides.
        try {
            assertEval("hazardUtils.log = StringIO()");
        } catch (JepException e) {
            throw new Exception(e);
        }

        // *** Set up the scaffolding for SmartScript methods that
        // MockHazardUtils overrides. ***
        // This is a fairly simple setup; a heat warning that was issued
        // at the start of the current hour that lasts a full day
        long startOfHour = now - (now % 3600000);
        long tomorrow = startOfHour + 24L * 60 * 60 * 1000;
        try {
            // Create Java time ranges in Python
            assertEval("oneDay = TR(" + startOfHour + ", " + tomorrow + ")");
            // Put the time range in a MockSlice
            assertEval("daySlice = MockSlice.MockSlice(oneDay)");
            // Put the gridSlice in a MockGridData
            assertEval("dayGridData = MockGridData.MockGridData(daySlice)");
            // Create Python MockParms
            assertEval("initialParm = MockParm.MockParm('Hazards', hazardUtils.log)");
            // Put the MockGridDatas in the parms' inventories
            assertEval("initialParm.gridInventory = [ dayGridData ]");
            // Put the parms in hazardUtils
            assertEval("hazardUtils.parms['Hazards'] = initialParm");
            str = (String) testScript.execute("keys", "hazardUtils.parms",
                    emptyMap);
            assertEquals("hazardUtils.parms.keys()", "['Hazards']", str);
            // Create a byte array to serve as data
            assertEval("heatPlot = np.zeros((145, 145), dtype='int8')");
            assertEval("heatPlot[4:8,4:8] = 1");
            // Create a key string
            assertEval("heatKey = '<None>^EH.Y'");
            // create a "discrete grid" tuple from heatPlot and heatKey
            assertEval("heatTuple = (heatPlot, ['<None>', heatKey])");
            // put the grid in hazardUtils
            assertEval("hazardUtils.grids['Hazards'] = heatTuple");
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Adding a hazard ending more than an hour in the past should silently
        // return w/o changing anything.
        // TODO: Look for error message once one is generated by this error
        try {
            cal.add(Calendar.HOUR_OF_DAY, -2);
            TimeRange nearPast = new TimeRange(cal.getTime(), 3600L);

            assertEval("hazardUtils.log = StringIO()");
            assertEval("onMask = np.ones((145, 145), dtype='bool')");
            argmap.put("weName", "Hazards");
            argmap.put("timeRange", nearPast);
            argmap.put("addHaz", "AF.Y:1234^EH.Y");
            argmap.put("::mask", "onMask");
            argmap.put("combine", 0);
            obj = testScript.execute("_addHazard", "hazardUtils", argmap);
            assertNull("_addHazard doesn't return anything", obj);
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    emptyMap);
            assertEquals("nearPast", "", str);

        } catch (JepException e) {
            throw new Exception(e);
        }

        // Add a hazard that began at the start of the hour that lasts
        // to the end of the hour.
        try {
            // clear the log
            assertEval("hazardUtils.log.truncate(0)");
            assertEval("hazardUtils.log.seek(0)");

            cal.setTimeInMillis(startOfHour);
            TimeRange thisHour = new TimeRange(cal.getTime(), 3600L);
            assertEval("onMask = np.ones((145, 145), dtype='bool')");
            argmap.put("weName", "Hazards");
            argmap.put("timeRange", thisHour);
            argmap.put("addHaz", "AF.Y:1234^EH.Y");
            argmap.put("::mask", "onMask");
            argmap.put("combine", 0);
            obj = testScript.execute("_addHazard", "hazardUtils", argmap);
            assertNull("_addHazard doesn't return anything", obj);
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    emptyMap);
            assertMatches("nearFuture", ADDHAZ_PAT, str);
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Do the same thing, but pass a Python version of thisHour
        try {
            // clear the log
            assertEval("hazardUtils.log.truncate(0)");
            assertEval("hazardUtils.log.seek(0)");

            cal.setTimeInMillis(startOfHour);
            assertEval("onMask = np.ones((145, 145), dtype='bool')");
            assertEval("thisHour = hazardUtils._makeTimeRange("
                    + (startOfHour / 1000) + ", " + (startOfHour / 1000 + 3600)
                    + ")");
            argmap.clear();
            argmap.put("weName", "Hazards");
            argmap.put("::timeRange", "thisHour");
            argmap.put("addHaz", "AF.Y:1234^EH.Y");
            argmap.put("::mask", "onMask");
            argmap.put("combine", 0);
            obj = testScript.execute("_addHazard", "hazardUtils", argmap);
            assertNull("_addHazard doesn't return anything", obj);
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    emptyMap);
            assertMatches("nearFuture", ADDHAZ_PAT, str);
        } catch (JepException e) {
            throw new Exception(e);
        }

        // Repeat the first test, using a temporary hazard name.
        try {
            // clear the log
            assertEval("hazardUtils.log.truncate(0)");
            assertEval("hazardUtils.log.seek(0)");

            cal.setTimeInMillis(startOfHour);
            TimeRange thisHour = new TimeRange(cal.getTime(), 3600L);
            assertEval("onMask = np.ones((145, 145), dtype='bool')");
            argmap.clear();
            argmap.put("weName", "hazAFY1234");
            argmap.put("timeRange", thisHour);
            argmap.put("addHaz", "AF.Y:1234");
            argmap.put("::mask", "onMask");
            argmap.put("combine", 0);
            obj = testScript.execute("_addHazard", "hazardUtils", argmap);
            assertNull("_addHazard doesn't return anything", obj);
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    emptyMap);
            assertMatches("nearFuture", ADDHAZ_PAT_2, str);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_tempWELoaded() throws Exception {
        Integer result = null;
        try {
            result = (Integer) testScript.execute("_tempWELoaded",
                    "hazardUtils", argmap);
            assertEquals("no parms", Integer.valueOf(0), result);

            assertEval("hazards = MockParm.MockParm('Hazards')");
            assertEval("hazardUtils.parms['Hazards'] = hazards");
            result = (Integer) testScript.execute("_tempWELoaded",
                    "hazardUtils", argmap);
            assertEquals("Hazards only", Integer.valueOf(0), result);

            assertEval("afytemp = MockParm.MockParm('AF.Y')");
            assertEval("hazardUtils.parms['hazAFY'] = afytemp");
            result = (Integer) testScript.execute("_tempWELoaded",
                    "hazardUtils", argmap);
            assertEquals("Temp present", Integer.valueOf(1), result);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_startEdit() throws Exception {
        Calendar cal = Calendar.getInstance();
        long now = cal.getTimeInMillis();
        String str = null;
        try {
            assertEval("timeRange = TR(" + now + ", " + (now + 1) + ")");
            assertEval("gridSlice = MockSlice.MockSlice(timeRange)");
            assertEval("gridData = MockGridData.MockGridData(gridSlice)");
            assertEval("hazParm = MockParm.MockParm('Hazards')");
            assertEval("hazParm.gridInventory = [ gridData ]");
            assertEval("hazardUtils.parms['Hazards'] = hazParm");
            str = (String) testScript.execute("_startEdit", "hazardUtils",
                    argmap);
            assertMatches("first edit", START_EDIT_PAT, str);

            assertEval("hazardUtils.log = StringIO()");
            str = (String) testScript.execute("_startEdit", "hazardUtils",
                    argmap);
            assertEquals("second edit", "(None, None)", str);
            argmap.clear();
            str = (String) testScript.execute("getvalue", "hazardUtils.log",
                    argmap);
            assertEquals(
                    "status bar",
                    "statusBarMsg(message='There are conflicting locks.  Please resolve these before adding any hazards', status='S', category='GFE')\n",
                    str);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void test_separateHazardGrids() throws Exception {

        Boolean result = null;
        String logStr = null;
        Calendar cal = Calendar.getInstance();
        long now = cal.getTimeInMillis();
        try {
            assertEval("hazardUtils.log = StringIO()");
            assertEval("hazParm = MockParm.MockParm('Hazards', hazardUtils.log)");
            assertEval("timeRange = TR(" + now + ", " + (now + 60000) + ")");
            assertEval("gridSlice = MockSlice.MockSlice(timeRange)");
            assertEval("data = MockGridData.MockGridData(gridSlice)");
            assertEval("hazParm.gridInventory.append(data)");
            assertEval("hazardUtils.parms['Hazards'] = hazParm");
            assertEval("byteGrid = np.zeros((145, 145), dtype='int8')");
            assertEval("byteGrid[4,4] = 1");
            assertEval("byteGrid[0,3] = 2");
            assertEval("keys = ['<None>','AF.Y^EH.A','EH.A', 'DU.Y']");
            assertEval("hazardUtils.grids['Hazards'] = (byteGrid, keys)");
            result = (Boolean) testScript.execute("_separateHazardGrids",
                    "hazardUtils", argmap);
            assertEquals("", Boolean.valueOf(true), result);
            logStr = (String) testScript.execute("getvalue", "hazardUtils.log",
                    argmap);
            assertMatches("", SEP_HAZ_PAT, logStr);
            result = (Boolean) testScript.execute("_separateHazardGrids",
                    "hazardUtils", argmap);
            assertEquals("", Boolean.valueOf(false), result);
        } catch (JepException e) {
            throw new Exception(e);
        }
    }

    @Test
    public void testAsdf() throws Exception {
        try {
            List<String> jabberwocky = new ArrayList<String>();
            jabberwocky = Arrays.asList("Twas brillig and the slithy toves"
                    .split(" "));
            assertEval("def printit(obj): print obj");
            argmap.put("obj", jabberwocky);
            Object dontcare = testScript.execute("printit", null, argmap);

        } catch (JepException e) {
            throw new Exception(e);
        }
    }
}
