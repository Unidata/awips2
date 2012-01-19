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

import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.time.TimeRange;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 1, 2008            wdougherty     Initial creation
 * </pre>
 * 
 * @author wdougherty
 * @version 1.0
 */

public class TestAbsTime extends TestCase {

    protected static final String testScriptName = "ROOT/build/static/common/cave/etc/gfe/userPython/tests/TestAbsTime.py";

    protected static final String smartScriptPath = "ROOT/build/static/common/cave/etc/gfe/userPython/utilities"
            + ":ROOT/build"
            + ":ROOT/AWIPSEdex/opt/utility/common_static/base/python/gfe"
            + ":ROOT/AWIPSEdex/opt/utility/common_static/base/python"
            + ":ROOT/AWIPSEdex/extensions/plugin-gfe/src";

    PythonScript testScript;

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    @Override
    public void setUp() throws Exception {
        try {
            // Set up an interpreter that can run Python with Java classes.
            // Hopefully, using user.home will make this portable to any
            // developer.
            String root = System.getProperty("user.home") + File.separator
                    + "workspace";

            testScript = new PythonScript(testScriptName.replaceAll("ROOT",
                    root), smartScriptPath.replaceAll("ROOT", root));

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
     * @throws Exception
     */
    public void testAbsTimeZero() throws Exception {
        Map<String, Object> emptyMap = new HashMap<String, Object>();
        try {
            Date now = new Date();
            // AbsTime truncates to the nearest second. Wait one second
            // so the truncated time isn't before "now".
            Thread.sleep(1000);
            TimeRange tr = (TimeRange) testScript.execute("testAbsTimeZero",
                    null, emptyMap);
            assertTrue(tr.contains(now));
        } catch (JepException e) {
            throw new Exception(e);
        }
    }
}
