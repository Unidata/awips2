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

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

import jep.JepException;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.time.TimeRange;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 01, 2008            wdougherty   Initial creation
 * Sep 05, 2013  #2307     dgilling     Fix test case.
 * </pre>
 * 
 * @author wdougherty
 * @version 1.0
 */

public class TestAbsTime {

    private static final File SCRIPT_FILE = new File(
            "./python/gfe/TestAbsTime.py");

    private PythonScript testScript = null;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        PathManagerFactoryTest.initLocalization();
    }

    @Before
    public void setUp() throws Exception {
        String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getUtilitiesIncludePath());

        try {
            testScript = new PythonScript(SCRIPT_FILE.getPath(), includePath,
                    this.getClass().getClassLoader());
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
    public void testAbsTimeZero() throws Exception {
        Map<String, Object> emptyMap = Collections.emptyMap();
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
