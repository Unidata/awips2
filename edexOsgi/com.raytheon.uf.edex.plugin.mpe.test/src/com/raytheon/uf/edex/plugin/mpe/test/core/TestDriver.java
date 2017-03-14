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
package com.raytheon.uf.edex.plugin.mpe.test.core;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.AppsDefaultsConversionWrapper;
import com.raytheon.uf.edex.plugin.mpe.test.ValidationTestException;
import com.raytheon.uf.edex.plugin.mpe.test.impl.BiasmesgenValidationTest;
import com.raytheon.uf.edex.plugin.mpe.test.impl.RocCheckerValidationTest;

/**
 * Test Driver that will run any registered mpe data validation tests and report
 * the final results.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2016  5614       bkowal      Initial creation
 * Jun 13, 2016 5576       bkowal      Implemented test registration and execution.
 * Jun 29, 2016 5699       bkowal      Register {@link RocCheckerValidationTest}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class TestDriver {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String MPE_VERIFY_ENABLED_PROPERTY = "mpe.convert.verify.enabled";

    private final boolean enabled = Boolean
            .getBoolean(MPE_VERIFY_ENABLED_PROPERTY);

    private final List<IValidationTest> registeredTests = new ArrayList<>();

    public TestDriver() {
        registeredTests.add(new BiasmesgenValidationTest());
        registeredTests.add(new RocCheckerValidationTest());
    }

    public void runTests() {
        if (!enabled || !AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            logger.info("MPE verification tests have been disabled.");
            return;
        }

        /*
         * Determine the run hour of the tests.
         */
        Calendar currentCalendar = TimeUtil.newGmtCalendar();
        /*
         * Subtract an hour and zero out minutes, seconds, and milliseconds.
         */
        currentCalendar.add(Calendar.HOUR, -1);
        TimeUtil.minCalendarFields(currentCalendar, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);

        ITimer verifyTimer = TimeUtil.getTimer();
        verifyTimer.start();
        logger.info("MPE verification tests: {} running ...", currentCalendar
                .getTime().toString());

        for (IValidationTest validationTest : registeredTests) {
            final String testName = validationTest.getTestName();
            if (testName == null || testName.isEmpty()) {
                logger.error(
                        "Refusing to run unidentified Validation Test: {}.",
                        validationTest.getClass().getName());
                continue;
            }

            ITimer verifyTestTimer = TimeUtil.getTimer();
            verifyTestTimer.start();
            logger.info("Running Validation Test: {} ...", testName);
            try {
                validationTest.executeValidationTest(currentCalendar);
            } catch (ValidationTestException e) {
                logger.error("Validation Test: " + testName + " has failed.", e);
                continue;
            } catch (Exception e) {
                logger.error("Caught unhandled exception for Validation Test: "
                        + testName + ".", e);
                continue;
            }
            verifyTestTimer.stop();
            logger.info("Validation Test: {} has finished; duration = {}.",
                    testName,
                    TimeUtil.prettyDuration(verifyTestTimer.getElapsedTime()));

        }

        verifyTimer.stop();
        logger.info("MPE verification tests: {} have finished; duration = {}.",
                currentCalendar.getTime().toString(),
                TimeUtil.prettyDuration(verifyTimer.getElapsedTime()));
    }
}