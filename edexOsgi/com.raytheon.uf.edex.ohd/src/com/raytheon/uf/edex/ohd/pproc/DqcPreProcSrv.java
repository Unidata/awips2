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

package com.raytheon.uf.edex.ohd.pproc;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ohd.MainMethod;

/**
 * Service implementation for executing the Dqc Preprocessor application
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25 2008             snaples    Initial creation
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */
public class DqcPreProcSrv {

    /** The argument pattern if only hours are specified */
    private static final String DAYS_ARG = "\\d{1,2}";

    /** The default number of days to process if no argument if provided */
    private static final String defaultNumDays = "10";

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private Log logger = LogFactory.getLog(getClass());

    public Object process(String dqcArg) throws EdexException {

        if (!dqcArg.matches(DAYS_ARG)) {
            logger.error("Invalid argument sent to DQC_preprocessor");
        }

        int exitValue = 0;
        if (AppsDefaults.getInstance().setAppContext(this)) {
            if (dqcArg.matches(DAYS_ARG)) {
                logger.info("Executing DQC Preprocessor with argument: "
                        + dqcArg);
                exitValue = MainMethod.runProgram("ksh",
                        appsDefaults.getToken("pproc_bin")
                                + "/run_dqc_preprocessor", "-d" + dqcArg);
            } else if (DAYS_ARG == null) {
                logger.info("Executing DQC Preprocessor with default number of days:  "
                        + defaultNumDays);
                exitValue = MainMethod.runProgram("ksh",
                        appsDefaults.getToken("pproc_bin")
                                + "/run_dqc_preprocessor", "-d"
                                + defaultNumDays);
            }

            if (exitValue == 0) {
                logger.info("DQC Preprocessor execution successful");
            } else {
                logger.error("DQC Preprocessor terminated abnormally with exit code: "
                        + exitValue);
            }
        }
        return exitValue;
    }

    public Object runDQCPreproc() throws EdexException {
        return process(defaultNumDays);
    }

}
