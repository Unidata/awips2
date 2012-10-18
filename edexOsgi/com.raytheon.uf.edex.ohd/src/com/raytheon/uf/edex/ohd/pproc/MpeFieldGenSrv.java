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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ohd.MainMethod;

/**
 * Service implementation for executing the MpeFieldGen application
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class MpeFieldGenSrv {

    /** The argument pattern if only hours are specified */
    private static final String HOURS_ARG = "\\d{1,2}";

    /** The argument pattern if hours and date is specified */
    private static final String HOURS_DATE_ARG = "(\\d{1,2}) (\\d{2}) (\\d{8})";

    private static final Pattern HOURS_DATE_PATTERN = Pattern
            .compile(HOURS_DATE_ARG);

    /** The default number of hours to process if no argument if provided */
    private static final String defaultMpeArg = "3";

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private Log logger = LogFactory.getLog(getClass());

    public Object process(String mpeArg) throws EdexException {

        if (!mpeArg.matches(HOURS_ARG) && !mpeArg.matches(HOURS_DATE_ARG)) {
            throw new EdexException("Invalid argument sent to mpe_fieldgen");
        }

        int exitValue = 0;
        if (appsDefaults.setAppContext(this)) {
            if (mpeArg.matches(HOURS_ARG)) {
                logger.info("Executing MPE FieldGen with argument: " + mpeArg);
                exitValue = MainMethod.runProgram("ksh",
                        appsDefaults.getToken("pproc_bin")
                                + "/run_mpe_fieldgen", mpeArg);
            } else if (mpeArg.matches(HOURS_DATE_ARG)) {
                logger.info("Executing MPE FieldGen with arguments: " + mpeArg);
                Matcher matcher = HOURS_DATE_PATTERN.matcher(mpeArg);
                if (matcher.find()) {
                    exitValue = MainMethod.runProgram("ksh",
                            appsDefaults.getToken("pproc_bin")
                                    + "/run_mpe_fieldgen", matcher.group(1),
                            matcher.group(2), matcher.group(3));
                }

            }
        }

        if (exitValue == 0) {
            logger.info("MpeFieldGen execution successful");
        } else {
            throw new EdexException(
                    "MpeFieldGen process terminated abnormally with exit code: "
                            + exitValue);
        }
        return exitValue;
    }

    public Object runHourlyMpe() throws EdexException {
        return process(defaultMpeArg);
    }

}
