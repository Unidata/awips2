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

import com.raytheon.uf.common.mpe.fieldgen.MpeFieldGenRequest;
import com.raytheon.uf.common.mpe.fieldgen.MpeFieldGenResponse;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
 * Mar 28, 2014   2952     mpduff      Changed to use ThriftSrv and UFStatus, cleanup
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class MpeFieldGenSrv implements IRequestHandler<MpeFieldGenRequest> {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(MpeFieldGenSrv.class);

    /** The argument pattern if only hours are specified */
    private static final String HOURS_ARG = "\\d{1,2}";

    /** The argument pattern if hours and date is specified */
    private static final String HOURS_DATE_ARG = "(\\d{1,2}) (\\d{2}) (\\d{8})";

    private static final Pattern HOURS_DATE_PATTERN = Pattern
            .compile(HOURS_DATE_ARG);

    private static final Pattern HOURS_ARG_PATTERN = Pattern.compile(HOURS_ARG);

    /** The default number of hours to process if no argument if provided */
    private static final String DEFAULT_ARG = "3";

    private static final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private static final String EXECUTE_PATH = appsDefaults
            .getToken("pproc_bin") + "/run_mpe_fieldgen";

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(MpeFieldGenRequest request) throws Exception {
        MpeFieldGenResponse response = new MpeFieldGenResponse();
        int exitValue = process(request.getArgs());
        response.setExitValue(exitValue);

        return response;
    }

    /**
     * Execute mpe fieldgen.
     * 
     * @param mpeArg
     *            Program arguments
     * @return exit status
     * @throws EdexException
     */
    private int process(String mpeArg) throws EdexException {

        if (!HOURS_ARG_PATTERN.matcher(mpeArg).matches()
                && !HOURS_DATE_PATTERN.matcher(mpeArg).matches()) {
            throw new EdexException("Invalid argument sent to mpe_fieldgen: "
                    + mpeArg);
        }

        int exitValue = 0;
        logger.info("Executing MPE FieldGen with argument: " + mpeArg);

        if (appsDefaults.setAppContext(this)) {
            if (HOURS_ARG_PATTERN.matcher(mpeArg).matches()) {
                exitValue = MainMethod.runProgram("ksh", EXECUTE_PATH, mpeArg);
            } else if (mpeArg.matches(HOURS_DATE_ARG)) {
                Matcher matcher = HOURS_DATE_PATTERN.matcher(mpeArg);
                if (matcher.find()) {
                    exitValue = MainMethod.runProgram("ksh", EXECUTE_PATH,
                            matcher.group(1), matcher.group(2),
                            matcher.group(3));
                }
            } else {
                throw new EdexException(
                        "Invalid argument sent to mpe_fieldgen: " + mpeArg);
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

    /**
     * Run fieldgen with the defalut argument.
     * 
     * @return exit status
     * @throws EdexException
     */
    public Object runHourlyMpe() throws EdexException {
        return process(DEFAULT_ARG);
    }

}
