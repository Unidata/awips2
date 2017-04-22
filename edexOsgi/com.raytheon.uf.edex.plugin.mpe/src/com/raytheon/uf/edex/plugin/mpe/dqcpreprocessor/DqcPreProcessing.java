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

package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.MpeException;

/**
 * Service implementation for executing the Dqc Preprocessor application. It
 * works in parallel with {@link DqcPreProcSrv}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2016 4623       skorolev    Initial creation
 * Mar 22, 2017 6180       bkowal      Updates so that it can be executed within its
 *                                     own camel route. Identified future improvements.
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class DqcPreProcessing {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    public boolean executeAllowed() {
        return AppsDefaultsConversionWrapper.parallelExecEnabled();
    }

    /**
     * Starts DQC Preprocessor.
     * 
     * @param dqcArg
     * @throws MpeException
     */
    public void execute() throws MpeException {
        /*
         * TODO: This value should be read from a localization file to match
         * other implementations within the MPE porting effort. It is presently
         * hard-coded in the code as (10) within the legacy ported code.
         * However, the port was poorly written; so, using the default number of
         * days causes the application to run for a significant amount of time.
         * When only set to 0 (= 1 day; because 1 is currently always added to
         * the number of days), the application finishes within minutes instead
         * of hours or more.
         */
        final String numOfDays = "0";
        if (AppsDefaults.getInstance().setAppContext(this)) {
            logger.info("STATUS: Start running dailyQC preprocessor.");
            if (dqcPreprocessor(numOfDays)) {
                logger.info("DQC Preprocessor execution successful");
            } else {
                logger.error("DQC Preprocessor terminated abnormally");
            }
        } else {
            logger.error(
                    "Token for the context in the APP_CONTEXT variable does not defined or is OFF");
        }
    }

    /**
     * DQC Preprocessor.
     * 
     * @param numOfDays
     *            Number of days string
     * @return true if no problem
     * @throws MpeException
     */
    private boolean dqcPreprocessor(String numOfDays) throws MpeException {
        // define the start date/time
        int numDays;
        try {
            numDays = Integer.parseInt(numOfDays);
        } catch (NumberFormatException e) {
            logger.error(
                    "Invalid string for Number of days: {}. Will use default value.\n{}",
                    numOfDays, e);
            numDays = 10;
        }
        Calendar cal = TimeUtil.newGmtCalendar();
        TimeUtil.minCalendarFields(cal, Calendar.HOUR, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);
        cal.add(Calendar.DAY_OF_YEAR, -numDays);
        Date runTime = cal.getTime();
        /*
         * advance one day from current day in order to get point data at 12~18Z
         * for the current day
         */
        numDays = numDays + 1;

        // build the area token list.
        String areas = AppsDefaults.getInstance()
                .getToken(PreProcConstants.MPE_AREA_NAMES);
        List<String> areaList = Arrays.asList(areas.split(","));
        int areaNumber = areaList.size();

        logger.info("STATUS: Setup parameters:");
        logger.info("        Running days: {}", numDays);
        logger.info("        Start time: {}", TimeUtil.formatCalendar(cal));
        logger.info("        Database: {}",
                AppsDefaults.getInstance().getToken(PreProcConstants.DB_NAME));
        logger.info("        mpe_site_id: {}", AppsDefaults.getInstance()
                .getToken(PreProcConstants.MPE_SITE_ID));
        logger.info("        Sub area list: {}", areaList.get(0));
        if (areaNumber > 1) {
            for (int i = 1; i <= areaNumber; i++) {
                logger.info("        {}", areaList.get(i));
            }
        }

        // Build the level 1 data for each sub-area and the master list.
        for (int i = 0; i < areaNumber; i++) {
            String area = areaList.get(i);
            /*
             * Get the station data. if the file is not available, then statusOK
             * = false.
             */
            boolean statusOK = PreProcUtils.readStationFile(numDays, area);
            if (statusOK) {
                // Get the daily precip data.
                try {
                    PrecipProc.processDailyPP(runTime, numDays);
                    // Get the HourlyPP/HourlyPC data.
                    PrecipProc.processHourlyPPPC(runTime, numDays);
                    // Write the precip data to a file.
                    PreProcUtils.writePrecipData(runTime, numDays, area);
                    // Get the 00z, 06z, 12z, 18z, max/min temperature data.
                    TemperatureProc.processTemperature(runTime, numDays);
                    // Write the temperature data to a file.
                    PreProcUtils.writeTemperatureData(runTime, numDays, area);
                    // Release precip and temperature arrays.
                    DqcPreProcInit.releasePrecipArray();
                    DqcPreProcInit.releaseTempArray();
                } catch (MpeException e) {
                    logger.error("Error getting data", e);
                    return false;
                }
            } else {
                return false;
            }
        }
        return true;
    }

}
