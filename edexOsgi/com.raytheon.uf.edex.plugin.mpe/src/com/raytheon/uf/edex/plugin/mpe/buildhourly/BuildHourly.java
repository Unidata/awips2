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
package com.raytheon.uf.edex.plugin.mpe.buildhourly;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;
import com.raytheon.uf.common.dataplugin.shef.tables.IRawTS;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.edex.plugin.mpe.AppsDefaultsConversionWrapper;
import com.raytheon.uf.edex.plugin.mpe.AppsDefaultsPathException;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.CurPPDao;

/**
 * Read the CurPP table, look for sub hourly data ( dur < 1001 ), do an
 * aggregate of their values and prepare load files for gage_pp to update in the
 * HourlyPP table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2016  5571       skorolev    Initial creation
 * Aug 25, 2016  5857      skorolev    Corrected logger.info when file not created
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class BuildHourly {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    /*
     * Default number of hours for which the hourly total is calculated. The
     * look back window is the number of previous hours that this program will
     * use to prepare the aggregate value.
     */
    private int lbWindow = 3;

    /*
     * Default minimum percent. The minimum percent of an hour required for the
     * hour of data to be used. Create the load file for the hour only if the
     * data obtained by summation of the sub hourly data covers the most of
     * hour.
     */
    private float minPercentFill = new Float(0.75);

    /*
     * Default type source argument. Type handling. The type source argument
     * will be SAME / PTYPE. SAME - then if the records in the curpp have type
     * source as RZ then the aggregate of the records will also have its type
     * source as RZ. If PTYPE - then the type source RZ will be changed to PZ,
     * RR to PR and so on.
     */
    private String typeSourceStr = BuildHourlyConstants.SAME;

    /* Log string for TypeSource */
    private String logTypeSourceStr = BuildHourlyConstants.LOG_SAME;

    /* Integer value of typeSourceStr */
    private int typeSourceInt;

    /* Directory for load files */
    private Path loadDir;

    /* Temporary file */
    private File workFile = null;

    /*
     * The record which have obstime beyond the end time will not be considered
     * by the program. Also the hour field of the end time is only considered,
     * meaning the mins and seconds are ignored. The end time have the format
     * YYYY-MM-DD HH:MM:SS Current time is used as an endTime by default.
     */
    private Date endTime;

    /* List of precipitation objects */
    private List<EdexPrecipTotal> listPrecipAgg = new ArrayList<>();

    private static final ThreadLocal<SimpleDateFormat> dbDateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat(
                    BuildHourlyConstants.DB_DATE_STRING);
            return sdf;
        }

    };

    private static final ThreadLocal<SimpleDateFormat> fileDateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat(
                    BuildHourlyConstants.FILE_DATE_STRING);
            return sdf;
        }

    };

    /**
     * Constructor
     * 
     * Perform_build_hourly: For example if the end time is 2005-06-25 08:15:50,
     * look back hours - 2, min percent filled - 0.75, -t PTYPE. The following
     * steps are performed. 1.Get the records from curpp where obstime between
     * 2005-06-25 07:00:00 and 2005-06-25 08:00:00 and dur < 1001 2.Aggregate
     * their values 3.Call function create_send_build_hourly_load_file 4.Perform
     * the above 3 steps for obstime between 2005-06-25 06:00:00 and 2005-06-25
     * 07:00:00
     * 
     */
    public BuildHourly() {
    }

    /**
     * Gets BuildHourly Arguments
     * 
     * @param lbWindow
     * @param percentFill
     * @param typeSourceStr
     * @throws ParseException
     * @throws AppsDefaultsPathException
     */
    public void getBuildHourlyArguments(int lbWindow, String percentFill,
            String typeSourceStr) throws ParseException,
            AppsDefaultsPathException {

        if (lbWindow > 0) {
            this.setLbWindow(lbWindow);
        } else {
            logger.debug("lbWindow is null. Will use default value: {}",
                    this.lbWindow);
        }

        if (percentFill == null) {
            logger.debug("minPercentFill is null. Will use default value: {}",
                    this.minPercentFill);
        }

        if (typeSourceStr == null) {
            logger.debug("typeSourceStr is null. Will use default value: {}",
                    this.getTypeSourceStr());
        } else {
            if (typeSourceStr == "PTYPE") {
                this.setTypeSourceInt(BuildHourlyConstants.INT_PTYPE);
                this.setTypeSourceStr(BuildHourlyConstants.PTYPE);
                this.logTypeSourceStr = BuildHourlyConstants.LOG_PTYPE;
            } else {
                this.setTypeSourceInt(BuildHourlyConstants.INT_SAME);
                /* SAME is the defult one */
                this.setTypeSourceStr(BuildHourlyConstants.SAME);
                this.logTypeSourceStr = BuildHourlyConstants.LOG_SAME;
            }
        }
        /*
         * If end_time is not given in cmd line or if the given end_time format
         * is not YYYY-MM-DD HH:MM:SS then current time will be taken as end
         * time
         */
        Date currentTime = TimeUtil.newDate();
        this.endTime = currentTime;

        loadDir = AppsDefaultsConversionWrapper
                .getPathForToken(BuildHourlyConstants.AppsDefaults.GAGE_PP_DATA);
        if (loadDir != null) {
            this.setLoadDir(loadDir);
        } else {
            logger.error("Token value for gage_pp_data is not available");
        }
        return;
    }

    /**
     * Starts by mpeBuildHourlyCron in mpe-periodic-triggered.xml
     * 
     * @throws IOException
     */
    public void runBuildHourly() throws IOException {

        try {
            logger.info("******************************");
            logger.info("Started Build Hourly Application...");
            logger.info("----------------------------------------------------------");
            // Get arguments
            getBuildHourlyArguments(this.lbWindow, null,
                    BuildHourlyConstants.PTYPE);
            Calendar endTimeCalendar = TimeUtil.newGmtCalendar();
            endTimeCalendar.setTime(this.getEndTime());
            TimeUtil.minCalendarFields(endTimeCalendar, Calendar.MILLISECOND);
            logger.info("Hostname [{}]", SystemUtil.getLocalAddress()
                    .getHostName());
            logger.info("Build Hourly Application is invoked with following arg: ");
            logger.info(
                    "DataBase Name: [{}]\tLook Back Hours: [{}]",
                    AppsDefaults.getInstance().getToken(
                            CommonHydroConstants.AppsDefaults.DB_NAME),
                    getLbWindow());
            logger.info("Minimum Percent To Be Filled: [{}]\tEnd Time: [{}]",
                    getMinPercentFill(),
                    dbDateFormat.get().format(this.getEndTime()));
            logger.info("Type Handling: {}", logTypeSourceStr);
            /*
             * If the current time has some secs or mins in it,get rid of them
             * and just consider only the hours
             */
            TimeUtil.minCalendarFields(endTimeCalendar, Calendar.MINUTE,
                    Calendar.SECOND, Calendar.MILLISECOND);
            Date rTime = endTimeCalendar.getTime();
            logger.info(
                    "Command line argument END_TIME rounded to the hour {}",
                    rTime.toString());
            logger.info(
                    "*** below means < {} percent of hourly period has data",
                    getMinPercentFill() * 100);

            CurPPDao curppDao = new CurPPDao();
            /* Build the hourly aggregate */
            for (int i = 0; i < getLbWindow(); i++) {
                long beginTime = rTime.getTime() - (i + 1)
                        * TimeUtil.MILLIS_PER_HOUR;
                long endT = rTime.getTime() - i * TimeUtil.MILLIS_PER_HOUR;
                endTimeCalendar.setTimeInMillis(beginTime);
                Date startTime = endTimeCalendar.getTime();
                endTimeCalendar.setTimeInMillis(endT);
                Date finishTime = endTimeCalendar.getTime();
                logger.info("*******************************************************");
                logger.info("Accumulation_Interval_Start_Time = {}",
                        startTime.toString());
                logger.info("Accumulation_Interval_Finish_Time = {}",
                        finishTime.toString());
                logger.info("Querying CurPP.");
                /*
                 * 1. Get Records from curPP table within time windows.
                 */

                List<Curpp> records = curppDao.getRecordList(startTime,
                        finishTime);
                if (records.isEmpty()) {
                    logger.info("No Records were found...");
                    continue;
                }
                logger.info("Number Of Records Retrieved = {}", records.size());
                /*
                 * 2. Aggregate record values.
                 */
                listPrecipAgg.clear();
                GetTotalPP gtp = new GetTotalPP();
                List<IRawTS> rawPPList = new ArrayList<>(records.size());
                Map<String, Integer> recNumbers = new HashMap<>();

                // Convert Curpp object to a Rawpp object and count records
                for (Curpp record : records) {
                    Rawpp rawPP = gtp.convertCurpp2Rawpp(record);
                    String li = rawPP.getLid();
                    if (recNumbers.containsKey(li)) {
                        recNumbers.put(li, recNumbers.get(li) + 1);
                    } else {
                        recNumbers.put(li, 1);
                    }
                    rawPPList.add(rawPP);
                }
                listPrecipAgg = gtp.getTotalPPRaw(null, rawPPList, startTime,
                        finishTime,
                        BuildHourlyConstants.REPORT_MISSING_BELOW_MIN_PERCENT,
                        this.minPercentFill, 0);
                // Loop through rawPPList data
                for (EdexPrecipTotal precipAgg : listPrecipAgg) {
                    if (this.getTypeSourceStr() == BuildHourlyConstants.PTYPE) {
                        precipAgg.setTs("P" + precipAgg.getTs().charAt(1));
                    }

                    if (precipAgg.getPercentFilled() >= this.minPercentFill) {
                        logger.info(
                                "{} {} {} val:{} minutes: {} num_recs: {}",
                                precipAgg.getLid(),
                                precipAgg.getPe(),
                                precipAgg.getTs(),
                                String.format("%1$,.3f", precipAgg.getTotal()),
                                String.format("%1$,.3f",
                                        precipAgg.getHoursCovered() * 60.0),
                                recNumbers.get(precipAgg.getLid()));
                        // create file only if filled % more than minimum
                        createBuildHourlyWorkFile(precipAgg, finishTime,
                                this.getLoadDir());
                    } else {
                        logger.info(
                                "{} {} {} val:{} minutes: {} num_recs: {} ***",
                                precipAgg.getLid(),
                                precipAgg.getPe(),
                                precipAgg.getTs(),
                                String.format("%1$,.3f", precipAgg.getTotal()),
                                String.format("%1$,.3f",
                                        precipAgg.getHoursCovered() * 60.0),
                                recNumbers.get(precipAgg.getLid()));
                    }
                }
            }
            /*
             * 3. Call function create_send_build_hourly_load_file. End of for
             * loop for time window slots depending on loop back hour.
             */
            createBuildHourlyLoadFile(this.getLoadDir());
        } catch (Exception e) {
            logger.error("Exiting since error occured...", e);
        }
        // THE END
        logger.info("--------------------------------------------------------");
        logger.info("Stopped Build Hourly Application...");
        logger.info("******************************");
    }

    /**
     * The load directory is where the load file will be created. Create a file
     * BUILD_HOURLY.work in that area. Write the record contents from precip_agg
     * into the file, which would be updated to hourlypp table by gage_pp
     * process. If cur percent filled < min percent filled then -9999.0 (VALUE
     * MISSING) will be written in the file. Rename this file into load file
     * BUILDHORLY.MMDD.YYYYHHMMSSss
     * 
     * @param precipAgg
     * @param endTime
     * @param loadDir
     * @return
     * @return
     */
    private void createBuildHourlyWorkFile(EdexPrecipTotal precipAgg,
            Date endTime, Path loadDir) {
        if (loadDir == null) {
            return;
        }
        /*
         * If percent filled >= threshold set then value to send is the precip
         * aggregate, else send missing value
         */
        double valueToLoad = precipAgg.getTotal();
        if (precipAgg.getPercentFilled() >= this.minPercentFill) {
            String productTime = dbDateFormat.get().format(
                    TimeUtil.newGmtCalendar().getTime());

            // File line %s|%s|%d|%s|%s|%s|%.3lf|%c|%d|%d|%s|%s|%s
            String fline = "";

            StringBuilder sb = new StringBuilder();
            sb.append(precipAgg.getLid());
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    precipAgg.getPe());
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(1001);
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    precipAgg.getTs());
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    BuildHourlyConstants.QCZ);
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    dbDateFormat.get().format(endTime));
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    String.format("%1$,.3f", valueToLoad));
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    precipAgg.getQc());
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    CommonHydroConstants.DEFAULT_QC_VALUE);
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(1);
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR).append(
                    BuildHourlyConstants.BUILDHOURLY);
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR)
                    .append(productTime);
            sb.append(BuildHourlyConstants.INLINE_SEPARATOR)
                    .append(productTime);

            fline = sb.toString();

            String fname = loadDir.toString() + File.separatorChar
                    + BuildHourlyConstants.WORKFILENAME
                    + BuildHourlyConstants.GPP_WORKFILE_SUFFIX;

            try (BufferedWriter buf = new BufferedWriter(new FileWriter(fname,
                    true))) {
                workFile = new File(fname);
                buf.write(fline + "\n");
                this.setWorkFile(workFile);

            } catch (IOException e) {
                logger.error("Error while opening work file " + fname + ".", e);
            }
        }
        return;
    }

    /**
     * Create BuildHourly Load File
     * 
     * @param loadDir
     * @return
     */
    private void createBuildHourlyLoadFile(Path loadDir) {
        if (loadDir == null) {
            return;
        }
        Date curTime = TimeUtil.newDate();
        String fileTimeStr = fileDateFormat.get().format(curTime);
        String loadFileName = BuildHourlyConstants.BUILDHOURLY + fileTimeStr;
        if (this.getWorkFile() != null && this.getWorkFile().exists()) {
            File loadFile = new File(loadDir.toString() + File.separatorChar
                    + loadFileName);
            if (this.getWorkFile().renameTo(loadFile)) {
                logger.info("Work file succesfuly renamed to {} .",
                        loadFileName);
            } else {
                logger.error(
                        "Rename failed. There was an error or work file {} was destroyed.",
                        this.getWorkFile().getName());
            }
        }
        return;
    }

    public int getLbWindow() {
        return lbWindow;
    }

    public float getMinPercentFill() {
        return minPercentFill;
    }

    public int getTypeSourceInt() {
        return typeSourceInt;
    }

    public void setLbWindow(int lbWindow) {
        this.lbWindow = lbWindow;
    }

    public void setMinPercentFill(float minPercentFill) {
        this.minPercentFill = minPercentFill;
    }

    public void setTypeSourceInt(int typeSourceInt) {
        this.typeSourceInt = typeSourceInt;
    }

    /**
     * @return the typeSourceStr
     */
    public String getTypeSourceStr() {
        return typeSourceStr;
    }

    /**
     * @param typeSourceStr
     *            the typeSourceStr to set
     */
    public void setTypeSourceStr(String typeSourceStr) {
        this.typeSourceStr = typeSourceStr;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the loadDir
     */
    public Path getLoadDir() {
        return loadDir;
    }

    /**
     * @param loadDir
     *            the loadDir to set
     */
    public void setLoadDir(Path loadDir) {
        this.loadDir = loadDir;
    }

    /**
     * @return the workFile
     */
    public File getWorkFile() {
        return workFile;
    }

    /**
     * @param workFile
     *            the workFile to set
     */
    public void setWorkFile(File workFile) {
        this.workFile = workFile;
    }

}
