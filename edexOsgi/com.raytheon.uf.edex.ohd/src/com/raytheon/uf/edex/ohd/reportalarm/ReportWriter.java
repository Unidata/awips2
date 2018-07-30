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
package com.raytheon.uf.edex.ohd.reportalarm;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.dataplugin.shef.tables.Counties;
import com.raytheon.uf.common.dataplugin.shef.tables.CountiesId;
import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.DatalimitsId;
import com.raytheon.uf.common.dataplugin.shef.tables.Descrip;
import com.raytheon.uf.common.dataplugin.shef.tables.Hsa;
import com.raytheon.uf.common.dataplugin.shef.tables.Location;
import com.raytheon.uf.common.dataplugin.shef.tables.Network;
import com.raytheon.uf.common.dataplugin.shef.tables.Rfc;
import com.raytheon.uf.common.dataplugin.shef.tables.State;
import com.raytheon.uf.common.dataplugin.shef.tables.Timezone;
import com.raytheon.uf.common.dataplugin.shef.tables.Wfo;
import com.raytheon.uf.common.dataplugin.shef.tables.Riverstat;
import com.raytheon.uf.common.dataplugin.shef.tables.Shefpe;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;


/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2011  9377      jnjanga      Initial creation
 * Jul 12, 2013  15711     wkwock       Fix verbose, observe mode, etc
 * Sep 05, 2013  16539     wkwock       Fix RECENT, NEAR_NOW,FRESH,and NEW_OR_INCREASED modes
 * Feb 13, 2014  #2783     dgilling     Refactored to support running as part
 *                                      of an EDEX service.
 * Oct 20, 2014  DIM#16799 deng         missing forecast report for FRESH mode
 * Oct 20, 2014  DIM#17605 deng         mismatch report format between A1 and A2
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */
class ReportWriter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReportWriter.class);

    private StringBuilder reportData;

    private ReportOptions opt;

    private int alarmCount = 0;

    private Date now;

    private Date startTime;

    private Date endTime;

    private String[] limitThreshold;

    /**
     * Constructor
     * 
     * @param opt
     * @param now
     */
    ReportWriter(ReportOptions opt, Date now) {
        this.reportData = new StringBuilder();
        this.limitThreshold = new String[] {"","","","","","","",""};
        this.opt = opt;
        this.now = now;
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MINUTE, opt.getMinutes());
        endTime = cal.getTime();
        cal.add(Calendar.MINUTE, opt.getMinutes() * (-2));
        startTime = cal.getTime();
    }

    public void generateReport(final AlertalarmRecord record) throws Exception {
        writeHeader();
        writeBody(record);
        if (opt.getVerbose()) {
            writeVerboseTrailer();
        } else {
            writeReportTrailer();
        }
    }

    /**
     * Processes and writes the data by groups found in this record.
     * 
     * @param record
     *            - the Alertalarm record for this run
     * @throws Exception
     */
    private void writeBody(AlertalarmRecord record) throws Exception {

        if (record != null) {
            Set<String> groups = record.getGroups();

            statusHandler.debug("      ---Groups---");
            statusHandler.debug(groups.toString());
            statusHandler.debug("      ---Record begin---");
            statusHandler.debug(record.toString());
            statusHandler.debug("      ---Record end---");

            for (String group : groups) {
                List<Alertalarmval> grpData = record.getGroupData(group);
                writeGroup(grpData);
            }
        }
    }

    /**
     * Writes the report's header information.
     */
    private void writeHeader() {

        write("***REPORT OF ALERT/ALARM DATA FROM THE HYDROLOGIC DATABASE***");
        writeNewline();
        writeNewline();

        // start to change for mismatch between A1 and A2, make sure the now is GMtime
        write("CREATED " + opt.getProductId() + " AT :  " + now.toString());
        writeNewline();
    }

    private void writeReportTrailer() {
        /* if no alarms found, then write message */

        if (alarmCount == 0) {
            writeln("\nNO ALERT/ALARM DATA TO REPORT FOR GIVEN REQUEST.\n");
        } else {
            String reportMode = opt.getMode().toString();
            if (reportMode.equals("")) {
                writeln("\n" + alarmCount + " ALERT/ALARMS REPORTED.");
            } else {
                writeln("\n" + alarmCount + " ALERT/ALARMS REPORTED. ("
                        + reportMode + " MODE)");
            }
        }

        writeln("\nEND OF REPORT");
    }

    /**
     * Writes the report's trailer information.
     */
    private void writeVerboseTrailer() {

        if (alarmCount == 0) {
            writeNewline();
            write("NO ALERT/ALARM DATA TO REPORT FOR GIVEN REQUEST.");
            writeNewline();
            writeNewline();
        } else {
            String reportMode = opt.getMode().toString();
            if (reportMode.equals("")) {
                writeln("\n" + alarmCount + " ALERT/ALARMS REPORTED.");
            } else {
                writeln("\n" + alarmCount + " ALERT/ALARMS REPORTED. ("
                        + reportMode + " MODE)");
            }
            writeln("\n-------------------------------------------------------------------");

            /* add to match A1 format*/

            write("DATA FILTER:  ");

            EnumSet<FilterOption> flags = opt.getFilter();
            if (CollectionUtil.isNullOrEmpty(flags)) {
                writeln("All alerts/alarms considered (i.e. no filter).");
            } else {
                write("Only considering ");
                if (flags.contains(FilterOption.OBSERVED)
                        && !flags.contains(FilterOption.FORECAST)) {
                    write(FilterOption.OBSERVED.name().toLowerCase());
                } else if (!flags.contains(FilterOption.OBSERVED)
                        && flags.contains(FilterOption.FORECAST)) {
                    write(FilterOption.FORECAST.name().toLowerCase());
                }

                if (flags.contains(FilterOption.RATE_OF_CHANGE)) {
                    write(" " + FilterOption.RATE_OF_CHANGE.name().toLowerCase());
                }

                if (flags.contains(FilterOption.UPPER_LIMIT)) {
                    write(" " + FilterOption.UPPER_LIMIT.name().toLowerCase());
                }
                if (flags.contains(FilterOption.LOWER_LIMIT)) {
                    write(" " + FilterOption.LOWER_LIMIT.name().toLowerCase());
                }
                if (flags.contains(FilterOption.DIFF_LIMIT)) {
                    write(" " + FilterOption.DIFF_LIMIT.name().toLowerCase());
                }

                if (flags.contains(FilterOption.ALERTS)
                        && !flags.contains(FilterOption.ALARMS)) {
                    write(" " + FilterOption.ALERTS.name().toLowerCase());
                } else if (!flags.contains(FilterOption.ALERTS)
                        && flags.contains(FilterOption.ALARMS)) {
                    write(" " + FilterOption.ALARMS.name().toLowerCase());
                } else {
                    write(" " + FilterOption.ALERTS.name().toLowerCase() + " and "
                            + FilterOption.ALARMS.name().toLowerCase());
                }

                writeNewline();
            }

            write("PE FILTER  : ");
            if (opt.getPEfilter() == null) {
                writeln(" All Physical Elements are considered (i.e. no filter).");

            } else {
                write(" Only considering ");
                writeln(opt.getPEfilter() + " physical element data");
            }

            if (opt.getMode() == ReportMode.RECENT
                    || opt.getMode() == ReportMode.NEAR_NOW
                    || opt.getMode() == ReportMode.FRESH
                    || opt.getMode() == ReportMode.NEW_OR_INCREASED) {
                write("NUM MINUTES: " + opt.getMinutes());
            }

            writeNewline();
            writeln("--------------------------------------------------------------------");

            writeln("Limits: shown above are the alert threshold/alarm threshold.");
            writeln("Info grouped by location, physical element, type-source and check type.");
            writeln("Upper, lower, diff and rate-of-change (roc) limits are shown for each group.");
            writeln("Columns shown are: threat type > level, time, value [details].");
            writeln("For forecast data, the time of the forecast is also given.");
            writeln("Threat types: ");
            writeln("              roc  =>value shown exceeded rate-of-change threshold");
            writeln("              lower<=value exceeded threshold");
            writeln("              upper=>value exceeded threshold");
            writeln("              diff=>value exceeded threshold\n");
        }

        // describe the report mode in somewhat verbose terms.
        switch (opt.getMode()) {
        case ALL:
            writeln("REPORT MODE: ALL");
            writeln("Listing all data.");
            break;
        case UNREPORTED:
            writeln("REPORT MODE: UNREPORTED");
            writeln("Listing all unreported records.");
            break;
        case RECENT:
            writeln("REPORT MODE: RECENT");
            writeln("Listing observed and forecast records posted within the");
            writeln("past " + opt.getMinutes() + " minutes");
            break;
        case NEAR_NOW:
            writeln("REPORT MODE: NEAR NOW");
            writeln("Listing observed value within the past "
                    + opt.getMinutes());
            writeln("and forecast value within the next " + opt.getMinutes()
                    + " .");
            break;
        case NEAREST:
            writeln("REPORT MODE: NEAREST");
            writeln("Listing most recent observed value and earliest forecast value.");
            break;
        case LATEST_MAXFCST:
            writeln("REPORT MODE: LATEST_MAXFCST");
            writeln("Listing most recent observed value and maximum forecast value.");
            break;
        case FRESH:
            writeln("REPORT MODE: FRESH");
            writeln("For observed data, listing all records that are later than "
                    + opt.getMinutes());
            writeln("minutes after the time of the most recent reported value. ");
            writeln("For forecast data, listing the maximum forecast value if it");
            writeln("was not reported within the past " + opt.getMinutes()
                    + " minutes.");
            break;
        case NEW_OR_INCREASED:
            writeln("REPORT MODE: NEW_OR_INCREASED");
            writeln("For observed data, listing unreported records if, the previous");
            writeln("reported was later than " + opt.getMinutes()
                    + " minutes ago OR if the report");
            writeln("has a higher value than the last reported record.");
            writeln("For forecasted data, listing the maximum forecast value if it");
            writeln("was not reported within the past " + opt.getMinutes()
                    + " minutes.");
            break;
        default:
            break;
        }

        writeNewline();
        writeln("END OF REPORT");
    }

    /**
     * Write the group data to the report
     * 
     * @param grpData
     *            - A list of Alertalarmval row data
     * @throws Exception
     */
    private void writeGroup(List<Alertalarmval> grpData) throws Exception {

    /* determine this grpData is observed or forecast, if forecast, need to reserve the order */

        char grpTs0 = grpData.get(0).getId().getTs().charAt(0);
        if (grpTs0 != 'R' && grpTs0 != 'P') {
            Collections.reverse(grpData);
        }
        if (hasDataToReport(grpData)) {
            /* write the alert-alarm group header */
            writeGroupHeader(grpData);
            /* generate alert-alarm report and update database */
            writeGroupReport(grpData);
        }
    }

    /**
     * Write info on the alert/alarm for the current group
     * 
     * If report mode is:
     * 
     * ALL - then show all records.
     * 
     * UNREPORTED - then still show all records knowing that the original
     * database retrieval limited the retrieval to just those that were
     * unreported.
     * 
     * NEAREST - then only show the first record, knowing that that is the
     * nearest one for this group.
     * 
     * NEAR_NOW - then still show all records knowing that the original database
     * retrieval limited the time window to the appropriate minutes. (validtime)
     * 
     * RECENT - then still show all records knowing that the original database
     * retrieval limited the time window to the appropriate minutes. (for
     * observed data: validtime, for forecast data:postingtime)
     * 
     * FRESH - then only show records not reported within xxx minutes for
     * observed data. for forecast data, report the data with the maximum value
     * if it is not reported within xxx minutes.
     * 
     * LATEST_MAXFCST - then show nearest data for the observed data; show the
     * data with the maximum value for the forecast data.
     * 
     * NEW_OR_INCREASED - show unreported data if the previous report is later
     * than xxx minutes ago, or if the report has a higher value than the last
     * reported.
     * 
     * updates alarmCount in the process
     * 
     * @param groupData
     * @throws Exception
     */
    private void writeGroupReport(List<Alertalarmval> grpData) throws Exception {

        Date posttime = null;
        double latestValue = -88888.;
        Boolean maxFcstActionTimeNullFlag = false;

        /*
         * processing starts with the first record for this group if processing
         * observed or processed data; otherwise reverse the order so that the
         * forecast data nearest the current time are considered first. this
         * special method is necessary because the alertalarm data are retrieved
         * all at once in descending time.
         */

        char grpTs0 = grpData.get(0).getId().getTs().charAt(0);

        /* consider the reverse order for forecast, move to after  modify 09/12/2014 */
        Alertalarmval maxfcst = findMaxfcst(grpData);
        Alertalarmval latestReport = findLatestAction(grpData);

        /*
         * now loop on the data for this group and write the data records
         * according to the report mode
         */

        switch (opt.getMode()) {
        /*
         * for report mode ALL and UNREPORTED -------------------------- for
         * these modes, simply report all the retrieved data since the SQL
         * filter did all the necessary filtering
         */
        case ALL:
        case UNREPORTED:
            for (Alertalarmval aav : grpData) {
                /* now write the info and update the database */
                writeAAval(aav);
                /*
                 * since this value is reported, then update the action_time in
                 * the database.
                 */
                updateDatabase(aav);
                alarmCount++;
            }
            break;

        case NEAREST:
            /*
             * for report mode NEAREST---------------------------------------
             * for this mode simply return the 'first' record, knowing that for
             * observed it is the latest, and for forecast it is the first.
             */
            writeAAval(grpData.get(0));
            updateDatabase(grpData.get(0));
            alarmCount++;
            break;

        case NEAR_NOW:
            for (Alertalarmval aav : grpData) {
                Date validtime = aav.getId().getValidtime();
                if ((grpTs0 == 'F' || grpTs0 == 'C')
                        && validtime.before(endTime)) {
                    writeAAval(aav);
                    updateDatabase(aav);
                    alarmCount++;
                }

                if ((grpTs0 == 'R' || grpTs0 == 'P')
                        && validtime.after(startTime)) {
                    writeAAval(aav);
                    updateDatabase(aav);
                    alarmCount++;
                }
            }
            break;

        case RECENT:
            for (Alertalarmval aav : grpData) {
                Date postingTime = aav.getPostingtime();
                if (postingTime.after(startTime)) {
                    writeAAval(aav);
                    updateDatabase(aav);
                    alarmCount++;
                }
            }
            break;

        case LATEST_MAXFCST:

            if (grpTs0 == 'R' || grpTs0 == 'P') {
                writeAAval(grpData.get(0));
                updateDatabase(grpData.get(0));
                alarmCount++;
            } else {
                if ((grpTs0 == 'F' || grpTs0 == 'C')) {
                    writeAAval(maxfcst);
                    updateDatabase(maxfcst);
                    alarmCount++;
                }
            }
            break;

        case FRESH:
            /*
             * get the latest action time reported, if there is one. for
             * forecast data, this is the action time of the maximum forecast
             * value
             */
            for (Alertalarmval aav : grpData) {
                if ((grpTs0 == 'R' || grpTs0 == 'P')) {
                    Date validtime = aav.getId().getValidtime();
                    Calendar cal = Calendar.getInstance();
                    if (latestReport != null) {
                        cal.setTime(latestReport.getId().getValidtime());
                    }
                    cal.add(Calendar.MINUTE, opt.getMinutes());
                    if (latestReport == null || validtime.after(cal.getTime())) {
                        writeAAval(aav);
                        updateDatabase(aav);
                        alarmCount++;
                    }
                }
            }
            if (grpTs0 == 'F' || grpTs0 == 'C') {
                if (maxfcst != null) {

                   if (isNull(maxfcst.getActionTime().getTime())) {
                       maxFcstActionTimeNullFlag = true;
                    }
                    else
                       maxFcstActionTimeNullFlag = false;

                    if (maxFcstActionTimeNullFlag == true || maxfcst.getActionTime().before(startTime)) {
                        writeAAval(maxfcst);
                        updateDatabase(maxfcst);
                        alarmCount++;
                    }
                }
            }

            break;

        case NEW_OR_INCREASED:
            Calendar cal = Calendar.getInstance();
            for (Alertalarmval aav : grpData) {
                if (latestReport != null) {
                    latestValue = latestReport.getValue();
                    cal.setTime(latestReport.getPostingtime());
                } else {
                    cal.setTimeInMillis(0);
                }

                if (isNull(aav.getActionTime().getTime()))
                {
                    posttime = aav.getPostingtime();
                    /*
                     * if the record was posted after the last posted report
                     * plus XX minutes (i.e the report is new), or if the record
                     * has a higher value than the last posted record's value
                     * (i.e. the report is 'increased'), then report it.
                     */

                    cal.add(Calendar.MINUTE, opt.getMinutes());

                    if (posttime.after(cal.getTime())
                            || (aav.getValue() > latestValue)) {
                        writeAAval(aav);
                        updateDatabase(aav);
                        alarmCount++;
                    }
                }
            }

            break;

        default:
            break;
        }

    }

    /**
     * Checks whether this group(lid-pe-ts-aa_check) has any row data that
     * exceeds specified alert/alarm limits. The checks depends on the modes.
     * 
     * @param grpData
     * 
     * @return - True if at least one row data satisfies the alert/alarm
     *         situation. - false if none.
     */
    private boolean hasDataToReport(List<Alertalarmval> grpData) {
        if (grpData == null || grpData.isEmpty()) {
            return false;
        }

        // These following modes are guaranteed to
        // use at least one value from the retrieved data.
        if (opt.getMode() == ReportMode.ALL
                || opt.getMode() == ReportMode.UNREPORTED
                || opt.getMode() == ReportMode.NEAREST
                || opt.getMode() == ReportMode.LATEST_MAXFCST) {
            return true;
        }

        Date posttime = null;
        double latestValue = -88888.;

        Alertalarmval maxfcstVal = findMaxfcst(grpData);
        Alertalarmval latestReport = findLatestAction(grpData);

        // The following mode are more restrictive
        for (Alertalarmval aav : grpData) {
            char ts0 = aav.getId().getTs().charAt(0);
            Date validTime = aav.getId().getValidtime();
            Date postingTime = aav.getPostingtime();
            switch (opt.getMode()) {
            case NEAR_NOW:
                if ((ts0 == 'F' || ts0 == 'C') && validTime.before(endTime)) {
                    return true;
                }
                if ((ts0 == 'R' || ts0 == 'P') && validTime.after(startTime)) {
                    return true;
                }
            case RECENT:
                if (postingTime.after(startTime)) {
                    return true;
                }
            case FRESH:
                /*
                 * get the latest action time reported, if there is one. for
                 * forecast data, this is the action time of the maximum
                 * forecast value
                 */

                if ((ts0 == 'R' || ts0 == 'P')) {
                    if (latestReport == null) {
                        return true;
                    }

                    Date validtime = aav.getId().getValidtime();
                    Calendar cal = Calendar.getInstance();
                    cal.setTime(latestReport.getId().getValidtime());
                    cal.add(Calendar.MINUTE, opt.getMinutes());
                    if (validtime.after(cal.getTime())) {
                        return true;
                    }
                }

                if (ts0 == 'F' || ts0 == 'C') {
                    if (maxfcstVal != null
                            && isNotNull(maxfcstVal.getActionTime().getTime())) {
                        Date latestActiondate = maxfcstVal.getActionTime();
                        if (latestActiondate.before(startTime)) {
                            return true;
                        }
                    }
                    else if (maxfcstVal != null && isNull(maxfcstVal.getActionTime().getTime())) {
                        return true;
                    }
                }
                break;

            case NEW_OR_INCREASED:
                /* get the last reported record and its time and value. */
                Calendar cal = Calendar.getInstance();
                if (latestReport != null) {
                    latestValue = latestReport.getValue();
                    cal.setTime(latestReport.getPostingtime());
                } else {
                    cal.setTimeInMillis(0);
                }

                if (isNull(aav.getActionTime().getTime()))
                {
                    posttime = aav.getPostingtime();
                    /*
                     * if the record was posted after the last posted report
                     * plus XX minutes (i.e the report is new), or if the record
                     * has a higher value than the last posted record's value
                     * (i.e. the report is 'increased'), then report it.
                     */

                    cal.add(Calendar.MINUTE, opt.getMinutes());

                    if (posttime.after(cal.getTime()))  {
                        return true;
                    } else if (aav.getValue() > latestValue) {
                        return true;
                    }
                }
                break;

            default:
                break;
            }
        }
        return false;
    }

    /**
     * write out to the report file this alertalarmval row data
     * 
     * @param aav
     * @throws Exception
     */
    private void writeAAval(Alertalarmval aav) throws Exception {
        String[] devb = buildString(aav);

        String durInfo = devb[0];
        String exInfo = devb[1];
        String validInfo = devb[2];
        String tsInfo = devb[3];
        String peInfo = devb[4];
        String line = null;
        String fmtSpecifier = null;

        ArrayList<Object> args = new ArrayList<Object>();
        args.add(tsInfo);
        args.add(peInfo);
        args.add(aav.getValue());
        args.add(validInfo);
      /*  args.add(aav.getId().getAaCheck());
        args.add(aav.getId().getAaCateg());*/

        if (aav.getId().getAaCheck().equals(ShefConstants.UPPER_CHECKSTR)) {
              args.add(aav.getId().getAaCheck());
              args.add(aav.getId().getAaCateg());
            if (aav.getId().getAaCateg().equals(ShefConstants.ALARM_CATEGSTR)) {
                args.add(limitThreshold[1]);
            } else {
                args.add(limitThreshold[0]);
            }

            fmtSpecifier = "%s %s %7.1f at %s > %s %s %s";
        } else if (aav.getId().getAaCheck()
                .equals(ShefConstants.LOWER_CHECKSTR)) {
            args.add(aav.getId().getAaCheck());
            args.add(aav.getId().getAaCateg());

           if (aav.getId().getAaCateg().equals(ShefConstants.ALARM_CATEGSTR)) {
                args.add(limitThreshold[3]);
            } else {
                args.add(limitThreshold[2]);
            }

            fmtSpecifier = "%s %s %7.1f at %s < %s %s %s";
        } else  if (aav.getId().getAaCheck().equals(ShefConstants.ROC_CHECKSTR)){
                args.add(4, aav.getSupplValue());
                args.add(aav.getId().getAaCateg());
            if (aav.getId().getAaCateg().equals(ShefConstants.ALARM_CATEGSTR)) {
                args.add(limitThreshold[5]);
            } else {
                args.add(limitThreshold[4]);
            }

            fmtSpecifier = "%s %s %7.1f at %s Chg=%7.1f/hr > %s %s/hr";

        } else {
            args.add(0,"Obs");
            args.add(1,peInfo);
            args.add(2, aav.getSupplValue());
            args.add(3,aav.getValue());
            args.add(4, validInfo);
            args.add(5, aav.getId().getAaCheck());
            args.add(6, aav.getId().getAaCateg());

            if (aav.getId().getAaCateg().equals(ShefConstants.ALARM_CATEGSTR)) {
                args.add(7, limitThreshold[7]);
            } else {
                args.add(7, limitThreshold[6]);
            }
            fmtSpecifier = "%s %s %7.1f  Fcst %7.1f at %s > %s %s %s";

        }

        statusHandler.debug("fmt=" + fmtSpecifier);
        statusHandler.debug("args=" + args.toString());

        line = String.format(fmtSpecifier, args.toArray());
        write(line);

        if (durInfo.length() > 0 || exInfo.length() > 0) {
            writeln("(" + "dur=" + durInfo + Constants.SPACE + "extr=" + exInfo + Constants.SPACE + ")");
        }
        writeNewline();

    }

    /**
     * build a presentable string for duration, extremum code and convert valid
     * time to time_t format.
     * 
     * @param aav
     * @throws Exception
     */
    private String[] buildString(Alertalarmval aav) throws Exception {
        String[] devbStr = new String[5];

        /* build a presentable string for the duration code value */
        short dur = aav.getId().getDur();
        if (dur != 0) {
            Object[] durData = getShefDurInfo(dur);
            if (durData == null) {
                devbStr[0] = Short.toString(dur);
            } else {
               Object[] aDurData = (Object[]) durData[0];
                devbStr[0] = (String) aDurData[1];
            }

        } else {
            devbStr[0] = "";
        }

        /* build a presentable string for the extremum code value */
        String ex = aav.getId().getExtremum();
        if (!ex.equals("Z")) {
            Object[] exData = getShefExInfo(ex);
            if (exData == null) {
                devbStr[1] = ex;
            } else {
                Object[] aExData = (Object[]) exData[0];
                devbStr[1] = (String) aExData[0];
            }
        } else {
            devbStr[1] = "";
        }

        /*
         * convert the valid time for use in the update statement and for
         * presenting the time in the output
         */
        String validTimeStr = aav.getId().getValidtime().toString();
        devbStr[2] = validTimeStr.substring(0,3) + "-" + validTimeStr.substring(4, 7) +
                     validTimeStr.substring(8, 10) +"-" + validTimeStr.substring(11, 16);

        /*
         * if forecast or contingency data, then show "Fcst", otherwise show "Obs"
         */

        char ts0 = aav.getId().getTs().charAt(0);
        if (ts0 == 'F' || ts0 == 'C') {
            devbStr[3] = "Fcst";
        } else {
            devbStr[3] = "Obs";
        }

        /* build the pe name string */

        Shefpe peinfo = getShefPeInfo(aav.getId().getPe());

        String peNameStr = null;

          if (peinfo == null) {
              peNameStr = "UndefinedName";
          }
          else {
              peNameStr = peinfo.getName();
          }

          devbStr[4] = peNameStr;

        return devbStr;
    }

    /**
     * Writes the group header information
     * 
     * @param headerTokens
     * 
     * @param grpData
     * @throws Exception
     */
    private void writeGroupHeader(List<Alertalarmval> grpData) throws Exception {
        // get the location info for this group
        String lid = grpData.get(0).getId().getLid();
        Location loc = getLocationInfo(lid);

        State state = getStateInfo(loc.getCounties().getId().getState());

        // get stream name from RiverStat table
        Riverstat rs = getRiverStatInfo(lid);
        String streamStr = null;
        if (rs == null || rs.getStream() == null)
            streamStr = "";
        else
            streamStr = rs.getStream() + Constants.SPACE;

        //get proximity field from Descrip table
        String proximityStr = getDescripInfo(lid);

        // get a description for this physical element
        String pe = grpData.get(0).getId().getPe();
        Shefpe peinfo = getShefPeInfo(pe);

      //  String peNameStr = null;
        String engunitStr = null;

        if (peinfo == null) {
        //    peNameStr = "UndefinedName";
            pe = "UndefinedName";
            engunitStr = "";
        }
        else {
          //  peNameStr = peinfo.getName();
            engunitStr = Constants.SPACE + peinfo.getEngUnit();
        }


        // make a description of the type portion of the type-source field

        String ts = grpData.get(0).getId().getTs();

        String typeInfo = null;
        String ts1StChr = ts.substring(0, 1).toUpperCase();
        if (ts1StChr.equals("C")) {
            typeInfo = "Contingengy";
        } else if (ts1StChr.equals("F")) {
            typeInfo = "Forecast";
        } else if (ts1StChr.equals("P")) {
            typeInfo = "Processed";
        } else {
            typeInfo = "Observed";
        }

        // write header lines to the file for this group
        writeNewline();

        /* match A1 latest format */

        if (!streamStr.equalsIgnoreCase("") && proximityStr != null)
            write(streamStr + proximityStr + loc.getName()  + Constants.SPACE +
                "(" + loc.getLid() + ")"
                + Constants.SPACE + loc.getCounties().getId().getCounty()
                + " County," + Constants.SPACE + state.getState() + Constants.SPACE + "(" +
                pe + Constants.SPACE + ts + engunitStr + ")");
        else if (loc.getName() != null)
            write(loc.getName()  + Constants.SPACE +
                    "(" + loc.getLid() + ")"
                    + Constants.SPACE + loc.getCounties().getId().getCounty()
                    + " County," + Constants.SPACE + state.getState() + Constants.SPACE + "(" +
                    pe + Constants.SPACE + ts + engunitStr + ")");
        else
            write("(" + loc.getLid() + ")"
                    + Constants.SPACE + loc.getCounties().getId().getCounty()
                    + " County," + Constants.SPACE + state.getState() + Constants.SPACE + "(" +
                    pe + Constants.SPACE + ts + engunitStr + ")");

        writeNewline();

        Datalimits limits = getDatalimits(grpData);

        if (limits != null) {

            StringBuilder lim = new StringBuilder("Limits: Upper ");
            if (limits.getAlertUpperLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlertUpperLimit()));
                limitThreshold[0] = Double.toString(limits.getAlertUpperLimit());
            } else {
                lim.append("undef");
                limitThreshold[0] = "-";
            }
            lim.append("/");
            if (limits.getAlarmUpperLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlarmUpperLimit()));
                limitThreshold[1] = Double.toString(limits.getAlarmUpperLimit());
            } else {
                lim.append("undef");
                limitThreshold[1] = "-";
            }

            lim.append("; Lower ");
            if (limits.getAlertLowerLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlertLowerLimit()));
                limitThreshold[2] = Double.toString(limits.getAlertLowerLimit());
            } else {
                lim.append("undef");
                limitThreshold[2] = "-";
            }
            lim.append("/");
            if (limits.getAlarmLowerLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlarmLowerLimit()));
                limitThreshold[3] = Double.toString(limits.getAlarmLowerLimit());
            } else {
                lim.append("undef");
                limitThreshold[3] = "-";
            }

            lim.append("; Chg ");
            if (limits.getAlertRocLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlertRocLimit()));
                limitThreshold[4] = Double.toString(limits.getAlertRocLimit());
            } else {
                lim.append("undef");
                limitThreshold[4] = "-";
            }
            lim.append("/");
            if (limits.getAlarmRocLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlarmRocLimit()));
                limitThreshold[5] = Double.toString(limits.getAlarmRocLimit());
            } else {
                lim.append("undef");
                limitThreshold[5] = "-";
            }

            lim.append("; ObsFcstDiff ");
            if (limits.getAlertDiffLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlertDiffLimit()));
                limitThreshold[6] = Double.toString(limits.getAlertDiffLimit());
            } else {
                lim.append("undef");
                limitThreshold[6] = "-";
            }
            lim.append("/");
            if (limits.getAlarmDiffLimit() != Constants.MISSING_VALUE_DOUBLE) {
                lim.append(String.format("%.1f", limits.getAlarmDiffLimit()));
                limitThreshold[7] = Double.toString(limits.getAlarmDiffLimit());
            } else {
                lim.append("undef");
                limitThreshold[7] = "-";
            }


            if (opt.getVerbose()) {
                writeln(lim.toString());
            }

        } else {
            statusHandler
                    .info("No data limits found in Database while writing alert/alarm group report!");
            writeln("Alert/Alarm limits not available.");
        }
    }

    /**
     * Find the most recent record which was reported already. The record with
     * the most recent action time is is returned.
     * 
     * @param grpData
     * 
     * @return
     */
    private Alertalarmval findLatestAction(List<Alertalarmval> grpData) {
        TreeSet<Alertalarmval> actions = new TreeSet<Alertalarmval>(
                new ActiontimeComparator());
        for (Alertalarmval aav : grpData) {
            if (isNotNull(aav.getActionTime().getTime())) {
                actions.add(aav);
            }
        }
        return actions.isEmpty() ? null : actions.first();
    }

    class ActiontimeComparator implements Comparator<Alertalarmval> {
        @Override
        public int compare(Alertalarmval aav1, Alertalarmval aav2) {
            Long actTime1 = aav1.getActionTime().getTime();
            Long actTime2 = aav2.getActionTime().getTime();
            return -actTime1.compareTo(actTime2);
        }
    }

    class FcstvalueComparator implements Comparator<Alertalarmval> {
        @Override
        public int compare(Alertalarmval aav1, Alertalarmval aav2) {
            Double val1 = aav1.getValue();
            Double val2 = aav2.getValue();
            return -val1.compareTo(val2);
        }
    }

    /**
     * Find the record in the forecast combination group with the maximum value.
     * This function should not return with a null pointer; i.e. if there are
     * data, there should always be a maximum value. This function is used for
     * the FRESH mode and LATEST_MAXFCST mode.
     * 
     * @param grpData
     * 
     * @return
     */
    private Alertalarmval findMaxfcst(List<Alertalarmval> grpData) {
        if (grpData == null || grpData.isEmpty()) {
            return null;
        }

        Alertalarmval maxfsct = grpData.get(0);
        TreeSet<Alertalarmval> fcstvalues = new TreeSet<Alertalarmval>(
                new FcstvalueComparator());
        /* only process the data if it is forecast type data */
        char ts0 = maxfsct.getId().getTs().charAt(0);
        if (ts0 == 'F' || ts0 == 'C') {
            for (Alertalarmval aav : grpData) {
                if (isNotNull(aav.getValue())) {
                    fcstvalues.add(aav);
                }
            }
            maxfsct = fcstvalues.first();

        }
        return fcstvalues.isEmpty() ? null : fcstvalues.first();
    }

    /**
     * Update the action time to now. Note that because certain report modes
     * include data that has already been reported, it is possible that the
     * action_time is not null; in this case, the action_time field will show
     * the last time the record was reported.
     * 
     * @throws Exception
     */
    private void updateDatabase(Alertalarmval aav) throws Exception {
        String nowAnsi = TimeUtil.formatToSqlTimestamp(now);
        String nowToAction = nowAnsi.substring(0, 19);
        String validAnsi = TimeUtil.formatToSqlTimestamp(aav.getId()
                .getValidtime());
        String basisAnsi = TimeUtil.formatToSqlTimestamp(aav.getId()
                .getBasistime());

        StringBuilder query = new StringBuilder();
        query.append(" UPDATE alertalarmval SET action_time ='");  
        query.append(nowToAction + "' ");
        query.append("WHERE lid='" + aav.getId().getLid() + "' ");
        query.append("AND pe='" + aav.getId().getPe() + "' ");
        query.append("AND dur=" + aav.getId().getDur() + Constants.SPACE);
        query.append("AND ts='" + aav.getId().getTs() + "' ");
        query.append("AND extremum='" + aav.getId().getExtremum() + "' ");
        query.append("AND probability=" + aav.getId().getProbability()
                + Constants.SPACE);
        query.append("AND validtime='" + validAnsi + "' ");
        query.append("AND basistime='" + basisAnsi + "' ");
        query.append("AND aa_categ='" + aav.getId().getAaCateg() + "' ");
        query.append("AND aa_check='" + aav.getId().getAaCheck() + "' ");
        try {
            CoreDao dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            dao.executeSQLUpdate(query.toString());

        } catch (Exception e) {
            statusHandler.error("Error updating alertalarmval table");
            statusHandler.error("Query = [" + query + "]");
            throw e;
        }
    }

    /**
     * Query the database and obtain the data limits for this group.
     * 
     * @param grpData
     * 
     * @return
     * @throws Exception
     */
    private Datalimits getDatalimits(List<Alertalarmval> grpData)
            throws Exception {
        Object[] limData = null;
        Object[] limRow = null;
        CoreDao dao = null;
        boolean dateWithin = false;
        boolean locRangeFound = false;
        DatalimitsId limitsId = null;
        Datalimits limits = null;
        String lid = grpData.get(0).getId().getLid();
        String pe = grpData.get(0).getId().getPe();
        Short dur = grpData.get(0).getId().getDur();
        Date validtime = grpData.get(0).getId().getValidtime();

        String query = "SELECT * FROM locdatalimits WHERE lid='" + lid
                + "' AND pe='" + pe + "' AND dur=" + dur;

        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            limData = dao.executeSQLQuery(query);

            if (limData != null && limData.length > 0) {
                for (Object lr : limData) {
                    limRow = (Object[]) lr;
                    String mds = (String) limRow[3];
                    String mde = (String) limRow[4];
                    limits = new Datalimits();
                    limitsId = new DatalimitsId(pe, dur, mds);
                    limits.setId(limitsId);
                    flushDataLimitsObj(limits);
                    dateWithin = checkDateRange(validtime, mds, mde);
                    if (dateWithin) {
                        locRangeFound = true;
                        copyThresholds(limits, limRow, locRangeFound);
                        break;
                    }
                }
            }

            if (!locRangeFound) {
                query = "SELECT * FROM datalimits WHERE pe='" + pe
                            + "' AND dur=" + dur;

                limData = dao.executeSQLQuery(query);
                if (limData != null && limData.length > 0) {
                    for (Object lr : limData) {
                        limRow = (Object[]) lr;
                        String mds = (String) limRow[2];
                        String mde = (String) limRow[3];
                        limits = new Datalimits();
                        limitsId = new DatalimitsId(pe, dur, mds);
                        limits.setId(limitsId);
                        flushDataLimitsObj(limits);
                        dateWithin = checkDateRange(validtime, mds, mde);
                        if (dateWithin) {
                            copyThresholds(limits, limRow, locRangeFound);
                            break;
                        }
                    }
                }
            }

        } catch (Exception e) {
            statusHandler.error("- PostgresSQL error executing Query = ["
                    + query + "]");
            throw e;
        }

        return limits;
    }

    /**
     * copy only the thresholds themselves into the returned record. check for
     * nulls always
     */
    private void copyThresholds(Datalimits limits, Object[] limitsRow,
            boolean locRangeFound) {

        Double grmin;
        Double grmax;
        Double rrmin;
        Double rrmax;
        Double rocmax;
        Double alertul;
        Double alertrl;
        Double alarmul;
        Double alarmrl;
        Double alertll;
        Double alarmll;
        Double alertdl;
        Double alarmdl;

        // traverse the limitsRow and substitute all nulls with
        // the default nullDouble value
        for (int i = 0; i < limitsRow.length; i++) {
            if (limitsRow[i] == null) {
                limitsRow[i] = getNullDouble();
            }
        }

        if (locRangeFound) { // copy from locdatalimits resultset

            grmin = (Double) limitsRow[5];
            grmax = (Double) limitsRow[6];
            rrmin = (Double) limitsRow[7];
            rrmax = (Double) limitsRow[8];
            rocmax = (Double) limitsRow[9];
            alertul = (Double) limitsRow[10];
            alertrl = (Double) limitsRow[11];
            alarmul = (Double) limitsRow[12];
            alarmrl = (Double) limitsRow[13];
            alertll = (Double) limitsRow[14];
            alarmll = (Double) limitsRow[15];
            alertdl = (Double) limitsRow[16];
            alarmdl = (Double) limitsRow[17];

        } else { // copy from datalimits resultset

            grmin = (Double) limitsRow[4];
            grmax = (Double) limitsRow[5];
            rrmin = (Double) limitsRow[6];
            rrmax = (Double) limitsRow[7];
            rocmax = (Double) limitsRow[8];
            alertul = (Double) limitsRow[9];
            alertrl = (Double) limitsRow[10];
            alarmul = (Double) limitsRow[11];
            alarmrl = (Double) limitsRow[12];
            alertll = (Double) limitsRow[13];
            alarmll = (Double) limitsRow[14];
            alertdl = (Double) limitsRow[15];
            alarmdl = (Double) limitsRow[16];
        }

        if (isNotNull(grmin)) {
            limits.setGrossRangeMin(grmin);
        }

        if (isNotNull(grmax)) {
            limits.setGrossRangeMax(grmax);
        }

        if (isNotNull(rrmin)) {
            limits.setReasonRangeMin(rrmin);
        }

        if (isNotNull(rrmax)) {
            limits.setReasonRangeMax(rrmax);
        }

        if (isNotNull(rocmax)) {
            limits.setRocMax(rocmax);
        }

        if (isNotNull(alertul)) {
            limits.setAlertUpperLimit(alertul);
        }

        if (isNotNull(alertll)) {
            limits.setAlertLowerLimit(alertll);
        }

        if (isNotNull(alertdl)) {
            limits.setAlertDiffLimit(alertdl);
        }

        if (isNotNull(alertrl)) {
            limits.setAlertRocLimit(alertrl);
        }

        if (isNotNull(alarmul)) {
            limits.setAlarmUpperLimit(alarmul);
        }

        if (isNotNull(alarmll)) {
            limits.setAlarmLowerLimit(alarmll);
        }

        if (isNotNull(alarmdl)) {
            limits.setAlarmDiffLimit(alarmdl);
        }

        if (isNotNull(alarmrl)) {
            limits.setAlarmRocLimit(alarmrl);
        }
    }

    /**
     * initializes all Double fields to the missing value
     * 
     * @throws Exception
     */
    private void flushDataLimitsObj(Datalimits limits) throws Exception {
        try {
            Class<?> cls = Class
                    .forName("com.raytheon.uf.common.dataplugin.shef.tables.Datalimits");
            Method methodlist[] = cls.getDeclaredMethods();
            for (Method method : methodlist) {
                if ((method.getName().startsWith("set") && !method.getName()
                        .contains("Id"))
                        && (method.getName().contains("Max")
                                || method.getName().contains("Min") || method
                                .getName().contains("Limit"))) {
                    method.invoke(limits, Constants.MISSING_VALUE_DOUBLE);
                }
            }
        } catch (Exception e) {
            statusHandler.error("Failed to flush Datalimits object.");
            throw e;
        }
    }

    public static double getNullDouble() {
        return -Double.MAX_VALUE;
    }

    public static long getNullLong() {
        return Long.MIN_VALUE;
    }

    public static boolean isNull(double value) {
        boolean result = false;

        if (value == getNullDouble()) {
            result = true;
        }
        return result;
    }

    public static boolean isNull(long value) {
        boolean result = false;

        if (value == getNullLong() || value == 0) {
            result = true;
        }

        return result;
    }

    public static boolean isNotNull(long value) {
        return !isNull(value);
    }

    public static boolean isNotNull(double value) {
        return !isNull(value);
    }

    /**
     * Check that the observation date is within the window limits found in the
     * db
     * 
     * @return
     */
    private boolean checkDateRange(Date obstime, String monthdaystart,
            String monthdayend) {
        String[] mds = monthdaystart.split("-");
        String[] mde = monthdayend.split("-");
        int mdsMonth = Integer.valueOf(mds[0]);
        int mdsDay = Integer.valueOf(mds[1]);
        int mdeMonth = Integer.valueOf(mde[0]);
        int mdeDay = Integer.valueOf(mde[1]);
        Calendar obs = Calendar.getInstance();
        obs.setTime(obstime);
        boolean monthWithin = obs.get(Calendar.MONTH) >= mdsMonth
                && obs.get(Calendar.MONTH) <= mdeMonth;
        boolean dayWithin = obs.get(Calendar.DAY_OF_MONTH) >= mdsDay
                && obs.get(Calendar.DAY_OF_MONTH) <= mdeDay;
        return (monthWithin && dayWithin) ? true : false;
    }

    /**
     * returns the report text as a string
     * 
     * @return
     */
    public String getReportData() {
        return reportData.toString();
    }

    /**
     * Query the location table and obtain a location rset for this location Id
     * 
     * @param lid
     *            - the location Id
     * 
     * @return - a location row
     * @throws Exception
     */
    private Location getLocationInfo(String lid) throws Exception {
        Object[] locData = null;
        CoreDao dao = null;
        Object[] locInfo = null;
        final String query = "SELECT * FROM location WHERE lid='" + lid + "'";
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            locData = dao.executeSQLQuery(query);
            if (locData != null && locData.length > 0) {
                locInfo = (Object[]) locData[0];
            }
        } catch (Exception e) {
            statusHandler
                    .error(" - PostgresSQL error retrieving location info for "
                            + lid);
            statusHandler.error("Query = [" + query + "]");
            throw e;
        }

        Location loc = new Location();
        loc.setLid((String) locInfo[0]);
        String county = (String) locInfo[1];
        String state = (String) locInfo[19];
        CountiesId ctyId = new CountiesId(county, state);
        Counties cty = new Counties();
        cty.setId(ctyId);
        loc.setCounties(cty);
        loc.setCoe((String) locInfo[2]);
        loc.setCpm((String) locInfo[3]);
        loc.setDetail((String) locInfo[4]);
        loc.setElev((Double) locInfo[5]);
        loc.setHdatum((String) locInfo[6]);
        loc.setHsa(new Hsa((String) locInfo[7]));
        loc.setHu((String) locInfo[8]);
        loc.setLat((Double) locInfo[9]);
        loc.setLon((Double) locInfo[10]);
        loc.setLremark((String) locInfo[11]);
        loc.setLrevise((Date) locInfo[12]);
        loc.setName((String) locInfo[13]);
        loc.setNetwork(new Network((String) locInfo[14]));
        loc.setRb((String) locInfo[15]);
        loc.setRfc(new Rfc((String) locInfo[16]));
        loc.setSbd((Date) locInfo[17]);
        loc.setSn((String) locInfo[18]);

        loc.setWaro((String) locInfo[20]);
        loc.setWfo(new Wfo((String) locInfo[21]));
        loc.setWsfo((String) locInfo[22]);
        loc.setType((String) locInfo[23]);
        loc.setDes((String) locInfo[24]);
        loc.setDet((String) locInfo[25]);
        loc.setPost((Integer) locInfo[26]);
        loc.setStntype((String) locInfo[27]);

        String tz = (String) locInfo[28];
        Timezone tzone = new Timezone(tz);
        loc.setTimezone(tzone);
        return loc;
    }

    /**
     * Query the shefex table and obtain shefex rset for the given duration
     * 
     * @param ex
     *            - the extremum
     * 
     * @return - a shefex row
     * @throws Exception
     */
    private Object[] getShefExInfo(String ex) throws Exception {
        Object[] exData = null;
        CoreDao dao = null;
        final String query = "SELECT * FROM shefex WHERE extremum='" + ex + "'";
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            exData = dao.executeSQLQuery(query);
        } catch (Exception e) {
            statusHandler
                    .error(" - PostgresSQL error retrieving Shefex info for extremum"
                            + ex);
            statusHandler.error("Query = [" + query + "]");
            throw e;
        }

        return exData;
    }

    /**
     * Query the shefdur table and obtain shefdur rset for the given duration
     * 
     * @param ex
     *            - the duration
     * 
     * @return - a shefdur row
     * @throws Exception
     */
    private Object[] getShefDurInfo(short dur) throws Exception {
        Object[] durData = null;
        CoreDao dao = null;
        final String query = "SELECT * FROM shefdur WHERE dur=" + dur;
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            durData = dao.executeSQLQuery(query);
        } catch (Exception e) {
            statusHandler
                    .error(" - PostgresSQL error retrieving ShefDur info for duration"
                            + dur);
            statusHandler.error("Query = [" + query + "]");
            throw e;
        }

        return durData;
    }

    /**
     * Query the shefpe table and obtain shefpe rset for the given physical
     * element
     * 
     * @param pe
     *            - the physical element
     * 
     * @return - a shefpe row
     * @throws Exception
     */
    private Shefpe getShefPeInfo(String pe) throws Exception {
        Object[] peData = null;
        CoreDao dao = null;
        Object[] peInfo = null;
        final String query = "SELECT * FROM shefpe WHERE pe='" + pe + "'";
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            peData = dao.executeSQLQuery(query);
            if (peData != null && peData.length > 0) {
                peInfo = (Object[]) peData[0];
            }
        } catch (Exception e) {
            statusHandler
                    .error(" - PostgresSQL error retrieving physical element info for "
                            + pe);
            statusHandler.error("Query = [" + query + "]");
            throw e;
        }

        /*if (peInfo == null) {
            return "UndefinedName";
        }*/

        Shefpe peRecord = new Shefpe();

        if (peInfo != null) {
            peRecord.setName((String) peInfo[1]);
            peRecord.setEngUnit((String) peInfo[2]);
        } else {
            peRecord = null;
        }


        return peRecord;
    }

    /**
     * Query the
     * 
     * @param state
     * 
     * @return
     * @throws Exception
     */
    private State getStateInfo(String state) throws Exception {
        Object[] stateData = null;
        CoreDao dao = null;
        Object[] stateInfo = null;
        final String query = "SELECT * FROM state WHERE state='" + state + "'";
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            stateData = dao.executeSQLQuery(query);
            if (stateData != null && stateData.length > 0) {
                stateInfo = (Object[]) stateData[0];
            }
        } catch (Exception e) {
            statusHandler
                    .error(" - PostgresSQL error retrieving state info info for "
                            + state);
            statusHandler.error("Query = [" + query + "]");
            throw e;
        }

        State s = new State();
        s.setState((String) stateInfo[0]);
        s.setName((String) stateInfo[1]);
        return s;
    }

    /**
     * Query the riverstat table and obtain stream name for the given location id
     *
     *
     * @param pe
     *            - the location id
     *
     * @return - riverstat
     * @throws Exception
     */
    private Riverstat getRiverStatInfo(String lid) throws Exception {
        Object[] rsData = null;
        CoreDao dao = null;
        Object[] rsInfo = null;
        final String query = "SELECT * FROM riverstat WHERE lid='" + lid + "'";
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            rsData = dao.executeSQLQuery(query);
            if (rsData != null && rsData.length > 0) {
                rsInfo = (Object[]) rsData[0];
            }
        } catch (Exception e) {
            statusHandler
                    .error(" - PostgresSQL error retrieving stream name info for "
                            + lid);
            statusHandler.error("Query = [" + query + "]");
            throw e;
        }

       Riverstat rsRecord = new Riverstat();
       if (rsInfo != null) {
           rsRecord.setStream((String) rsInfo[20]);
       } else
           rsRecord = null;

       return rsRecord;
    }

    /**
     * Query the descrip table and obtain proximity field for the given location id
     *
     *
     * @param pe
     *            - the location id
     *
     * @return - descrip
     * @throws Exception
     */
  
    private String getDescripInfo(String lid) throws Exception {
        Object[] dcData = null;
        CoreDao dao = null;
        Object[] dcInfo = null;
        String proximityStr = null;
        final String query = "SELECT * FROM descrip WHERE lid='" + lid + "'";
        try {
            dao = new CoreDao(DaoConfig.forDatabase(opt.getDbname()));
            dcData = dao.executeSQLQuery(query);
            if (dcData != null && dcData.length > 0) {
                statusHandler.debug("In getDescriptionInfo1 - query is select * from descript where lid is " + lid);
                dcInfo = (Object[]) dcData[0];
                if (dcInfo == null)
                {
                    proximityStr = "";
                }
                else
                {
                    proximityStr = (String) dcInfo[5] + Constants.SPACE;
                }
            }
        } catch (Exception e) {
            statusHandler
                    .error(" - PostgresSQL error retrieving descrip info for "
                            + lid);
            statusHandler.error("Query = [" + query + "]");
            throw e;

        }

        return proximityStr;
    }


    /**
     * @return - the alarm count
     */
    public int getAlarmCount() {
        return alarmCount;
    }
    /**
     *
     * @return threshold
     */
    public String getLimitThreshold() {
        return limitThreshold.toString();
    }

    /**
     * does not perform disk access
     * 
     * @param str
     *            -
     */
    private void write(String str) {
        reportData.append(str);
    }

    /**
     * writes a line and positions the file cursor after the new line separator.
     * 
     * @param str
     *            - a line to write to the file
     */
    private void writeln(String str) {
        reportData.append(str);
        writeNewline();
    }

    /*
     * write a line separator
     */
    private void writeNewline() {
        reportData.append(Constants.NEWLINE);
    }

    /**
     * Writes the report to disk.
     * 
     * @param report
     *            The file to write the report to.
     */
    public void writeToDisk(final File report) throws IOException {
        Writer outWriter = null;
        try {
            outWriter = new BufferedWriter(new FileWriter(report));
            outWriter.write(Constants.EOL);
            outWriter.write(reportData.toString());
        } finally {
            if (outWriter != null) {
                outWriter.close();
            }
        }
    }

}
