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
package com.raytheon.viz.aviation.monitor;

import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest.QueryLanguage;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Utility functions related to TAFs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2009             njensen     Initial creation
 * Jul 6, 2010  5792       rferrel     getLatestTafs now returns tafs
 *                                     sorted by issue date newest at
 *                                     the start of the array.
 * 08AUG2012    15613      zhao        Modified safeFormatTaf()
 * Sep 11, 2013 2277       mschenke    Got rid of ScriptCreator references
 * Feb 24, 2014 2830       njensen     Sort dataTimes in getLatestTafs()
 *                                       so it works correctly
 * May 15, 2014 3002       bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf
 * Sep 16, 2015 4880       njensen     Optimized getLatestTafs()
 * Jan 31, 2018 6945       tgurney     Sort COR and AMD tafs before routine tafs
 *
 * </pre>
 *
 * @author njensen
 */

public class TafUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafUtil.class);

    public static final String LINE_BREAK = "\n";

    public static final String FIVE_SPACES = "     ";

    /**
     * Gets the latest TAF for a site. This should be deprecated and the request
     * should be done in python through pointdata, as soon as pointdata is
     * hooked up to TAFs.
     *
     * @param siteID
     * @return
     */
    public static TafRecord getLatestTaf(String siteID) {
        TafRecord[] tafs = TafUtil.getLatestTafs(siteID, 1);
        if (tafs != null && tafs.length > 0) {
            return tafs[0];
        } else {
            return null;
        }
    }

    /**
     * Gets the latest tafs for a site, up to the number specified
     *
     * @param siteID
     * @param numberOfTafs
     * @return
     */
    public static TafRecord[] getLatestTafs(String siteID, int numberOfTafs) {
        try {
            Map<String, Object> paramMap = new HashMap<>();
            paramMap.put("stationId", siteID);
            QlServerRequest r = new QlServerRequest();
            r.setLang(QueryLanguage.HQL);
            r.setParamMap(paramMap);
            r.setQuery("from TafRecord t " + "where stationId = :stationId "
                    + "order by issue_time desc, amdindicator, corindicator ");
            r.setMaxResults(numberOfTafs);
            ResponseMessageGeneric response = (ResponseMessageGeneric) RequestRouter
                    .route(r);
            QueryResult result = (QueryResult) response.getContents();
            QueryResultRow[] rows = result.getRows();
            return Arrays.stream(rows)
                    .map(row -> (TafRecord) (row.getColumnValues()[0]))
                    .toArray(TafRecord[]::new);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error retrieving TAFs", e);
        }
        return null;
    }

    /**
     * Formats a TAF as a String how AvnFPS expects it
     *
     * @param t
     * @param includeHeader
     * @return
     */
    public static String safeFormatTaf(TafRecord t, boolean includeHeader) {
        StringBuilder sb = new StringBuilder();
        if (t != null) {
            String[] text = t.getTafText().split("[\r\n]");
            if (includeHeader) {
                sb.append(t.getWmoHeader());
                sb.append(LINE_BREAK);
                sb.append("TAF").append(t.getStationId().substring(1, 4))
                        .append(LINE_BREAK);
            }
            String firstLine = text[0];
            if (firstLine.startsWith("TAF AMD")
                    || firstLine.startsWith("TAF COR")) {
                sb.append(firstLine.substring(0, 7).trim());
                sb.append(LINE_BREAK);
                sb.append(firstLine.substring(7).trim());
                sb.append(LINE_BREAK);
            } else {
                sb.append(firstLine.substring(0, 4).trim());
                sb.append(LINE_BREAK);
                sb.append(firstLine.substring(4).trim());
                sb.append(LINE_BREAK);
            }
            for (int i = 1; i < text.length; i++) {
                String line = text[i].trim();
                sb.append(FIVE_SPACES);
                if (line.startsWith("TEMPO")) {
                    sb.append(" ");
                }
                sb.append(line);
                if (i + 1 < text.length) {
                    sb.append(LINE_BREAK);
                }
            }
            sb.append(LINE_BREAK);
        }
        return sb.toString();
    }

    /**
     * This converts time to a string in the format DDHHMMZ. Where DD is the day
     * of the month, HH the hours of the day and MM the minute of the hour and
     * the Z indicates GMT. This is based on AvnLib.getFmtIssueTime.
     *
     * @param time
     *            - time to format when <= 0 then current time is used
     * @return itime
     */
    public static String getFmtIssueTime(long time) {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        if (time <= 0L) {
            time = System.currentTimeMillis();
        }
        calendar.setTimeInMillis(time);
        return String.format("%02d%02d%02dZ",
                calendar.get(Calendar.DAY_OF_MONTH),
                calendar.get(Calendar.HOUR_OF_DAY),
                calendar.get(Calendar.MINUTE));
    }

    private static long MSEC_PER_SEC = 1000L;

    private static long MSEC_PER_MIN = 60L * MSEC_PER_SEC;

    private static long MSEC_PER_HR = 60L * MSEC_PER_MIN;

    private static long[] FCST_TIMES = new long[] { 6L * MSEC_PER_HR,
            12L * MSEC_PER_HR, 18L * MSEC_PER_HR, 24L * MSEC_PER_HR };

    public static int startHour(boolean routineTAF, long time) {
        if (!routineTAF) {
            time += 30L * MSEC_PER_MIN;
        }

        long thour = time % (24L * MSEC_PER_HR);
        int i = 0;
        for (i = 0; i < FCST_TIMES.length; ++i) {
            if (thour < FCST_TIMES[i]) {
                break;
            }
        }
        if (i >= FCST_TIMES.length) {
            i = 0;
        }

        if (!routineTAF) {
            i--;
            if (i < 0) {
                i = FCST_TIMES.length - 1;
            }
        }
        return (int) ((FCST_TIMES[i] / MSEC_PER_HR) % 24);
    }

    /**
     * This gets a string that indicates the starting and end time for a TAF.
     * The string is in the format DDHH/DDHH. Where the start time is before the
     * slash and the end time is after. The times are GMT and the DD is the day
     * of the month and HH the hour of the day. This is based on the
     * AvnLig.getFmtValidTime.
     *
     * @param bbb
     *            - The type of TAF
     * @param time
     *            - Use to determine start/end time if <= 0 use current time
     * @param tafDuration
     *            - hours of TAF's duration
     * @param evtime
     *            - Not Yet Implemented
     * @return vtime
     */
    public static String getFmtValidTime(String bbb, long time,
            String tafDuration, Object evtime) {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        if (time <= 0L) {
            time = System.currentTimeMillis();
        }

        boolean routineTaf = bbb != null && bbb.startsWith(" ");

        // Get ending day and hour.
        int duration = Integer.parseInt(tafDuration);
        calendar.setTimeInMillis(time);
        calendar.set(Calendar.HOUR_OF_DAY, startHour(routineTaf, time));
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        if (calendar.get(Calendar.HOUR_OF_DAY) == 0 && routineTaf) {
            calendar.add(Calendar.DAY_OF_MONTH, 1);
        }
        calendar.add(Calendar.HOUR_OF_DAY, duration);
        int endDay = calendar.get(Calendar.DAY_OF_MONTH);
        int endHour = calendar.get(Calendar.HOUR_OF_DAY);

        // Make previous day's 2400 hour
        if (endHour == 0) {
            endHour = 24;
            calendar.add(Calendar.HOUR_OF_DAY, -24);
            endDay = calendar.get(Calendar.DAY_OF_MONTH);
        }

        // for amendments and delayed forecasts: use closest hour
        int stHour = 0;
        if (!routineTaf) {
            stHour = (int) (((time + 30L * MSEC_PER_MIN) / MSEC_PER_HR) % 24L);
        } else {
            stHour = startHour(routineTaf, time);
        }
        calendar.setTimeInMillis(time);
        calendar.set(Calendar.HOUR_OF_DAY, stHour);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        if (calendar.get(Calendar.HOUR_OF_DAY) == 0 && routineTaf) {
            calendar.add(Calendar.DAY_OF_MONTH, 1);
        }
        int startDay = calendar.get(Calendar.DAY_OF_MONTH);
        int startHour = calendar.get(Calendar.HOUR_OF_DAY);
        return String.format("%02d%02d/%02d%02d", startDay, startHour, endDay,
                endHour);
    }
}
