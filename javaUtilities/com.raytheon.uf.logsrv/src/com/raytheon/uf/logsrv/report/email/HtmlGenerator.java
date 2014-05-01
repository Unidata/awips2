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
package com.raytheon.uf.logsrv.report.email;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.logsrv.report.data.LogReportContainer;
import com.raytheon.uf.logsrv.report.data.LogReportEvent;

/**
 * Uses a LogReportContainer to generate an HTML report that can be emailed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class HtmlGenerator {

    private static final int MAX_ERRORS = 100;

    private static final String LINE_BREAK = "<BR/>";

    private static final String HR = "<HR/>";

    private static final SimpleDateFormat SDF = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm z");

    /**
     * Generates an HTML string based on the report container
     * 
     * @param container
     *            the container of events to generate HTML for
     * @return an HTML formatted report
     */
    public static String generateHtml(LogReportContainer container) {
        String report = null;
        if (container != null) {
            Collection<LogReportEvent> collection = container.getEvents();
            LogReportEvent[] array = collection.toArray(new LogReportEvent[0]);
            Arrays.sort(array, Collections.reverseOrder());

            StringBuilder sb = new StringBuilder();
            sb.append(buildHeader(array.length, container.getEarliestTime(),
                    container.getLatestTime()));
            sb.append(LINE_BREAK);
            boolean foundFirstWithoutStacktrace = false;
            for (int i = 0; i < array.length && i < MAX_ERRORS; i++) {
                LogReportEvent event = array[i];
                if (!event.receivedStack() && !foundFirstWithoutStacktrace) {
                    foundFirstWithoutStacktrace = true;
                    sb.append(buildNoStacktraceDisclaimer());
                }
                sb.append(HR);
                sb.append(LINE_BREAK);
                sb.append(buildEvent(array[i]));
                sb.append(LINE_BREAK);
                sb.append(LINE_BREAK);
            }
            report = sb.toString();
        }

        return report;
    }

    private static CharSequence buildHeader(int distinctErrors, Date earliest,
            Date latest) {
        StringBuilder sb = new StringBuilder();
        sb.append("Auto-generated error report");
        sb.append(LINE_BREAK);
        sb.append("Created on ");
        sb.append(SDF.format(new Date()));
        sb.append(LINE_BREAK);
        if (distinctErrors > 0) {
            sb.append("Timeframe of errors: <span style=\"font-style:italic;\">");
            sb.append(SDF.format(earliest));
            sb.append("</span>");
            sb.append(" to <span style=\"font-style:italic;\">");
            sb.append(SDF.format(latest));
            sb.append("</span>");
            sb.append(LINE_BREAK);
        }
        sb.append("Number of distinct errors reported: <span style=\"font-weight:bold;\">");
        sb.append(distinctErrors);
        sb.append("</span>");
        sb.append(LINE_BREAK);
        if (distinctErrors > MAX_ERRORS) {
            sb.append("Report truncated to top ");
            sb.append(MAX_ERRORS);
            sb.append(" errors/warnings");
            sb.append(LINE_BREAK);
        }

        return sb;
    }

    private static CharSequence buildEvent(LogReportEvent event) {
        StringBuilder sb = new StringBuilder();
        sb.append("<span style=\"font-weight:bold;\">");
        sb.append(event.getLevel());
        if (!event.receivedStack()) {
            // thread name is really only useful if we don't have a stacktrace
            sb.append("  [");
            sb.append(event.getThreadName());
            sb.append("]");
        }
        sb.append("  ");
        sb.append(event.getSampleMessage());
        sb.append("</span>");
        sb.append(LINE_BREAK);
        sb.append("Occurred a total of <span style=\"font-weight:bold;\">");
        sb.append(event.getTotalOccurences());
        sb.append("</span> times on the following machines:");
        sb.append(LINE_BREAK);
        Map<String, Integer> machineCount = event.getMachineCount();
        Set<Entry<String, Integer>> set = machineCount.entrySet();
        List<Entry<String, Integer>> list = new ArrayList<Entry<String, Integer>>(
                set);
        // sort it so the machines with the most errors are listed first
        Collections.sort(list, new Comparator<Entry<String, Integer>>() {
            @Override
            public int compare(Entry<String, Integer> o1,
                    Entry<String, Integer> o2) {
                int val1 = o1.getValue();
                int val2 = o2.getValue();
                return (val2 < val1) ? -1 : ((val1 == val2) ? 0 : 1);
            }

        });

        for (Entry<String, Integer> entry : list) {
            sb.append("<span style=\"font-style:italic; padding-left: 30px;\">");
            sb.append(entry.getKey());
            sb.append("</span>: ");
            sb.append(entry.getValue());
            sb.append(" occurrences");
            sb.append(LINE_BREAK);
        }
        sb.append(LINE_BREAK);
        if (event.receivedStack()) {
            sb.append(event.getSampleStacktrace().replaceAll("\n", LINE_BREAK));
        }
        sb.append(LINE_BREAK);
        return sb;
    }

    private static CharSequence buildNoStacktraceDisclaimer() {
        StringBuilder sb = new StringBuilder();
        sb.append(LINE_BREAK);
        sb.append(HR);
        sb.append("<b>Disclaimer</b>: The following errors were received without stacktraces and...");
        sb.append("<ul><li>Are estimated/inexact</li>");
        sb.append("<li>Should be considered for downgrade to INFO or DEBUG messages");
        sb.append("<li>Should include stacktraces if remaining as ERROR</li></ul>");
        return sb;
    }

}
