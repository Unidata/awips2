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
package com.raytheon.uf.viz.alertviz;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;

/**
 * Used to write log entries out to a file
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 16, 2008  1433     chammack  Initial creation
 * Oct 01, 2018  7455     randerso  Simplified LogUtil.saveLogToFile() interface
 *                                  as the TimeStamp filter was not being used.
 *                                  Reordered priority, source, and category in
 *                                  log output.
 *
 * </pre>
 *
 * @author chammack
 */

public class LogUtil {

    /**
     * Save the entire alertViz log to a file
     *
     * @param file
     * @throws AlertvizException
     */
    public static void saveLogToFile(File file) throws AlertvizException {
        List<StatusMessage> msgs = LogMessageDAO.getInstance().load();
        try {
            file.getParentFile().mkdirs();
        } catch (Exception e) {
            throw new AlertvizException(
                    "Error creating directory " + file.getParent(), e);
        }

        try (PrintWriter pw = new PrintWriter(new FileWriter(file))) {
            SimpleDateFormat df = new SimpleDateFormat(
                    "yyyy-MM-dd HH:mm:ss.SSS");
            String localTZ = System.getenv("FXA_LOCAL_TZ");
            if (localTZ == null) {
                localTZ = "GMT";
            }
            df.setTimeZone(TimeZone.getTimeZone("localTZ"));
            for (StatusMessage sm : msgs) {
                StringBuilder sb = new StringBuilder();
                sb.append(df.format(sm.getEventTime()));
                sb.append(" ");
                sb.append('(').append(sm.getPriority().ordinal()).append(')');
                sb.append(" ");
                sb.append(sm.getSourceKey());
                sb.append(" ");
                sb.append(sm.getCategory());
                sb.append(" ");
                sb.append(sm.getMessage());
                if (sm.getDetails() != null) {
                    sb.append("\n  DETAILS: \n");
                    sb.append(sm.getDetails());
                }
                sb.append("\n");

                pw.write(sb.toString());
            }

        } catch (IOException e) {
            throw new AlertvizException("Error writing to file", e);
        }
    }

    /**
     * Roll text logs up one number for archival purposes
     *
     * @param logPrefix
     * @param logsToKeep
     */
    public static void rollLogs(String logPrefix, int logsToKeep) {
        for (int i = logsToKeep - 2; i >= 0; i--) {
            File oldFile = new File(logPrefix + "." + i);
            if (oldFile.exists()) {
                File newFile = new File(logPrefix + "." + (i + 1));
                oldFile.renameTo(newFile);
            }
        }
    }
}
