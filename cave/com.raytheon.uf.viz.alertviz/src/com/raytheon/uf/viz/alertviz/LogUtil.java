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
import java.sql.Timestamp;
import java.text.DateFormat;
import java.util.TimeZone;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;

/**
 * Used to write log entries out to a file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2008 1433       chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class LogUtil {

    public static enum Order {
        BEFORE, AFTER
    };

    public static void saveLogToFile(File file, Timestamp threshold, Order order)
            throws AlertvizException {
        StatusMessage[] msgs = LogMessageDAO.getInstance().load(999999,
                threshold, order);
        FileWriter fw = null;
        PrintWriter pw = null;
        try {
            File parent = file.getParentFile();
            parent.mkdirs();

            fw = new FileWriter(file);
            pw = new PrintWriter(fw);
            DateFormat df = DateFormat.getDateTimeInstance();
            df.setTimeZone(TimeZone.getTimeZone("GMT"));
            for (StatusMessage sm : msgs) {
                StringBuffer sBuffer = new StringBuffer();
                sBuffer.append(df.format(sm.getEventTime()));
                sBuffer.append(" ");
                sBuffer.append(sm.getCategory());
                sBuffer.append(" ");
                sBuffer.append(sm.getPriority());
                sBuffer.append(" ");
                sBuffer.append(sm.getSourceKey());
                sBuffer.append(" ");
                sBuffer.append(sm.getMessage());
                if (sm.getDetails() != null) {
                    sBuffer.append("\n  DETAILS: \n");
                    sBuffer.append(sm.getDetails());
                }
                sBuffer.append("\n");

                pw.write(sBuffer.toString());
            }

        } catch (IOException e) {
            throw new AlertvizException("Error writing to file", e);
        } finally {
            if (pw != null)
                pw.close();
            try {
                if (fw != null)
                    fw.close();
            } catch (IOException e) {
                // ignore
            }
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
