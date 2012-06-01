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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.ui.Activator;

/**
 * Message archiver for a collaboration session. Archives messages to
 * Localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2012            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class SessionMsgArchive {

    private static final String LOG_DIR = "collaboration"
            + IPathManager.SEPARATOR + "logs";

    private LocalizationFile logFile;

    private StringBuffer log;

    public SessionMsgArchive(String hostName, String userId, String sessionName) {
        Calendar gmtTimestamp = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        String logFileName = new SimpleDateFormat("yyyy-MM-dd.hhmmss")
                .format(gmtTimestamp.getTime()) + ".txt";

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);
        logFile = pm.getLocalizationFile(ctx,
                getLogFilePath(hostName, userId, sessionName)
                        + IPathManager.SEPARATOR + logFileName);
        log = new StringBuffer();
    }

    private static String getLogFilePath(String hostName, String userId,
            String sessionName) {
        return LOG_DIR
                + IPathManager.SEPARATOR
                + hostName
                + IPathManager.SEPARATOR
                + userId
                + (sessionName == null ? "" : IPathManager.SEPARATOR
                        + sessionName);
    }

    public static LocalizationFile getArchiveDir(String hostName,
            String userId, String sessionName) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);
        return pm.getLocalizationFile(ctx,
                getLogFilePath(hostName, userId, sessionName));
    }

    public void close() {
        // Write log contents to logFile
        try {
            logFile.write(log.toString().getBytes());
        } catch (LocalizationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Error writing log file: " + e.getLocalizedMessage(), e);
        }
    }

    public void archive(String string) {
        log.append(string);
    }
}
