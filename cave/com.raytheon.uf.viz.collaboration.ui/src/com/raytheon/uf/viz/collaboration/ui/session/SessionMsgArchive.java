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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;

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
    Calendar archiveDate = null;

    LocalizationFile logFile = null;

    synchronized private LocalizationFile getLocalizationLogFile(
            long timestamp, String sessionId, String host) {
        boolean createNew = (archiveDate == null || logFile == null);
        Calendar gmtTimestamp = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        gmtTimestamp.setTimeInMillis(timestamp);

        if (archiveDate != null
                && archiveDate.get(Calendar.DAY_OF_YEAR) != gmtTimestamp
                        .get(Calendar.DAY_OF_YEAR)) {
            // roll log to new date
            if (logFile != null) {
                try {
                    logFile.save();
                } catch (LocalizationOpFailedException e) {
                    e.printStackTrace();
                }
                archiveDate = gmtTimestamp;
                createNew = true;
            }
        }

        if (createNew) {
            String dirName = "collaboration" + File.separator + sessionId + "@"
                    + host + File.separator;
            // 2012-05-16.txt
            String date = String.format("%1$tY-%1$tm-%1$td", gmtTimestamp);
            String fileName = dirName + date + ".txt";

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext ctx = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            logFile = pm.getLocalizationFile(ctx, fileName);
        }
        return logFile;
    }

    public void save() {
        if (logFile != null) {
            try {
                logFile.save();
            } catch (LocalizationOpFailedException e) {
                e.printStackTrace();
            }
        }
    }

    public void archive(String string, long timestamp, String sessionId,
            String host) {
        LocalizationFile outFile = getLocalizationLogFile(timestamp, sessionId,
                host);
        OutputStream os = null;
        try {
            os = outFile.openOutputStream(true);
            os.write(string.getBytes());
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (LocalizationException e) {
            e.printStackTrace();
        } finally {
            if (os != null) {
                try {
                    os.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
