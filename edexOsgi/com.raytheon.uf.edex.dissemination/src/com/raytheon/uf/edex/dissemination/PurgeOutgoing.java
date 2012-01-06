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
package com.raytheon.uf.edex.dissemination;

import java.io.File;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.edex.database.purge.PurgeLogger;

/**
 * PurgeOutgoing
 * 
 * purge() method purges files in the defined outgoingDirectory that are more
 * than 24 hours old based on the millisecond timestamp in the filename
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2011            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class PurgeOutgoing {

    private static final String plugin = "Purge Outgoing";

    /**
     * outgoingDirectory - the directory to purge files from
     */
    private String outgoingDirectory;

    // this pattern is intended to grab all consecutive numerical digits at the
    // end of the file, and assumes it is the unix time in millis
    private static final Pattern outgoingDatePattern = Pattern
            .compile("[^\\d]*(\\d+)$");

    public PurgeOutgoing() {

    }

    /**
     * purges files in outgoingDirectory
     */
    public void purge() {
        long now = (new Date()).getTime();
        long day = 24L * 60L * 60L * 1000L;
        int count = 0;
        PurgeLogger.logInfo("---------START OUTGOING PURGE---------", plugin);
        if (outgoingDirectory != null) {
            File path = new File(outgoingDirectory);
            if (path.exists()) {
                String[] files = path.list();
                for (String file : files) {
                    Matcher m = outgoingDatePattern.matcher(file);
                    if (m.find()) {
                        long time = Long.parseLong(m.group(1));
                        time *= 1000;
                        if (now - time >= day) {
                            // remove the file
                            File tmp = new File(outgoingDirectory + "/" + file);
                            if (tmp.exists()) {
                                count++;
                                tmp.delete();
                            }
                        }
                    } else {
                        PurgeLogger.logInfo(
                                "Skipped file with unexpected fileName: "
                                        + file, plugin);
                    }
                }
            }
        }
        PurgeLogger.logInfo("Removed " + count + " files", plugin);
        PurgeLogger.logInfo("---------END OUTGOING PURGE-----------", plugin);
    }

    /**
     * @param outgoingDirectory
     *            the outgoingDirectory to set
     */
    public void setOutgoingDirectory(String outgoingDirectory) {
        this.outgoingDirectory = outgoingDirectory;
    }

    /**
     * @return the outgoingDirectory
     */
    public String getOutgoingDirectory() {
        return outgoingDirectory;
    }
}
