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
package com.raytheon.viz.aviation;

import com.raytheon.viz.aviation.utility.IBackupRestart;

/**
 * Holds a single backup/restart utility. Code originally extracted from
 * ForecastModel.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2015 4880       njensen     Extracted from ForecastModel
 *
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class BackupRestart {

    /**
     * Backup/Restart Utility for backing-up or restarting the TAF Monitor GUI.
     */
    private static IBackupRestart backupRestartUtility;

    private BackupRestart() {
        // don't allow instantiation
    }

    /**
     * Getter for backup/restart utility.
     * 
     * @return the backup/restart utility
     */
    public static IBackupRestart getBackupRestartUtility() {
        return backupRestartUtility;
    }

    /**
     * Setter for backup/restart utility.
     * 
     * @param backupRestartUtility
     */
    public static void setBackupRestartUtility(
            IBackupRestart backupRestartUtility) {
        BackupRestart.backupRestartUtility = backupRestartUtility;
    }

}
