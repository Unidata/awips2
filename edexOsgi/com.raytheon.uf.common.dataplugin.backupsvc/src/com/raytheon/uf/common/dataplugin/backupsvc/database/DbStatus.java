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
package com.raytheon.uf.common.dataplugin.backupsvc.database;

/**
 * Enumeration for Backup Service Job Status Code.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer       Description
 * ------------ ---------- ----------- --------------------------
 * Jun 08, 2021  84473     Amanuel.Challa  Initial creation
 * Jul 13, 2021  92922     Robert.Blum     Added Failed status.
 * Aug 17, 2021  95214     Amanuel.Challa  Modified toString() to return all caps
 * </pre>
 *
 * @author achalla
 * @version 1.0
 */
public enum DbStatus {
    /**
     * Set the status of Backup service Jobs as New.
     **/
    NEW,
    /**
     * Set the status of Backup service Jobs as Send.
     **/
    SENT,
    /**
     * Set the status of Backup service Jobs as Wait.
     **/
    WAIT,
    /**
     * Set the status of Backup service Jobs as Accepted.
     **/
    ACCEPTED,
    /**
     * Set the status of Backup service Jobs as Rejected.
     **/
    REJECTED,
    /**
     * Set the status of Backup service Jobs as Failed.
     */
    FAILED;

    @Override
    public String toString() {
        String name = name().toUpperCase();
        return name;
    }
}
