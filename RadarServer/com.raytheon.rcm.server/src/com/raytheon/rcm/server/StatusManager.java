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
package com.raytheon.rcm.server;

import com.raytheon.rcm.config.LinkResource;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2016-05-25   DR 18800   jdynina     Removed alerting
 * 2016-05-15   DCS18795   jdynina     Added CPM
 * </pre>
 *
 */

public interface StatusManager {
    public static interface RadarStatus {
        byte[] getCurrentGSM();
        byte[] getCurrentCPM();
        byte[] getCurrentPTL();
        byte[] getLastGSM();
        byte[] getLastCPM();
        byte[] getLastPTL();

        // Required to determine the maximum RPS list size
        LinkResource getLinkResource();
    }

    public RadarStatus getRadarStatus(String radarID);
}
