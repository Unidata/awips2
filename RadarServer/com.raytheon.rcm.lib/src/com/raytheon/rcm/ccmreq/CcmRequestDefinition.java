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
package com.raytheon.rcm.ccmreq;

/**
 * Command control message requests definition
 *
 * <pre>
 *
 *  SOFTWARE HISTORY
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------- --------------------------
 *  2016-05-10   18795      jdynina     Initial creation
 *
 * </pre>
 *
 */

public class CcmRequestDefinition {

    boolean restartVcp;
    int vcp;
    int avsetEnabled;
    int sailsCount;

    public boolean getRestartVcp() {
        return restartVcp;
    }

    public int getVcp() {
        return vcp;
    }

    public int getAvsetEnabled() {
        return avsetEnabled;
    }

    public int getSailsCount() {
        return sailsCount;
    }

    public void setRestartVcp(boolean restartVcpFlag) {
        restartVcp = restartVcpFlag;
    }

    public void setVcp (int newVcp) {
        vcp = newVcp;
    }

    public void setAvsetEnabled (int avsetEnabledFlag) {
        avsetEnabled = avsetEnabledFlag;
    }

    public void setSailsCount (int newSailsCount) {
        sailsCount = newSailsCount;
    }
}


