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
package com.raytheon.uf.viz.monitor.scan;

import java.util.Date;

/**
 * Time class to manage times for scan
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2011            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ScanTime {

    protected enum ScanTimeType {
        REFTIME, VOLSCANTIME, DIALOGTIME
    }

    private Date reftime;

    private Date volScanTime;

    private Date dialogTime;

    public Date getTime(ScanTimeType type) {
        if (type == ScanTimeType.REFTIME) {
            return reftime;
        }
        if (type == ScanTimeType.VOLSCANTIME) {
            return volScanTime;
        }
        if (type == ScanTimeType.DIALOGTIME) {
            return dialogTime;
        }
        return null;
    }

    public void setTime(Date date, ScanTimeType type) {
        if (type == ScanTimeType.REFTIME) {
            reftime = date;
        }
        if (type == ScanTimeType.VOLSCANTIME) {
            volScanTime = date;
        }
        if (type == ScanTimeType.DIALOGTIME) {
            dialogTime = date;
        }
    }
}