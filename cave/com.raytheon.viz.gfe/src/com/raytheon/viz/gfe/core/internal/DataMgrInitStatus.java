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
package com.raytheon.viz.gfe.core.internal;

/**
 * Status of initialization of various fields of a DataManager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25, 2011            njensen     Initial creation
 * Oct 30, 2014  #3775     randerso    Added parmCacheInit to initStatus
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataMgrInitStatus {

    private boolean parmCacheInitDone = false;

    private boolean iscInitDone = false;

    private boolean selectTRMgrDone = false;

    private boolean sampleSetMgrDone = false;

    public boolean isParmCacheInitDone() {
        return parmCacheInitDone;
    }

    public void setParmCacheInitDone(boolean parmCacheInitDone) {
        this.parmCacheInitDone = parmCacheInitDone;
    }

    public boolean isIscInitDone() {
        return iscInitDone;
    }

    public void setIscInitDone(boolean iscInitDone) {
        this.iscInitDone = iscInitDone;
    }

    public boolean isSelectTRMgrDone() {
        return selectTRMgrDone;
    }

    public void setSelectTRMgrDone(boolean selectTRMgrDone) {
        this.selectTRMgrDone = selectTRMgrDone;
    }

    public boolean isSampleSetMgrDone() {
        return sampleSetMgrDone;
    }

    public void setSampleSetMgrDone(boolean sampleSetMgrDone) {
        this.sampleSetMgrDone = sampleSetMgrDone;
    }

    public boolean isDone() {
        return parmCacheInitDone && iscInitDone && sampleSetMgrDone
                && selectTRMgrDone;
    }

}
