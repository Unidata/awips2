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
package com.raytheon.uf.common.activetable;

import java.util.Calendar;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2011            rjpeter     Initial creation
 * Oct 21, 2013  #1843     dgilling    Add performISC and reportConflictOnly
 *                                     fields, proper constructors.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class GetNextEtnRequest implements IServerRequest {

    @DynamicSerializeElement
    private String siteID;

    @DynamicSerializeElement
    private ActiveTableMode mode;

    @DynamicSerializeElement
    private String phensig;

    @DynamicSerializeElement
    private Calendar currentTime;

    @DynamicSerializeElement
    private boolean lockEtn;

    @DynamicSerializeElement
    private boolean performISC;

    @DynamicSerializeElement
    private boolean reportConflictOnly;

    @DynamicSerializeElement
    private Integer etnOverride;

    public GetNextEtnRequest() {
        // no-op, for dynamic serialize support
    }

    /**
     * Constructs a GetNextEtnRequest.
     * 
     * @param siteID
     *            The 4-character site ID of the office.
     * @param mode
     *            Active table mode.
     * @param phensig
     *            The phenomenon and significance of the hazard concatenated
     *            with a '.' (e.g., TO.W or DU.Y)
     * @param currentTime
     *            <code>Calendar</code> representing time (needed for DRT mode).
     * @param lockEtn
     *            Whether or not to request an exclusive ETN--if true, this will
     *            cause the server to increment its running ETN sequence to the
     *            next number after determining the next ETN for this request.
     *            If false, the next ETN will be returned, but it will not
     *            increment the server's running sequence, so the ETN return
     *            could be used by another client that makes a
     *            GetNextEtnRequest.
     */
    public GetNextEtnRequest(String siteID, ActiveTableMode mode,
            String phensig, Calendar currentTime, boolean lockEtn) {
        this(siteID, mode, phensig, currentTime, lockEtn, false, false, null);
    }

    /**
     * Constructs a GetNextEtnRequest.
     * 
     * @param siteID
     *            The 4-character site ID of the office.
     * @param mode
     *            Active table mode.
     * @param phensig
     *            The phenomenon and significance of the hazard concatenated
     *            with a '.' (e.g., TO.W or DU.Y)
     * @param currentTime
     *            <code>Calendar</code> representing time (needed for DRT mode).
     * @param lockEtn
     *            Whether or not to request an exclusive ETN--if true, this will
     *            cause the server to increment its running ETN sequence to the
     *            next number after determining the next ETN for this request.
     *            If false, the next ETN will be returned, but it will not
     *            increment the server's running sequence, so the ETN return
     *            could be used by another client that makes a
     *            GetNextEtnRequest.
     * @param performISC
     *            Whether or not to collaborate with neighboring sites to
     *            determine the next ETN. See {@link
     *            GetNextEtnUtil#getNextEtnFromPartners(String, ActiveTableMode,
     *            String, Calendar, List<IRequestRouter>)} for more information.
     * @param reportConflictOnly
     *            Affects which kinds of errors get reported back to the
     *            requestor. If true, only cases where the value of
     *            <code>etnOverride</code> is less than or equal to the last ETN
     *            used by this site or any of its partners will be reported.
     *            Else, all significant errors will be reported back.
     * @param etnOverride
     *            Allows the user to influence the next ETN assigned by using
     *            this value unless it is less than or equal to the last ETN
     *            used by this site or one of its partners.
     */
    public GetNextEtnRequest(String siteID, ActiveTableMode mode,
            String phensig, Calendar currentTime, boolean lockEtn,
            boolean performISC, boolean reportConflictOnly, Integer etnOverride) {
        this.siteID = siteID;
        this.mode = mode;
        this.phensig = phensig;
        this.currentTime = currentTime;
        this.lockEtn = lockEtn;
        this.performISC = performISC;
        this.reportConflictOnly = reportConflictOnly;
        this.etnOverride = etnOverride;
    }

    public String getSiteID() {
        return siteID;
    }

    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }

    public ActiveTableMode getMode() {
        return mode;
    }

    public void setMode(ActiveTableMode mode) {
        this.mode = mode;
    }

    public String getPhensig() {
        return phensig;
    }

    public void setPhensig(String phensig) {
        this.phensig = phensig;
    }

    public boolean isLockEtn() {
        return lockEtn;
    }

    public void setLockEtn(boolean lockEtn) {
        this.lockEtn = lockEtn;
    }

    public Calendar getCurrentTime() {
        return currentTime;
    }

    public void setCurrentTime(Calendar currentTime) {
        this.currentTime = currentTime;
    }

    public boolean isPerformISC() {
        return performISC;
    }

    public void setPerformISC(boolean performISC) {
        this.performISC = performISC;
    }

    public boolean isReportConflictOnly() {
        return reportConflictOnly;
    }

    public void setReportConflictOnly(boolean reportConflictOnly) {
        this.reportConflictOnly = reportConflictOnly;
    }

    public Integer getEtnOverride() {
        return etnOverride;
    }

    public void setEtnOverride(Integer etnOverride) {
        this.etnOverride = etnOverride;
    }
}
