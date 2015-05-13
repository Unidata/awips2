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
package com.raytheon.uf.common.activetable.request;

import java.util.Date;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to lock active table and get next ETN to be used.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2013  #1843     dgilling     Initial creation
 * Oct 21, 2013  #1843     dgilling     Add ETN override field.
 * Apr 28, 2015  #4027     randerso     Expunged Calendar from ActiveTableRecord
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class LockAndGetNextEtnRequest implements IServerRequest {

    @DynamicSerializeElement
    private String siteID;

    @DynamicSerializeElement
    private String requestorSiteID;

    @DynamicSerializeElement
    private ActiveTableMode mode;

    @DynamicSerializeElement
    private String phensig;

    @DynamicSerializeElement
    private Date currentTime;

    @DynamicSerializeElement
    private Integer etnOverride;

    public LockAndGetNextEtnRequest() {
        // default constructor for thrift/dynamicserialize
    }

    public LockAndGetNextEtnRequest(String siteID, String requestorSiteID,
            ActiveTableMode mode, String phensig, Date currentTime) {
        this(siteID, requestorSiteID, mode, phensig, currentTime, null);
    }

    public LockAndGetNextEtnRequest(String siteID, String requestorSiteID,
            ActiveTableMode mode, String phensig, Date currentTime,
            Integer etnOverride) {
        this.siteID = siteID;
        this.requestorSiteID = requestorSiteID;
        this.mode = mode;
        this.phensig = phensig;
        this.currentTime = currentTime;
        this.etnOverride = etnOverride;
    }

    public String getSiteID() {
        return siteID;
    }

    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }

    public String getRequestorSiteID() {
        return requestorSiteID;
    }

    public void setRequestorSiteID(String requestorSiteID) {
        this.requestorSiteID = requestorSiteID;
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

    public Date getCurrentTime() {
        return currentTime;
    }

    public void setCurrentTime(Date currentTime) {
        this.currentTime = currentTime;
    }

    public Integer getEtnOverride() {
        return etnOverride;
    }

    public void setEtnOverride(Integer etnOverride) {
        this.etnOverride = etnOverride;
    }
}
