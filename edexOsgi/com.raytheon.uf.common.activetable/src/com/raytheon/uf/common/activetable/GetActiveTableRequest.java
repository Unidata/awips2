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
 * Feb 11, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class GetActiveTableRequest implements IServerRequest {

    @DynamicSerializeElement
    private String siteID;

    @DynamicSerializeElement
    private ActiveTableMode mode;

    @DynamicSerializeElement
    private String phensigList;

    @DynamicSerializeElement
    private Boolean requestValidTimes;

    @DynamicSerializeElement
    private String etn;

    @DynamicSerializeElement
    private String act;

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

    public String getPhensigList() {
        return phensigList;
    }

    public void setPhensigList(String phensigList) {
        this.phensigList = phensigList;
    }

    public Boolean getRequestValidTimes() {
        return requestValidTimes;
    }

    public void setRequestValidTimes(Boolean requestValidTimes) {
        this.requestValidTimes = requestValidTimes;
    }

    public String getEtn() {
        return etn;
    }

    public void setEtn(String etn) {
        this.etn = etn;
    }

    public String getAct() {
        return act;
    }

    public void setAct(String act) {
        this.act = act;
    }
}
