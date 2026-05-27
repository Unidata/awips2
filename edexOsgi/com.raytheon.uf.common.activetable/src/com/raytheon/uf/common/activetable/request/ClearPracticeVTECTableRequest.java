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

import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to clear practice VTEC active table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 09, 2014  #3004     dgilling    Move to activetable plugin, remove GFE
 *                                     dependencies.
 * 
 * </pre>
 * 
 * @author xxxxxxxx
 * @version 1.0
 */
@DynamicSerialize
public final class ClearPracticeVTECTableRequest implements IServerRequest {

    @DynamicSerializeElement
    private WsId workstationID;

    @DynamicSerializeElement
    private String siteID;

    /**
     * Default constructor. Intended only to be used by DynamicSerialize.
     */
    public ClearPracticeVTECTableRequest() {

    }

    /**
     * Constructs a new request for the given site ID and workstation ID.
     * 
     * @param siteID
     *            The 4-character site identifier of the site whose active table
     *            you want to clear.
     * @param workstationID
     *            <code>WsId</code> of the user making the request.
     */
    public ClearPracticeVTECTableRequest(String siteID, WsId workstationID) {
        this.siteID = siteID;
        this.workstationID = workstationID;
    }

    public WsId getWorkstationID() {
        return workstationID;
    }

    public void setWorkstationID(WsId workstationID) {
        this.workstationID = workstationID;
    }

    public String getSiteID() {
        return siteID;
    }

    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }
}
