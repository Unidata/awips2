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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 3058       rjpeter     Initial creation
 * Apr 03, 2014 2737       randerso    Moved clientSendStatus from SaveGridRequest
 *                                     Added proper constructor
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class SaveGfeGridRequest extends AbstractGfeRequest {
    /**
     * Denotes whether to send ISC grids on save based on the client's
     * preferences
     */

    @DynamicSerializeElement
    private boolean clientSendStatus = true;

    @DynamicSerializeElement
    private List<SaveGridRequest> saveRequests = new ArrayList<SaveGridRequest>();

    /**
     * Default constructor for serialization
     */
    public SaveGfeGridRequest() {

    }

    /**
     * Constructor
     * 
     * @param clientSendStatus
     *            true if ISC grids should be sent
     * @param saveRequests
     *            the save requests
     */
    public SaveGfeGridRequest(boolean clientSendStatus,
            List<SaveGridRequest> saveRequests) {
        this.clientSendStatus = clientSendStatus;
        this.saveRequests = saveRequests;
    }

    /**
     * @return the clientSendStatus
     */
    public boolean isClientSendStatus() {
        return clientSendStatus;
    }

    /**
     * @param clientSendStatus
     *            the clientSendStatus to set
     */
    public void setClientSendStatus(boolean clientSendStatus) {
        this.clientSendStatus = clientSendStatus;
    }

    /**
     * @return the saveRequest
     */
    public List<SaveGridRequest> getSaveRequests() {
        return saveRequests;
    }

    /**
     * @param saveRequest
     *            the saveRequest to set
     */
    public void setSaveRequests(List<SaveGridRequest> saveRequests) {
        this.saveRequests = saveRequests;
    }
}
