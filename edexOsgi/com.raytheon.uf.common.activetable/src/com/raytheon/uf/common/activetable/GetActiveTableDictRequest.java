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
 * This is just like GetActiveTableRequest, except that the handler returns the
 * active table as a List&lt;Map&lt;String,Object&gt;&gt;, removing the need for
 * Python clients to convert it (and thus their need for access to several
 * server-side scripts).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 3058       rjpeter     Initial creation
 * Oct.29, 2010 4582       wldougher   Copied from GetActiveTableRequest
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@DynamicSerialize
public class GetActiveTableDictRequest implements IServerRequest {

    @DynamicSerializeElement
    private String requestedSiteId;

    @DynamicSerializeElement
    private ActiveTableMode mode;

    @DynamicSerializeElement
    private String[] wfos;

    /**
     * @return the requestedSiteId
     */
    public String getRequestedSiteId() {
        return requestedSiteId;
    }

    /**
     * @param requestedSiteId
     *            the requestedSiteId to set
     */
    public void setRequestedSiteId(String requestedSiteId) {
        this.requestedSiteId = requestedSiteId;
    }

    public ActiveTableMode getMode() {
        return mode;
    }

    public void setMode(ActiveTableMode mode) {
        this.mode = mode;
    }

    public String[] getWfos() {
        return wfos;
    }

    public void setWfos(String[] wfos) {
        this.wfos = wfos;
    }
}
