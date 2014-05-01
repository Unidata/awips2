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

import com.raytheon.uf.common.activetable.ActiveTableMode;
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
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@DynamicSerialize
public class GetActiveTableRequest extends AbstractGfeRequest {

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

    /**
     * Set the WFOs on which to filter. If any entry is the special string
     * "all", no WFO filtering will be performed. Otherwise, only records whose
     * xxxid field matches an entry in this list will be returned.
     * 
     * @param wfos
     *            The wfos to include
     */
    public void setWfos(String... wfos) {
        this.wfos = wfos;
    }

    /**
     * @return The list of wfos to include.
     */
    public String[] getWfos() {
        return wfos;
    }
}
