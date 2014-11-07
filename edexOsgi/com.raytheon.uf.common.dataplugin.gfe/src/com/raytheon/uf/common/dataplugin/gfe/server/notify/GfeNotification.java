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

package com.raytheon.uf.common.dataplugin.gfe.server.notify;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.message.IMessage;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.SystemUtil;

/**
 * Base class for GFE Notifications
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2014   #3684     randerso    Added sourceId field
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class GfeNotification implements IMessage {
    private static final String localSourceID;
    static {
        String host = SystemUtil.getHostName();
        int pid = SystemUtil.getPid();
        localSourceID = host + ":" + pid;
    }

    @DynamicSerializeElement
    protected String siteID;

    @DynamicSerializeElement
    protected String sourceID;

    public GfeNotification() {
        this.sourceID = localSourceID;
    }

    public GfeNotification(String siteId) {
        this();
        this.siteID = siteId;
    }

    public String getSourceID() {
        return sourceID;
    }

    public void setSourceID(String sourceId) {
        this.sourceID = sourceId;
    }

    public String getSiteID() {
        return siteID;
    }

    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }

    /**
     * 
     * 
     * @return
     */
    public boolean isLocal() {
        return localSourceID.equals(this.sourceID);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.message.IMessage#getHeaders()
     */
    @Override
    public Map<String, Object> getHeaders() {
        Map<String, Object> headers = new HashMap<String, Object>(1);
        headers.put("siteID", getSiteID());
        return headers;
    }
}
