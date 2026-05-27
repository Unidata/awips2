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

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.message.IMessage;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * A SendPracticeProductRequest with an optional offset time string.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2011            wldougher   Initial creation
 * Nov 14, 2014  4953      randerso    Merged PracticeProductOfftimeRequest with this
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

@DynamicSerialize
public class SendPracticeProductRequest implements IServerRequest, IMessage {

    @DynamicSerializeElement
    private String productText;

    @DynamicSerializeElement
    private String drtString;

    @DynamicSerializeElement
    private boolean notifyGFE;

    /**
     * @return the productText
     */
    public String getProductText() {
        return productText;
    }

    /**
     * @param productText
     *            the productText to set
     */
    public void setProductText(String productText) {
        this.productText = productText;
    }

    /**
     * @return the drtString
     */
    public String getDrtString() {
        return drtString;
    }

    /**
     * @param drtString
     *            the drtString to set
     */
    public void setDrtString(String drtString) {
        this.drtString = drtString;
    }

    /**
     * Set the flag value that tells whether GFE notifications should be sent.
     * 
     * @param notifyGFE
     *            the notifyGFE to set
     */
    public void setNotifyGFE(boolean notifyGFE) {
        this.notifyGFE = notifyGFE;
    }

    /**
     * @return the notifyGFE
     */
    public boolean isNotifyGFE() {
        return notifyGFE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.message.IMessage#getHeaders()
     */
    @Override
    public Map<String, Object> getHeaders() {
        Map<String, Object> headers = new HashMap<String, Object>(2, 1.0f);
        headers.put("drtstring", getDrtString());
        headers.put("notifygfe", isNotifyGFE());
        return headers;
    }

}
