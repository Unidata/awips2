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

import java.util.Map;

import com.raytheon.uf.common.message.IMessage;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A SendPracticeProductRequest with the addition of an offset time string.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2011            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

@DynamicSerialize
public class PracticeProductOfftimeRequest extends SendPracticeProductRequest
        implements IMessage {

    @DynamicSerializeElement
    private String drtString;

    @DynamicSerializeElement
    private boolean notifyGFE;

    private int offsetSeconds;

    private Map<String, Object> headers;

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
     * @param offsetSeconds
     *            the offsetSeconds to set
     */
    public void setOffsetSeconds(int offsetSeconds) {
        this.offsetSeconds = offsetSeconds;
    }

    /**
     * @return the offsetSeconds
     */
    public int getOffsetSeconds() {
        return offsetSeconds;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.message.IMessage#getHeaders()
     */
    @Override
    public Map<String, Object> getHeaders() {
        return headers;
    }

    /**
     * @param headers
     *            the headers to set
     */
    public void setHeaders(Map<String, Object> headers) {
        this.headers = headers;
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

}
