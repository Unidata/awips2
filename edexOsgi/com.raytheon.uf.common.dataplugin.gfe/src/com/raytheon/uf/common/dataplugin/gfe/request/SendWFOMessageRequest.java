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

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Send WFO Message Request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2016  #5374     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class SendWFOMessageRequest extends AbstractGfeRequest {
    /**
     * List destination WFOs
     */
    @DynamicSerializeElement
    private List<String> wfos;

    /**
     * The message to send
     */
    @DynamicSerializeElement
    private String message;

    /**
     * Default constructor for serialization
     */
    public SendWFOMessageRequest() {
        super();
    }

    /**
     * Consructor
     * 
     * @param wfos
     *            list of destination wfos
     * @param message
     *            the message to be sent
     */
    public SendWFOMessageRequest(List<String> wfos, String message) {
        super();
        this.wfos = wfos;
        this.message = message;
    }

    /**
     * @return the wfos
     */
    public List<String> getWfos() {
        return wfos;
    }

    /**
     * @param wfos
     *            the wfos to set
     */
    public void setWfos(List<String> wfos) {
        this.wfos = wfos;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

}
