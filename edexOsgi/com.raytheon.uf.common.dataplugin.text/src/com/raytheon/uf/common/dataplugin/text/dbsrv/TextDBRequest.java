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
package com.raytheon.uf.common.dataplugin.text.dbsrv;

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Object used by thrift clients to make textdb requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2014 2926       bclement     Initial creation
 * Sep 05, 2014 2926       bclement     moved to common
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@DynamicSerialize
public class TextDBRequest implements IServerRequest {

    @DynamicSerializeElement
    private Message message;

    /**
     * 
     */
    public TextDBRequest() {
    }

    /**
     * @param message
     */
    public TextDBRequest(Message message) {
        this.message = message;
    }

    /**
     * @return the message
     */
    public Message getMessage() {
        return message;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(Message message) {
        this.message = message;
    }

}
