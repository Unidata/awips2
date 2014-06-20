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
package com.raytheon.uf.viz.collaboration.comm.provider.event;

import com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;

/**
 * Event fired when a text message is received. Carries TextMessage with
 * optional error
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            jkorman     Initial creation
 * Jun 20, 2014 3281       bclement    added error field
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ChatMessageEvent implements ITextMessageEvent {

    private final TextMessage message;

    private String error;

    /**
     * @param msg
     */
    public ChatMessageEvent(TextMessage msg) {
        this(msg, null);
    }

    /**
     * @param msg
     * @param error
     */
    public ChatMessageEvent(TextMessage msg, String error) {
        message = msg;
        this.error = error;
    }

    /**
     * 
     * @return
     * 
     */
    public TextMessage getMessage() {
        return message;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent
     * #hasError()
     */
    @Override
    public boolean hasError() {
        return error != null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent
     * #getError()
     */
    @Override
    public String getError() {
        return error;
    }

    /**
     * @param error
     *            the error to set
     */
    public void setError(String error) {
        this.error = error;
    }

}
