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
package com.raytheon.viz.ui.statusline;

import java.text.SimpleDateFormat;
import java.util.Date;

import com.raytheon.uf.common.time.SimulatedTime;

/**
 * TODO Add Description Message.java May 19, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	May 19, 2008					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class StatusMessage {

    // Regular
    // Significant
    // Urgent
    // Alert

    private SimpleDateFormat messageTimeFormat = new SimpleDateFormat(
            "yy/mm/dd hh:mm:ss");

    public static enum Importance {
        REGULAR, SIGNIFICANT, URGENT, ALERT, ALERT1, ALERT2, EXPIRED;

        @Override
        public String toString() {
            return name().substring(0, 1);
        }
    };

    public static enum MessageType {
        NORMAL, ISC_INIT, ALL
    }

    private String messageText;

    private Date messageDate;

    private Importance importance;

    private boolean messageAck;

    public StatusMessage(String messageText, Importance importance) {
        this.messageText = messageText;
        this.importance = importance;
        messageDate = SimulatedTime.getSystemTime().getTime();
    }

    public String getMessageText() {
        return messageText;
    }

    public void setMessageText(String messageText) {
        this.messageText = messageText;
    }

    public Date getMessageDate() {
        return messageDate;
    }

    public void setMessageDate(Date messageDate) {
        this.messageDate = messageDate;
    }

    public Importance getImportance() {
        return importance;
    }

    public void setImportance(Importance importance) {
        this.importance = importance;
    }

    public boolean isMessageAck() {
        return messageAck;
    }

    public void setMessageAck(boolean messageAck) {
        this.messageAck = messageAck;
    }

    @Override
    public String toString() {
        return messageTimeFormat.format(getMessageDate()) + " "
                + getMessageText();

    }
}
