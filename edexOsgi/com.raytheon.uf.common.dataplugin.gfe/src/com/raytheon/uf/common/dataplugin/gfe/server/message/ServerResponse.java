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

package com.raytheon.uf.common.dataplugin.gfe.server.message;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Encapsulates messages sent from the server to the client.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/24/08     #875       bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@DynamicSerialize
public class ServerResponse<T> implements ISerializableObject {

    /** Messages indicating an error */
    @DynamicSerializeElement
    private ArrayList<ServerMsg> messages = new ArrayList<ServerMsg>();

    /** List of return objects from uEngine tasks */
    @DynamicSerializeElement
    private T payload;

    @DynamicSerializeElement
    private List<GfeNotification> notifications = new ArrayList<GfeNotification>();

    /**
     * Constructs and empty ServerResponse
     */
    public ServerResponse() {

    }

    public boolean isOkay() {
        return messages.isEmpty();
    }

    public void addMessage(String message) {
        messages.add(new ServerMsg(message));
    }

    /**
     * Appends another ServerResponse to this
     * 
     * @param ssr
     *            The ServerResponse to add
     */
    public void addMessages(ServerResponse<?> ssr) {
        for (ServerMsg message : ssr.getMessages()) {
            messages.add(message);
        }
        for (GfeNotification notify : ssr.getNotifications()) {
            notifications.add(notify);
        }
    }

    /**
     * Gets the messages
     * 
     * @return The messages
     */
    public ArrayList<ServerMsg> getMessages() {
        return messages;
    }

    /**
     * Sets the message
     * 
     * @param messages
     */
    public void setMessages(ArrayList<ServerMsg> messages) {
        this.messages = messages;
    }

    /**
     * Returns this ServerResponse as a String
     * 
     * @return
     */
    public String message() {
        if (!isOkay()) {
            StringBuffer buf = new StringBuffer();
            for (ServerMsg message : messages) {
                buf.append(message);
                buf.append("\n");
            }
            return buf.toString();
        } else {
            return "";
        }
    }

    public String toString() {
        return message();
    }

    public T getPayload() {
        return payload;
    }

    public void setPayload(T payload) {
        this.payload = payload;
    }

    public List<GfeNotification> getNotifications() {
        return notifications;
    }

    public void setNotifications(List<GfeNotification> notifications) {
        this.notifications = notifications;
    }

    public void addNotifications(GfeNotification notify) {
        this.notifications.add(notify);
    }
}
