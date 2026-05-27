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
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Encapsulates messages sent from the server to the client.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/24/08     875        bphillip    Initial Creation
 * 04/24/13     1949       rjpeter     Create lists on demand
 * 06/22/17     6298       mapeters    Added errorResponseList(),
 *                                     errorResponseMap()
 * </pre>
 *
 * @author bphillip
 */
@DynamicSerialize
public class ServerResponse<T> implements ISerializableObject {

    /** Messages indicating an error */
    @DynamicSerializeElement
    private List<ServerMsg> messages = null;

    /** List of return objects from uEngine tasks */
    @DynamicSerializeElement
    private T payload;

    @DynamicSerializeElement
    private List<GfeNotification> notifications = null;

    /**
     * Constructs and empty ServerResponse
     */
    public ServerResponse() {

    }

    public boolean isOkay() {
        return (messages == null) || messages.isEmpty();
    }

    public void addMessage(String message) {
        if (messages == null) {
            messages = new ArrayList<>();
        }

        messages.add(new ServerMsg(message));
    }

    /**
     * Appends another ServerResponse to this
     *
     * @param ssr
     *            The ServerResponse to add
     */
    public void addMessages(ServerResponse<?> ssr) {
        List<ServerMsg> ssrMsgs = ssr.getMessages();
        if (!CollectionUtil.isNullOrEmpty(ssrMsgs)) {
            if (messages == null) {
                messages = new ArrayList<>(ssrMsgs.size());
            }
            messages.addAll(ssrMsgs);
        }

        List<GfeNotification> ssrNotifs = ssr.getNotifications();
        if (!CollectionUtil.isNullOrEmpty(ssrNotifs)) {
            if (notifications == null) {
                notifications = new ArrayList<>(ssrNotifs.size());
            }

            notifications.addAll(ssrNotifs);
        }
    }

    /**
     * Gets the messages
     *
     * @return The messages
     */
    public List<ServerMsg> getMessages() {
        if (messages == null) {
            messages = new ArrayList<>(0);
        }

        return messages;
    }

    /**
     * Sets the message
     *
     * @param messages
     */
    public void setMessages(List<ServerMsg> messages) {
        this.messages = messages;
    }

    /**
     * Returns this ServerResponse as a String
     *
     * @return
     */
    public String message() {
        if (!isOkay()) {
            StringBuilder sb = new StringBuilder();
            for (ServerMsg message : getMessages()) {
                sb.append(message);
                sb.append("\n");
            }
            return sb.toString();
        } else {
            return "";
        }
    }

    @Override
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
        if (notifications == null) {
            notifications = new ArrayList<>(0);
        }

        return notifications;
    }

    public void setNotifications(List<GfeNotification> notifications) {
        this.notifications = notifications;
    }

    public void addNotifications(GfeNotification notify) {
        if (notifications == null) {
            notifications = new ArrayList<>();
        }

        notifications.add(notify);
    }

    /**
     * Return a server response (containing an empty list) indicating that the
     * given error message occurred
     *
     * @param msg
     * @return the server response
     */
    public static <E> ServerResponse<List<E>> errorResponseList(String msg) {
        ServerResponse<List<E>> sr = new ServerResponse<>();
        sr.addMessage(msg);
        sr.setPayload(Collections.emptyList());
        return sr;
    }

    /**
     * Return a server response (containing an empty map) indicating that the
     * given error message occurred
     *
     * @param msg
     * @return the server response
     */
    public static <K, V> ServerResponse<Map<K, V>> errorResponseMap(
            String msg) {
        ServerResponse<Map<K, V>> sr = new ServerResponse<>();
        sr.addMessage(msg);
        sr.setPayload(Collections.emptyMap());
        return sr;
    }
}
