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
package com.raytheon.viz.gfe.core.msgs;

import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public abstract class Message {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(Message.class);
    public static interface IMessageClient {
        public abstract void receiveMessage(Message message);
    }

    private static Comparator<Object> objectComparator = new Comparator<Object>() {

        @Override
        public int compare(Object o1, Object o2) {
            return o1.hashCode() - o2.hashCode();
        }

    };

    /** map of registered clients for each message type */
    private static Map<Class<? extends Message>, Set<IMessageClient>> registeredMap = new java.util.concurrent.ConcurrentSkipListMap<Class<? extends Message>, Set<IMessageClient>>(
            objectComparator);

    /** map of last message sent for each message type */
    private static Map<Class<? extends Message>, Message> lastSentMap = new java.util.concurrent.ConcurrentSkipListMap<Class<? extends Message>, Message>(
            objectComparator);

    public static void registerInterest(IMessageClient client,
            Class<? extends Message>... msgClassList) {

        for (Class<? extends Message> msgClass : msgClassList) {
            Set<IMessageClient> set = registeredMap.get(msgClass);
            if (set == null) {
                set = new ConcurrentSkipListSet<IMessageClient>(
                        objectComparator);
                registeredMap.put(msgClass, set);
            }
            set.add(client);
        }
    }

    public static void unregisterInterest(IMessageClient client,
            Class<? extends Message>... msgClassList) {

        for (Class<? extends Message> msgClass : msgClassList) {
            Set<IMessageClient> set = registeredMap.get(msgClass);
            if (set != null) {
                set.remove(client);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public static <C extends Message> C inquireLastMessage(Class<C> msgClass) {
        try {
            Class.forName(msgClass.getName());
        } catch (ClassNotFoundException e) {
            statusHandler.handle(Priority.CRITICAL, "", e);
        }
        Message message = lastSentMap.get(msgClass);
        if (message == null) {
            try {
                message = msgClass.newInstance();
            } catch (Throwable e) {
                message = null;
            }
        }
        return (C) message;
    }

    public void send() {
        lastSentMap.put(this.getClass(), this);
        Set<IMessageClient> clients = registeredMap.get(this.getClass());
        if (clients != null) {
            for (IMessageClient client : clients) {
                client.receiveMessage(this);
            }
        }
    }

}
