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
package com.raytheon.uf.viz.alertviz;

import java.io.PrintStream;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Deque;
import java.util.Iterator;
import java.util.concurrent.LinkedBlockingDeque;

import javax.jms.JMSException;
import javax.jms.TextMessage;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.slf4j.Marker;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.ThrowableProxy;

import com.raytheon.uf.common.logback.appender.JmsQueueAppender;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.status.slf4j.UFMarkers;

/**
 * Sends a log event as a StatusMessage to a JMS queue. Typically this is
 * sending to the separate process of AlertViz.
 * 
 * Also queues up messages if it cannot make the connection, hoping to send them
 * later. If the queue reaches its limit, it will begin to empty the queue into
 * the console to attempt to retain some level of logging and traceability.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2015 4473       njensen     Initial creation
 *
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class StatusMessageAppender extends JmsQueueAppender {

    // TODO how many messages do we want to hold in memory if AlertViz is down?
    protected Deque<StatusMessage> queued = new LinkedBlockingDeque<>(100);

    protected SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss");

    protected JAXBContext jaxbContext;

    protected Marshaller marshaller;

    /** a boolean used to ensure we don't overprint the queue warning */
    protected boolean printQueueWarning = true;

    public StatusMessageAppender() throws JAXBException {
        jaxbContext = JAXBContext.newInstance(StatusMessage.class);
        marshaller = jaxbContext.createMarshaller();
    }

    @Override
    protected void append(ILoggingEvent event) {
        StatusMessage sm = buildStatusMessage(event);
        if (sm != null) {
            boolean connected = isConnected();
            if (!isConnected()) {
                connected = connect();
                if (connected) {
                    System.err
                            .println("StatusMessageAppender established connection to AlertViz");
                    printQueueWarning = true;
                }
            }

            // check again to make sure it connected
            if (connected) {
                /*
                 * first let's process any backed up messages to keep them in
                 * order
                 */
                StatusMessage waiting = queued.peekFirst();
                while (waiting != null) {
                    try {
                        sendMessage(waiting);
                        queued.pop();
                        waiting = queued.peekFirst();
                    } catch (Exception e) {
                        /*
                         * failed to send, instead of logging them all let's
                         * just give up by breaking out, then let the trigger
                         * message complain below
                         */
                        break;
                    }
                }

                /*
                 * now let's try the original message we received
                 */
                try {
                    sendMessage(sm);
                } catch (JAXBException | JMSException e) {
                    /*
                     * Let's queue this one up and disconnect so the next
                     * message will try to reconnect. If the original message to
                     * be sent was of high enough priority, we'll print the
                     * current [most likely JMS]Exception to the console so
                     * someone (?) will know that messages aren't getting sent
                     * for a bit.
                     */
                    switch (sm.getPriority()) {
                    case CRITICAL:
                    case SIGNIFICANT:
                    case PROBLEM:
                        e.printStackTrace();
                        break;
                    default: // do nothing
                        break;
                    }
                    disconnect();
                    connected = false;
                }
            }

            if (!connected) {
                if (printQueueWarning) {
                    System.err
                            .println("StatusMessageAppender couldn't connect to AlertViz, queueing message(s)");
                    printQueueWarning = false;
                }
                // Lost our connection or it's still coming up....
                queueMessage(sm);
            }
        }
    }

    /**
     * Sends a message as a JMS TextMessage of an XML representation of the
     * StatusMessage.
     * 
     * @param sm
     * @throws JAXBException
     * @throws JMSException
     * @throws Exception
     */
    protected void sendMessage(StatusMessage sm) throws JAXBException,
            JMSException {
        StringWriter sw = new StringWriter();
        marshaller.marshal(sm, sw);
        TextMessage message = session.createTextMessage();
        message.setText(sw.toString());
        producer.send(message);
    }

    /**
     * Queues a message to be processed later in the hopes that the connection
     * comes back. If the queue limit is reached, this method will start popping
     * messages off the front of the queue and sending them to the console to
     * make room for newer messages.
     * 
     * @param sm
     */
    protected void queueMessage(StatusMessage sm) {
        while (!queued.offerLast(sm)) {
            /*
             * queue is full, let's only keep the more recent ones and start
             * logging older messages to the console
             */
            StatusMessage old = queued.pollFirst();
            if (old != null) {
                PrintStream ps = null;
                switch (old.getPriority()) {
                case CRITICAL:
                case SIGNIFICANT:
                case PROBLEM:
                    ps = System.err;
                    break;
                default:
                    ps = System.out;
                    break;
                }
                ps.println(sdf.format(old.getEventTime()) + "  "
                        + old.toString());
            }
        }
    }

    /**
     * Builds a StatusMessage from an ILoggingEvent. The ILoggingEvent is
     * expected to have certain markers on it to properly create the
     * StatusMessage, otherwise some default values may be used.
     * 
     * @param event
     * @return
     */
    protected StatusMessage buildStatusMessage(ILoggingEvent event) {
        Level level = event.getLevel();
        String msg = event.getMessage();
        ThrowableProxy tp = (ThrowableProxy) event.getThrowableProxy();
        Throwable t = null;
        if (tp != null) {
            t = tp.getThrowable();
        }

        String source = null;
        String category = null;
        String plugin = null;
        String priority = null;
        Marker m = event.getMarker();
        if (m != null) {
            category = getMarkerValue(m, UFMarkers.CATEGORY_START);
            source = getMarkerValue(m, UFMarkers.SOURCE_START);
            plugin = getMarkerValue(m, UFMarkers.PLUGIN_START);
            priority = getMarkerValue(m, UFMarkers.PRIORITY_START);
        }

        /*
         * If it came through UFStatus a marker will indicate the priority,
         * otherwise if it came through SLF4J directly we will infer the
         * priority.
         */
        Priority p = null;
        if (priority != null) {
            p = Priority.valueOf(priority);
        } else {
            switch (level.toInt()) {
            case Level.ERROR_INT:
                p = Priority.ERROR;
                break;
            case Level.WARN_INT:
                p = Priority.WARN;
                break;
            case Level.INFO_INT:
                p = Priority.INFO;
                break;
            case Level.DEBUG_INT:
            case Level.TRACE_INT:
                p = Priority.DEBUG;
                break;
            default:
                // shouldn't ever get here
                System.err
                        .println("StatusMessageAppender received unsupported Level "
                                + level);
                System.err.println(msg);
                if (t != null) {
                    t.printStackTrace();
                }
                return null;
            }
        }

        StatusMessage sm = new StatusMessage(source, category, p, plugin, msg,
                t);
        sm.setMachineToCurrent();
        return sm;
    }

    /**
     * Tries to find a marker value based on the concept of a key value pair.
     * Checks the Marker argument and any children markers recursively.
     * 
     * For example, if the Marker had children or was named PLUGIN=XYZ, and the
     * start arg was PLUGIN=, then this method would return XYZ.
     * 
     * @param m
     * @param start
     * @return
     */
    private String getMarkerValue(Marker m, String start) {
        return getMarkerValue(m, start, start.length());
    }

    /**
     * Tries to find a marker value based on the concept of a key value pair.
     * Checks the Marker argument and any children markers recursively.
     * 
     * For example, if the Marker had children or was named PLUGIN=XYZ, and the
     * start arg was PLUGIN= and startLen 7, then this method would return XYZ.
     * 
     * startLen should always match start.length(), passing it along is purely
     * an optimization since the method may go recursive.
     * 
     * @param m
     * @param start
     * @param startLen
     * @return
     */
    @SuppressWarnings("unchecked")
    private String getMarkerValue(Marker m, String start, int startLen) {
        String value = null;
        String name = m.getName();
        if (name.startsWith(start)) {
            value = name.substring(startLen);
        }
        if (value == null) {
            Iterator<Marker> itr = m.iterator();
            while (itr.hasNext() && value == null) {
                value = getMarkerValue(itr.next(), start, startLen);
            }
        }

        return value;
    }

    @Override
    public void onException(JMSException e) {
        super.onException(e);
        System.out.println("Will attempt to reconnect on next message");
    }

}
