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
package com.raytheon.uf.common.message;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.management.ManagementFactory;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Describes the StatusMessage that is transmitted to alertviz for notifications
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2008  1433       chammack    Initial creation
 * Nov 11,2010  2235       cjeanbap    Added attribute, audioFile.
 * Feb  7,2011  6329       rferrel     Checks to make sure details 
 *                                     and message are never null.
 * Apr 10, 2013 1893       bsteffen    Switch machine to be LOCAL instead of
 *                                     using RuntimeMXBean
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@DynamicSerialize
public class StatusMessage implements ISerializableObject, IMessage {

    private static final int MAX_DETAILS_LENGTH = 32000;

    private static final int MAX_MESSAGE_LENGTH = 1024;

    private static final String LOCAL = "LOCAL";

    /**
     * The source of the message
     */
    @XmlAttribute
    @DynamicSerializeElement
    private String sourceKey;

    /**
     * The category
     */
    @XmlAttribute
    @DynamicSerializeElement
    private String category;

    /**
     * The priority
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Priority priority;

    /**
     * The plugin name
     */
    @XmlAttribute
    @DynamicSerializeElement
    private String plugin;

    /**
     * The machine that generated the message
     */
    @XmlAttribute
    @DynamicSerializeElement
    private String machine;

    /**
     * The message
     */
    @XmlElement
    @DynamicSerializeElement
    private String message;

    /**
     * The details of the message
     */
    @XmlElement
    @DynamicSerializeElement
    private String details;

    /**
     * The event time
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Date eventTime;

    /**
     * Optional: The primary key of the db (used internally)
     */
    private int pk;

    /**
     * The user who acknowledged the message (if applicable)
     */
    private String acknowledgedBy;

    /**
     * When the message was acknowledged (if applicable)
     */
    private Date acknowledgedAt;

    @XmlAttribute
    @DynamicSerializeElement
    private String audioFile;

    /**
     * Default constructor
     */
    public StatusMessage() {
        this.message = "";
        this.details = "";
    }

    /**
     * @param sourceKey
     * @param category
     * @param priority
     * @param plugin
     * @param message
     * @param throwable
     */
    public StatusMessage(String sourceKey, String category, Priority priority,
            String plugin, String message, Throwable throwable) {
        super();
        this.sourceKey = sourceKey;
        this.category = category;
        this.priority = priority;
        this.plugin = plugin;
        this.setMachine(LOCAL);
        buildMessageAndDetails(message, throwable, this);
    }

    public static void buildMessageAndDetails(String msg, Throwable throwable,
            StatusMessage sm) {
        int len = msg.indexOf('\n');
        if (len < 0) {
            len = msg.length();
        }
        if (len > MAX_MESSAGE_LENGTH) {
            len = MAX_MESSAGE_LENGTH;
        }

        // No line breaks, break at reasonable length
        String shortenedMsg = msg.substring(0, len);

        StringBuffer detailsBuffer = new StringBuffer();
        detailsBuffer.append(msg);

        String exception = exceptionToString(throwable);
        if (exception != null) {
            detailsBuffer.append(exception);
        }

        String details = detailsBuffer.toString();
        if (details.length() > MAX_DETAILS_LENGTH) {
            details = details.substring(0, MAX_DETAILS_LENGTH);
        }

        sm.setDetails(details);
        sm.setMessage(shortenedMsg);

    }

    private static String exceptionToString(Throwable t) {
        // Convert Throwable to string
        if (t == null) {
            return null;
        }

        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        t.printStackTrace(pw);
        String details = sw.toString();
        return details;
    }

    /**
     * @return the sourceKey
     */
    public String getSourceKey() {
        return sourceKey;
    }

    /**
     * @param sourceKey
     *            the sourceKey to set
     */
    public void setSourceKey(String sourceKey) {
        this.sourceKey = sourceKey;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * @param category
     *            the category to set
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * @return the priority
     */
    public Priority getPriority() {
        return priority;
    }

    /**
     * @param priority
     *            the priority to set
     */
    public void setPriority(Priority priority) {
        this.priority = priority;
    }

    /**
     * 
     * @return the plugin
     */
    public String getPlugin() {
        return plugin;
    }

    /**
     * 
     * @param plugin
     *            the plugin to set
     */
    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }

    /**
     * @return the machine
     */
    public String getMachine() {
        return machine;
    }

    /**
     * @param machine
     *            the machine to set
     */
    public void setMachine(String machine) {
        this.machine = machine;
    }

    /**
     * Set to the current machine.
     */
    public void setMachineToCurrent() {
        this.machine = ManagementFactory.getRuntimeMXBean().getName();
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
        if (message == null) {
            this.message = "";
        } else {
            this.message = message;
        }
    }

    /**
     * @return the details
     */
    public String getDetails() {
        return details;
    }

    /**
     * @param details
     *            the details to set
     */
    public void setDetails(String details) {
        if (details == null) {
            this.details = "";
        } else {
            this.details = details;
        }
    }

    /**
     * @return the eventTime
     */
    public Date getEventTime() {
        return eventTime;
    }

    /**
     * @param eventTime
     *            the eventTime to set
     */
    public void setEventTime(Date eventTime) {
        this.eventTime = eventTime;
    }

    /**
     * @return the pk
     */
    public int getPk() {
        return pk;
    }

    /**
     * @param pk
     *            the pk to set
     */
    public void setPk(int pk) {
        this.pk = pk;
    }

    /**
     * @return the acknowledgedBy
     */
    public String getAcknowledgedBy() {
        return acknowledgedBy;
    }

    /**
     * @param acknowledgedBy
     *            the acknowledgedBy to set
     */
    public void setAcknowledgedBy(String acknowledgedBy) {
        this.acknowledgedBy = acknowledgedBy;
    }

    /**
     * @return the acknowledgedAt
     */
    public Date getAcknowledgedAt() {
        return acknowledgedAt;
    }

    /**
     * @param acknowledgedAt
     *            the acknowledgedAt to set
     */
    public void setAcknowledgedAt(Date acknowledgedAt) {
        this.acknowledgedAt = acknowledgedAt;
    }

    public String getAudioFile() {
        return audioFile;
    }

    public void setAudioFile(String audioFile) {
        this.audioFile = audioFile;
    }

    @Override
    public Map<String, Object> getHeaders() {
        Map<String, Object> headers = new HashMap<String, Object>();
        headers.put("SOURCE", sourceKey);
        headers.put("PLUGIN", plugin);
        headers.put("CATEGORY", category);
        headers.put("PRIORITY", priority);
        return headers;
    }

    @Override
    public String toString() {
        String statusMsg = getMessage();
        String detailsMsg = getDetails();
        boolean useBothMsgs = detailsMsg.length() > statusMsg.length();
        int bufferLength = useBothMsgs ? statusMsg.length() + 65
                + detailsMsg.length() : statusMsg.length() + 64;
        StringBuilder sb = new StringBuilder(bufferLength);

        if (getCategory() != null) {
            sb.append(getCategory());

            if (getSourceKey() != null) {
                sb.append(": ");
                sb.append(getSourceKey());
            }

            sb.append(" - ");
            sb.append(statusMsg);
            if (useBothMsgs) {
                sb.append("\n");
                sb.append(detailsMsg);
            }
        }
        return sb.toString();
    }
}
