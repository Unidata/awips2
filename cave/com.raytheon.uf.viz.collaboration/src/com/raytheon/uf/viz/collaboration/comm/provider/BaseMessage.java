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
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class BaseMessage implements Serializable, IMessage {

    private static final long serialVersionUID = 1L;

    private Map<String, String> properties = null;

    private String body;

    private IQualifiedID to;

    private IQualifiedID from;

    private String subject;

    private String status;

    private final long timeStamp;

    /**
     * 
     * @param to
     * @param body
     */
    protected BaseMessage(IQualifiedID to, String body) {
        this.body = body;
        this.to = to;
        initProperties();
        timeStamp = setTimeStamp();
    }

    /**
     * @return the to
     */
    @Override
    public IQualifiedID getTo() {
        return to;
    }

    /**
     * @param to
     *            the to to set
     */
    @Override
    public void setTo(IQualifiedID to) {
        this.to = to;
    }

    /**
     * @return the from
     */
    @Override
    public IQualifiedID getFrom() {
        return from;
    }

    /**
     * @param from
     *            the from to set
     */
    @Override
    public void setFrom(IQualifiedID from) {
        this.from = from;
    }

    /**
     * @param body
     *            the body to set
     */
    @Override
    public void setBody(String body) {
        this.body = body;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getBody()
     */
    @Override
    public String getBody() {
        return body;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getBodyAsBinary()
     */
    @Override
    public byte[] getBodyAsBinary() {
        return null;
    }

    /**
     * 
     */
    private void initProperties() {
        if (properties == null) {
            properties = new HashMap<String, String>();
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#setProperty(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public void setProperty(String key, String value) {
        properties.put(key, value);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getProperty(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public String getProperty(String key, String defaultValue) {
        String retValue = defaultValue;
        if (properties != null) {
            if (properties.containsKey(key)) {
                retValue = properties.get(key);
            }
        }
        return retValue;
    }

    /**
     * Gets the message properties as a collection of key, value pairs. Always
     * returns a not-null value.
     * 
     * @return A Collection of properties associated with this message.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getProperties()
     */
    @Override
    public Collection<Property> getProperties() {
        Collection<Property> p = new ArrayList<Property>();
        for (String s : properties.keySet()) {
            p.add(new Property(s, properties.get(s)));
        }
        return p;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getSubject()
     */
    @Override
    public String getSubject() {
        return subject;
    }

    /**
     * 
     * @param subject
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#setSubject(java.lang.String)
     */
    @Override
    public void setSubject(String subject) {
        this.subject = subject;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getStatus()
     */
    @Override
    public String getStatus() {
        return status;
    }

    /**
     * 
     * @param status
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#setStatus(java.lang.String)
     */
    @Override
    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * Get the receipt time for this message in milliseconds from Jan 1, 1970.
     * 
     * @return The receipt time stamp.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getTimeStamp()
     */
    @Override
    public long getTimeStamp() {
        return timeStamp;
    }

    /**
     * 
     * @return
     */
    private long setTimeStamp() {
        long timestamp = System.currentTimeMillis();
        properties.put(TIMESTAMP, Long.toHexString(timestamp));
        return timestamp;
    }

}
