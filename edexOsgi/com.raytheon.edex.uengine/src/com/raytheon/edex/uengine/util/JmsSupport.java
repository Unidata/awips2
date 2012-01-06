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

package com.raytheon.edex.uengine.util;

import javax.jms.Connection;
import javax.jms.DeliveryMode;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Session;

import org.apache.activemq.ActiveMQConnection;
import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.util.IndentPrinter;

import com.raytheon.edex.util.Util;

/**
 * Taken from test.ToolSupport, should probably be cleaned up and moved to common?
 * 
 * @author pheaberl
 *
 */
public class JmsSupport {
    protected Destination destination = null;
    protected String subject = "TOOL.DEFAULT";
    protected boolean topic = false;
    protected String user = ActiveMQConnection.DEFAULT_USER;
    protected String pwd = ActiveMQConnection.DEFAULT_PASSWORD;
    protected String url = ActiveMQConnection.DEFAULT_BROKER_URL;
    protected boolean transacted = false;
    protected boolean durable = false;
    protected String clientID = null;
    protected int ackMode = Session.AUTO_ACKNOWLEDGE;
    protected long timeToLive = 0;

    public JmsSupport() {
        
    }
    
    public JmsSupport(String subject, boolean topic, boolean transacted, boolean durable, String clientID) {
        super();
        this.subject = subject;
        this.topic = topic;
        this.transacted = transacted;
        this.durable = durable;
        this.clientID = clientID;
    }
    
    public JmsSupport(String subject, boolean topic) {
        super();
        this.subject = subject;
        this.topic = topic;
    }

    public Session createSession(Connection connection) throws Exception {
        Session session = connection.createSession(transacted, ackMode);
        if (topic) {
            destination = session.createTopic(subject);
        }
        else {
            destination = session.createQueue(subject);
        }
        return session;
    }

    public Connection createConnection() throws JMSException, Exception {
        ActiveMQConnectionFactory connectionFactory = new ActiveMQConnectionFactory(user, pwd, url);
        Connection connection = connectionFactory.createConnection();
        if (durable && clientID!=null) {
            connection.setClientID(clientID);
        }
        connection.start();
        return connection;
    }

    public MessageProducer createProducer(Session session) throws JMSException {
        MessageProducer producer = session.createProducer(destination);
        if (durable) {
            producer.setDeliveryMode(DeliveryMode.PERSISTENT);
        }
        else {
            producer.setDeliveryMode(DeliveryMode.NON_PERSISTENT);
        }
        if( timeToLive!=0 )
            producer.setTimeToLive(timeToLive);

        return producer;
    }
    
    public MessageConsumer createConsumer(Session session) throws JMSException {
        return createConsumer(session, null, null);
    }
    public MessageConsumer createConsumer(Session session,String selector) throws JMSException {
        return createConsumer(session,null,selector);
    }
    public MessageConsumer createConsumer(Session session, MessageListener msgListener) throws JMSException {
        
        return createConsumer(session,msgListener,null);
    }
    public MessageConsumer createConsumer(Session session, MessageListener msgListener, String selector) throws JMSException {
        MessageConsumer consumer;
        if(Util.isEmptyString(selector)) {
            consumer = session.createConsumer(destination);
        } else {
            consumer = session.createConsumer(destination, selector);
        }
        
        if (msgListener != null) { 
            consumer.setMessageListener(msgListener);
        }        
        return consumer;
    }
    
    public void close(Connection connection, Session session) throws JMSException {
        // lets dump the stats
        dumpStats(connection);

        if (session != null) {
            session.close();
        }
        if (connection != null) {
            connection.close();
        }
    }

    protected void dumpStats(Connection connection) {
        ActiveMQConnection c = (ActiveMQConnection) connection;
        c.getConnectionStats().dump(new IndentPrinter());
    }

    public int getAckMode() {
        return ackMode;
    }

    public void setAckMode(int ackMode) {
        this.ackMode = ackMode;
    }

    public String getClientID() {
        return clientID;
    }

    public void setClientID(String clientID) {
        this.clientID = clientID;
    }

    public Destination getDestination() {
        return destination;
    }

    public void setDestination(Destination destination) {
        this.destination = destination;
    }

    public boolean isDurable() {
        return durable;
    }

    public void setDurable(boolean durable) {
        this.durable = durable;
    }

    public String getPwd() {
        return pwd;
    }

    public void setPwd(String pwd) {
        this.pwd = pwd;
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public boolean isTopic() {
        return topic;
    }

    public void setTopic(boolean topic) {
        this.topic = topic;
    }

    public boolean isTransacted() {
        return transacted;
    }

    public void setTransacted(boolean transacted) {
        this.transacted = transacted;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }
    
}
