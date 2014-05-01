/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.console;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.UUID;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.codec.BBEncoder;
import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;

public class Broker implements MessageListener
{
    class HeaderInfo
    {
        boolean valid;
        long sequence;
        char opcode;

        public String toString()
        {
            return String.format("%s Header with opcode %s and sequence %s",
                    (valid ? "Valid" : "Invalid"), opcode, sequence);
        }
    }

    private static Logger log = LoggerFactory.getLogger(Broker.class);
    public static int SYNC_TIME = 60000;
    // JMS Stuff
    private javax.jms.Session session;
    boolean sessionTransacted = false;
    private String replyName;
    private String topicName;
    private MessageProducer prod;
    private ArrayList<MessageConsumer> consumers = new ArrayList<MessageConsumer>();
    private Queue reply;
    private Queue topic;
    private int acknowledgeMode = javax.jms.Session.AUTO_ACKNOWLEDGE;
    // QMF Stuff
    AMQConnection connection;
    public String url;
    public java.util.HashMap<String, Agent> Agents = new java.util.HashMap<String, Agent>();
    private Session consoleSession;
    private boolean connected = false;
    private boolean syncInFlight = false;
    private boolean topicBound = false;
    private int reqsOutstanding = 0;
    private Object lockObject = new Object();
    UUID brokerId = UUID.randomUUID();

    public Broker(org.apache.qpid.console.Session session, String url)
    {
        log.debug("Creating a new Broker for url " + url);
        this.url = url;
        consoleSession = session;
        this.tryToConnect();
    }

    public int brokerBank()
    {
        return 1;
    }

    protected HeaderInfo CheckHeader(Decoder decoder)
    {
        HeaderInfo returnValue = new HeaderInfo();
        returnValue.opcode = 'x';
        returnValue.sequence = -1;
        returnValue.valid = false;
        if (decoder.hasRemaining())
        {
            char character = (char) decoder.readUint8();
            if (character != 'A')
            {
                return returnValue;
            }
            character = (char) decoder.readUint8();
            if (character != 'M')
            {
                return returnValue;
            }
            character = (char) decoder.readUint8();
            if (character != '2')
            {
                return returnValue;
            }
            returnValue.valid = true;
            returnValue.opcode = (char) decoder.readUint8();
            returnValue.sequence = decoder.readUint32();
        }
        return returnValue;
    }

    public Encoder createEncoder(char opcode, long sequence)
    {
        return setHeader(new BBEncoder(1024), opcode, sequence);
    }

    public Message createMessage(Encoder enc)
    {
        try
        {
            byte[] buf = new byte[1024];
            byte[] body = new byte[1024];
            BBEncoder bbenc = (BBEncoder) enc;
            BytesMessage msg = session.createBytesMessage();
            ByteBuffer slice = bbenc.buffer();
            while (slice.hasRemaining())
            {
                int n = Math.min(buf.length, slice.remaining());
                slice.get(buf, 0, n);
                msg.writeBytes(buf, 0, n);
            }
            return msg;
        } catch (JMSException e)
        {
            throw new ConsoleException(e);
        }
    }

    public void decrementOutstanding()
    {
        synchronized (lockObject)
        {
            this.reqsOutstanding -= 1;
            if ((reqsOutstanding == 0) & !topicBound)
            {
                for (String key : consoleSession.bindingKeys())
                {
                    try
                    {
                        // this.clientSession.exchangeBind(topicName,
                        // "qpid.mannagement", key) ;
                        log.debug("Setting Topic Binding " + key);
                        // topicName = "management://qpid.management//" + key;
                        String rk = String.format("&routingkey='%s'", key);
                        Queue aQueue = session.createQueue(topicName + rk);
                        MessageConsumer cons = session.createConsumer(aQueue);
                        cons.setMessageListener(this);
                        consumers.add(cons);
                    } catch (JMSException e)
                    {
                        throw new ConsoleException(e);
                    }
                }
                topicBound = true;
            }
            if ((reqsOutstanding == 0) & syncInFlight)
            {
                syncInFlight = false;
                lockObject.notifyAll();
            }
        }
    }

    private byte[] ensure(int capacity, byte[] body, int size)
    {
        if (capacity > body.length)
        {
            byte[] copy = new byte[capacity];
            System.arraycopy(body, 0, copy, 0, size);
            body = copy;
        }
        return body;
    }

    protected void finalize()
    {
        if (connected)
        {
            this.shutdown();
        }
    }

    public boolean getSyncInFlight()
    {
        return syncInFlight;
    }

    public void incrementOutstanding()
    {
        synchronized (lockObject)
        {
            this.reqsOutstanding += 1;
        }
    }

    public boolean isConnected()
    {
        return connected;
    }

    public void onMessage(Message msg)
    {
        Decoder decoder = readBody(msg);
        HeaderInfo headerInfo = this.CheckHeader(decoder);
        // log.debug(headerInfo.toString());
        while (headerInfo.valid)
        {
            long seq = headerInfo.sequence;
            switch (headerInfo.opcode)
            {
            case 'b':
                consoleSession.handleBrokerResponse(this, decoder, seq);
                break;
            case 'p':
                consoleSession.handlePackageIndicator(this, decoder, seq);
                break;
            case 'z':
                consoleSession.handleCommandComplete(this, decoder, seq);
                break;
            case 'q':
                consoleSession.handleClassIndicator(this, decoder, seq);
                break;
            case 'm':
                consoleSession.handleMethodResponse(this, decoder, seq);
                break;
            case 'h':
                consoleSession
                        .handleHeartbeatIndicator(this, decoder, seq, msg);
                break;
            case 'e':
                consoleSession.handleEventIndicator(this, decoder, seq);
                break;
            case 's':
                consoleSession.handleSchemaResponse(this, decoder, seq);
                break;
            case 'c':
                consoleSession.handleContentIndicator(this, decoder, seq, true,
                        false);
                break;
            case 'i':
                consoleSession.handleContentIndicator(this, decoder, seq,
                        false, true);
                break;
            case 'g':
                consoleSession.handleContentIndicator(this, decoder, seq, true,
                        true);
                break;
            default:
                log.error("Invalid message type recieved with opcode "
                        + headerInfo.opcode);
                break;
            }
            headerInfo = this.CheckHeader(decoder);
        }
    }

    private Decoder readBody(Message message)
    {
        BytesMessage msg = (BytesMessage) message;
        BBDecoder dec = new BBDecoder();
        byte[] buf = new byte[1024];
        byte[] body = new byte[1024];
        int size = 0;
        int n;
        try
        {
            while ((n = msg.readBytes(buf)) > 0)
            {
                body = ensure(size + n, body, size);
                System.arraycopy(buf, 0, body, size, n);
                size += n;
            }
        } catch (JMSException e)
        {
            throw new ConsoleException(e);
        }
        dec.init(ByteBuffer.wrap(body, 0, size));
        return dec;
    }

    public void send(Encoder enc)
    {
        this.send(this.createMessage(enc), "broker");
    }

    public void send(Message msg)
    {
        this.send(msg, "broker", -1);
    }

    public void send(Message msg, String routingKey)
    {
        this.send(msg, routingKey, -1);
    }

    public void send(Message msg, String routingKey, int ttl)
    {
        synchronized (lockObject)
        {
            try
            {
                log.debug(String.format("Sending message to routing key '%s'",
                        routingKey));
                String destName = String.format(
                        "management://qpid.management//?routingkey='%s'",
                        routingKey);
                log.debug(destName);
                Queue dest = session.createQueue(destName);
                // Queue jmsReply = session
                // createQueue("direct://amq.direct//?routingkey='reply-"
                // + brokerId + "'");
                if (ttl != -1)
                {
                    msg.setJMSExpiration(ttl);
                }
                msg.setJMSReplyTo(reply);
                prod.send(dest, msg);
            } catch (Exception e)
            {
                throw new ConsoleException(e);
            }
        }
    }

    protected Encoder setHeader(Encoder enc, char opcode, long sequence)
    {
        enc.writeUint8((short) 'A');
        enc.writeUint8((short) 'M');
        enc.writeUint8((short) '2');
        enc.writeUint8((short) opcode);
        enc.writeUint32(sequence);
        return enc;
    }

    public void setSyncInFlight(boolean inFlight)
    {
        synchronized (lockObject)
        {
            syncInFlight = inFlight;
            lockObject.notifyAll();
        }
    }

    public void shutdown()
    {
        if (connected)
        {
            this.waitForStable();
            try
            {
                session.close();
                for (MessageConsumer cons : consumers)
                {
                    cons.close();
                }
                connection.close();
            } catch (Exception e)
            {
                throw new ConsoleException(e);
            } finally
            {
                this.connected = false;
            }
        }
    }

    protected void tryToConnect()
    {
        try
        {
            reqsOutstanding = 1;
            Agent newAgent = new Agent(this, 0, "BrokerAgent");
            Agents.put(newAgent.agentKey(), newAgent);
            connection = new AMQConnection(url);
            session = connection.createSession(sessionTransacted,
                    acknowledgeMode);
            replyName = String
                    .format(
                            "direct://amq.direct//reply-%s?exclusive='True'&autodelete='True'",
                            brokerId);
            topicName = String
                    .format(
                            "management://qpid.management//topic-%s?exclusive='True'&autodelete='True'",
                            brokerId);
            reply = session.createQueue(replyName);
            MessageConsumer cons = session.createConsumer(reply);
            cons.setMessageListener(this);
            consumers.add(cons);
            prod = session.createProducer(null);
            topic = session.createQueue(topicName);
            cons = session.createConsumer(topic);
            cons.setMessageListener(this);
            consumers.add(cons);
            connection.start();
            // Rest of the topic is bound later. Start er up
        } catch (Exception e)
        {
            throw new ConsoleException(e);
        }
        connected = true;
        consoleSession.handleBrokerConnect(this);
        Encoder Encoder = createEncoder('B', 0);
        this.send(Encoder);
    }

    public void updateAgent(QMFObject obj)
    {
        long agentBank = (Long) obj.getProperty("agentBank");
        long brokerBank = (Long) obj.getProperty("brokerBank");
        String key = Agent.AgentKey(agentBank, brokerBank);
        if (obj.isDeleted())
        {
            if (Agents.containsKey(key))
            {
                Agent agent = Agents.get(key);
                Agents.remove(key);
                consoleSession.handleAgentRemoved(agent);
            }
        } else
        {
            if (!Agents.containsKey(key))
            {
                Agent newAgent = new Agent(this, agentBank, (String) obj
                        .getProperty("label"));
                Agents.put(key, newAgent);
                consoleSession.handleNewAgent(newAgent);
            }
        }
    }

    public void waitForStable()
    {
        synchronized (lockObject)
        {
            if (connected)
            {
                long start = System.currentTimeMillis();
                syncInFlight = true;
                while (reqsOutstanding != 0)
                {
                    log.debug("Waiting to recieve messages");
                    try
                    {
                        lockObject.wait(SYNC_TIME);
                    } catch (Exception e)
                    {
                        throw new ConsoleException(e);
                    }
                    long duration = System.currentTimeMillis() - start;
                    if (duration > SYNC_TIME)
                    {
                        throw new ConsoleException(
                                "Timeout waiting for Broker to Sync");
                    }
                }
            }
        }
    }

    public void waitForSync(int timeout)
    {
        synchronized (lockObject)
        {
            long start = System.currentTimeMillis();
            while (syncInFlight)
            {
                try
                {
                    lockObject.wait(SYNC_TIME);
                } catch (Exception e)
                {
                    throw new ConsoleException(e);
                }
            }
            long duration = System.currentTimeMillis() - start;
            if (duration > timeout)
            {
                throw new ConsoleException("Timeout waiting for Broker to Sync");
            }
        }
    }
}
