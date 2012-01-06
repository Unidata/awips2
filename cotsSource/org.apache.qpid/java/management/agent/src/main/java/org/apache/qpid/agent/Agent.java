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
package org.apache.qpid.agent;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.agent.binding.BindingContext;
import org.apache.qpid.agent.binding.BindingUtils;
import org.apache.qpid.agent.binding.ClassBinding;
import org.apache.qpid.agent.binding.BindingException;
import org.apache.qpid.agent.binding.MethodBinding;
import org.apache.qpid.agent.binding.ParameterBinding;
import org.apache.qpid.agent.binding.PropertyBinding;
import org.apache.qpid.agent.binding.TypeBinding;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.codec.BBEncoder;
import org.apache.qpid.transport.codec.Decoder;

/**
 * The main class for interacting with the QMF bus. Objects which are to be
 * managed can be registered with the agent, as can classes to be exposed via
 * the schema.
 */
public class Agent implements MessageListener
{
    // The following are settings to configure the Agent
    protected AMQConnection connection;
    protected boolean sessionTransacted = false;
    protected int acknowledgeMode = Session.AUTO_ACKNOWLEDGE;
    protected String label;
    protected UUID systemId;
    // this list holds the objects until the agent is started
    protected List managedObjects = new ArrayList();
    protected List registeredClasses = new ArrayList();
    // The following instance variables are not
    // able to be set by the end user.
    protected Session session;
    protected MessageProducer prod;
    protected MessageConsumer cons;
    protected Queue reply;
    protected BindingContext bctx = new BindingContext();
    protected Map<Long, ManagedObject> objects = new Hashtable<Long, ManagedObject>();
    protected long bbank;
    protected long abank;
    protected static Log log = LogFactory.getLog(Agent.class);
    protected volatile boolean inside = false;
    protected ClassLoader classLoader = null;

    public Agent()
    {
        systemId = UUID.randomUUID();
        log.debug(String.format("Agent with uid %s created", systemId
                .toString()));
    }

    public Agent(String label, UUID systemId)
    {
        this.systemId = systemId;
        this.label = label;
        log.debug(String.format("Agent with name %s and uid %s created", label,
                systemId.toString()));
    }

    public void register(ManagedObject managedObject)
    {
        Class managedClass = managedObject.getObjectClass();
        long id = managedObject.getId();
        ClassBinding cb = bctx.register(managedClass);
        managedObject.setManagedClassName(cb.getName());
        managedObject.setManagedPackageName(cb.getPackage());
        log.debug(String.format(
                "Added managed object id '%d' for package '%s' class '%s'", id,
                managedObject.getManagedPackageName(), managedObject
                        .getManagedClassName()));
        objects.put(id, managedObject);
        managedObjects.add(managedObject);
    }

    public void registerClass(Class cls)
    {
        bctx.register(cls);
        if (!registeredClasses.contains(cls))
        {
            registeredClasses.add(cls);
        }
    }

    /**
     * Stops the agents connection to the bus
     */
    public void stop()
    {
        try
        {
            cons.close();
            prod.close();
            connection.stop();
            connection.close();
            session.close();
        } catch (JMSException e)
        {
            log.error("Exception:", e);
        }
    }

    /**
     * Starts up the agent. Many bean containers may call this by default which
     * aids in deployment
     */
    public void start()
    {
        log.debug(String.format("Agent with uid %s and name %s starting",
                systemId.toString(), label));
        for (Object clsName : registeredClasses.toArray())
        {
            try
            {
                Class cls = null;
                if (String.class.isAssignableFrom(clsName.getClass()))
                {
                    cls = getClass(clsName.toString());
                } else
                {
                    cls = (Class) clsName;
                }
                this.registerClass(cls);
            } catch (Exception e)
            {
                log.error("Could not register class " + clsName);
            }
        }
        for (Object obj : managedObjects.toArray())
        {
            this.register((ManagedObject) obj);
        }
        try
        {
            session = connection.createSession(sessionTransacted,
                    acknowledgeMode);
            reply = session
                    .createQueue(String
                            .format(
                                    "direct://amq.direct//%s-%s?exclusive='True'&autodelete='True'",
                                    label, systemId));
            cons = session.createConsumer(reply);
            cons.setMessageListener(this);
            prod = session.createProducer(null);
        } catch (JMSException e)
        {
            throw new AgentException(e);
        }
        attachRequest(label, systemId);
        try
        {
            connection.start();
        } catch (JMSException e)
        {
            throw new AgentException(e);
        }
    }

    /**
     * Send an event object to the bus
     */
    public void raiseEvent(Object value, EventSeverity sev)
    {
        log.debug(String.format("Sending event of class %s with Severity %s",
                value.getClass(), sev.ordinal()));
        BBEncoder enc = this.init('e');
        ClassBinding cb = bctx.getClassBinding(value.getClass());
        String pkg = cb.getPackage();
        String cls = cb.getName();
        enc.writeStr8(pkg);
        enc.writeStr8(cls);
        enc.writeBin128(cb.getSchemaHash());
        long now = System.currentTimeMillis() * 1000000;
        enc.writeInt64(now);
        enc.writeUint8((short) sev.ordinal());
        for (PropertyBinding p : cb.getProperties())
        {
            p.getType().encode(enc, BindingUtils.get(p, value));
        }
        send(
                String.format("console.event.%d.%d.%s.%s", bbank, abank, pkg,
                        cls), enc);
    }

    public void onMessage(Message message)
    {
        if (inside)
        {
            new Throwable().printStackTrace();
        }
        inside = true;
        Decoder dec = readBody(message);
        Destination replyTo;
        try
        {
            replyTo = message.getJMSReplyTo();
        } catch (JMSException e)
        {
            throw new AgentException(e);
        }
        byte[] magic = dec.readBytes(3);
        if (magic[0] != 'A' || magic[1] != 'M' || magic[2] != '2')
        {
            throw new AgentException("bad magic: " + new String(magic));
        }
        short op = dec.readUint8();
        long seq = dec.readUint32();
        log.debug("Message recieved: " + (char) op);
        switch (op)
        {
        case 'a':
            this.handleAgentAttach(seq, replyTo, dec);
            break;
        case 'G':
            this.handleGetQuery(seq, replyTo, dec);
            break;
        case 'M':
            this.handleMethodRequest(seq, replyTo, dec);
            break;
        case 'S':
            this.handleSchemaRequest(seq, replyTo, dec);
            break;
        case 'x':
            // TODO
            break;
        default:
            throw new IllegalArgumentException("opcode: " + ((char) op));
        }
        inside = false;
    }

    protected ClassBinding getClassBinding(ManagedObject mobj)
    {
        return bctx.getClassBinding(mobj.getObjectClass());
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
            throw new AgentException(e);
        }
        dec.init(ByteBuffer.wrap(body, 0, size));
        return dec;
    }

    protected void handleAgentAttach(long seq, Destination replyTo, Decoder dec)
    {
        log.debug("Agent Attach Message");
        bbank = dec.readUint32();
        abank = dec.readUint32();
        try
        {
            MessageConsumer mc = session
                    .createConsumer(session
                            .createQueue(String
                                    .format(
                                            "management://qpid.management//%s-%s?routingkey='agent.%d.%d'&exclusive='True'&autodelete='True'",
                                            label, systemId, bbank, abank)));
            mc.setMessageListener(this);
        } catch (JMSException e)
        {
            throw new AgentException(e);
        }
        for (String packageName : bctx.getPackages())
        {
            packageIndication(packageName);
        }
        for (ClassBinding cb : bctx.getAllBindings())
        {
            classIndication(cb);
        }
        for (ManagedObject mo : objects.values())
        {
            content('i', seq, null, mo);
        }
    }

    protected void handleMethodRequest(long seq, Destination replyTo,
            Decoder dec)
    {
        dec.readUint64(); // first part of object-id
        long id = dec.readUint64();
        ManagedObject mo = objects.get(id);
        if (mo == null)
        {
            methodResponse(seq, replyTo, 1, String.format(
                    "no such object: 0x%x", id));
        } else
        {
            dec.readStr8(); // pkg
            dec.readStr8(); // cls
            dec.readBin128(); // hash
            String mname = dec.readStr8();
            ClassBinding cls = getClassBinding(mo);
            MethodBinding method = cls.getMethod(mname);
            if (method == null)
            {
                methodResponse(seq, replyTo, 2, String.format(
                        "no such method: %s", mname));
            } else
            {
                log.trace("Handle method: " + method.getName());
                List<ParameterBinding> params = method.getInParameters();
                Object[] args = new Object[params.size()];
                int idx = 0;
                for (ParameterBinding p : params)
                {
                    TypeBinding typeBinding = p.getType();
                    log
                            .trace(String
                                    .format(
                                            "Decoding parameter with type %s ref package %s ref class %s ",
                                            typeBinding.getCode(), typeBinding
                                                    .getRefPackage(),
                                            typeBinding.getRefClass()));
                    args[idx++] = typeBinding.decode(dec);
                    log.trace("Done");
                }
                try
                {
                    Object[] result = mo.invoke(method, args);
                    methodResponse(seq, replyTo, 0, null, method, result);
                } catch (BindingException ex)
                {
                    log
                            .error(
                                    String
                                            .format(
                                                    "An exception occured invoking method %s. Stack trace sent to console.",
                                                    method.getName()), ex);
                    StringWriter str = new StringWriter();
                    PrintWriter writer = new PrintWriter(str);
                    ex.printStackTrace(writer);
                    writer.flush();
                    methodResponse(seq, replyTo, 7, str.toString());
                }
                log.trace("Done with method: " + method.getName());
            }
        }
    }

    protected void handleGetQuery(long seq, Destination replyTo, Decoder dec)
    {
        Map<String, Object> data = dec.readMap();
        if (data.containsKey("_objectid"))
        {
            long objId = (Long) data.get("_objectid");
            log.debug("Get Request message for object id " + objId);
            ManagedObject mo = objects.get(objId);
            if (mo == null)
            {
                methodResponse(seq, replyTo, 1, String.format(
                        "no such object: 0x%x", objId));
            } else
            {
                content('g', seq, replyTo, mo);
            }
        } else if (data.containsKey("_class"))
        {
            String className = (String) data.get("_class");
            String packageName = (String) data.get("_package");
            log.debug(String.format(
                    "Get Request message for package '%s' class '%s'",
                    packageName, className));
            for (ManagedObject mo : objects.values())
            {
                if (mo.getManagedClassName().equals(className))
                {
                    if ((packageName == null) || packageName.equals("")
                            || packageName.equals(mo.getManagedPackageName()))
                    {
                        content('g', seq, replyTo, mo);
                    }
                }
            }
        } else
        {
            for (ManagedObject mo : objects.values())
            {
                content('g', seq, replyTo, mo);
            }
        }
        complete(seq, replyTo);
    }

    protected void handleSchemaRequest(long seq, Destination replyTo,
            Decoder dec)
    {
        String pkg = dec.readStr8();
        String cls = dec.readStr8();
        log.debug(String.format(
                "SchemaRequest message for package '%s' class '%s'", pkg, cls));
        ClassBinding cb = bctx.getClassBinding(pkg, cls);
        if (cb == null)
        {
            throw new AgentException("no such class: " + pkg + ", " + cls);
        }
        schemaResponse(seq, cb);
    }

    protected BBEncoder init(char opcode)
    {
        return init(opcode, 0);
    }

    protected BBEncoder init(char opcode, long sequence)
    {
        BBEncoder enc = new BBEncoder(1024);
        enc.init();
        enc.writeUint8((short) 'A');
        enc.writeUint8((short) 'M');
        enc.writeUint8((short) '2');
        enc.writeUint8((short) opcode);
        enc.writeUint32(sequence);
        return enc;
    }

    protected void send(BBEncoder enc)
    {
        send("broker", enc);
    }

    protected void send(Destination dest, BBEncoder enc)
    {
        try
        {
            byte[] buf = new byte[1024];
            byte[] body = new byte[1024];
            BytesMessage msg = session.createBytesMessage();
            ByteBuffer slice = enc.segment();
            while (slice.hasRemaining())
            {
                int n = Math.min(buf.length, slice.remaining());
                slice.get(buf, 0, n);
                msg.writeBytes(buf, 0, n);
            }
            msg.setJMSReplyTo(reply);
            // ???: I assume this is thread safe.
            prod.send(dest, msg);
        } catch (JMSException e)
        {
            throw new AgentException(e);
        }
    }

    protected void send(String routingKey, BBEncoder enc)
    {
        try
        {
            send(session
                    .createQueue("management://qpid.management//?routingkey='"
                            + routingKey + "'"), enc);
        } catch (JMSException e)
        {
            throw new AgentException(e);
        }
    }

    protected void attachRequest(String label, UUID systemId)
    {
        BBEncoder enc = init('A');
        enc.writeStr8(label);
        enc.writeUuid(systemId);
        enc.writeUint32(0);
        enc.writeUint32(0);
        send(enc);
    }

    protected void packageIndication(String pkg)
    {
        BBEncoder enc = init('p');
        enc.writeStr8(pkg);
        send(enc);
    }

    protected void classIndication(ClassBinding cb)
    {
        BBEncoder enc = init('q');
        enc.writeUint8(cb.getKind());
        enc.writeStr8(cb.getPackage());
        enc.writeStr8(cb.getName());
        enc.writeBin128(cb.getSchemaHash()); // schema hash?
        send(enc);
    }

    protected void schemaResponse(long seq, ClassBinding cb)
    {
        BBEncoder enc = init('s', seq);
        cb.encode(enc);
        send(enc);
    }

    protected void content(char c, long seq, Destination dest, ManagedObject mo)
    {
        BBEncoder enc = init(c, seq);
        ClassBinding cb = getClassBinding(mo);
        String pkg = cb.getPackage();
        String cls = cb.getName();
        enc.writeStr8(pkg);
        enc.writeStr8(cls);
        enc.writeBin128(cb.getSchemaHash());
        long now = System.currentTimeMillis() * 1000000;
        enc.writeUint64(now);
        enc.writeUint64(now);
        enc.writeUint64(0);
        enc.writeUint64(0x0000FFFFFFFFFFFFL & ((bbank << 28) | abank));
        enc.writeUint64(mo.getId());
        for (PropertyBinding p : cb.getProperties())
        {
            p.getType().encode(enc, mo.get(p));
        }
        if (dest == null)
        {
            send(String.format("console.obj.%d.%d.%s.%s", bbank, abank, pkg,
                    cls), enc);
        } else
        {
            send(dest, enc);
        }
    }

    protected void complete(long seq, Destination dest)
    {
        BBEncoder enc = init('z', seq);
        enc.writeUint32(0);
        enc.writeStr8("");
        send(dest, enc);
    }

    protected void methodResponse(long seq, Destination dest, int status,
            String text)
    {
        methodResponse(seq, dest, status, text, null, null);
    }

    protected void methodResponse(long seq, Destination dest, int status,
            String text, MethodBinding method, Object[] result)
    {
        BBEncoder enc = init('m', seq);
        enc.writeUint32(status);
        enc.writeStr16(text == null ? "" : text);
        if (method != null)
        {
            int idx = 0;
            for (ParameterBinding p : method.getOutParameters())
            {
                p.getType().encode(enc, result[idx++]);
            }
        }
        send(dest, enc);
    }

    protected Class getClass(String className)
    {
        try
        {
            if (classLoader != null)
            {
                return classLoader.loadClass(className);
            } else
            {
                return Class.forName(className);
            }
        } catch (ClassNotFoundException e)
        {
            throw new AgentException(String.format(
                    "No class named %s was found", className), e);
        }
    }

    public String getLabel()
    {
        return label;
    }

    public void setLabel(String label)
    {
        this.label = label;
    }

    public AMQConnection getConnection()
    {
        return connection;
    }

    public void setConnection(AMQConnection connection)
    {
        this.connection = connection;
    }

    public boolean isSessionTransacted()
    {
        return sessionTransacted;
    }

    public void setSessionTransacted(boolean sessionTransacted)
    {
        this.sessionTransacted = sessionTransacted;
    }

    public void setManagedObjects(List objectList)
    {
        this.managedObjects = objectList;
    }

    public List getManagedObjects()
    {
        return managedObjects;
    }

    public void setRegisteredClasses(List objectList)
    {
        this.registeredClasses = objectList;
    }

    public List getRegisteredClasses()
    {
        return this.registeredClasses;
    }

    public static void main(String[] args) throws Exception
    {
        String broker = args[0];
        String name = args[1];
        String url = String.format(
                "amqp://guest:guest@/?brokerlist='tcp://%s'", broker);
        AMQConnection conn = new AMQConnection(url);
        Agent agent = new Agent(name, UUID.randomUUID());
        agent.setConnection(conn);
        for (int i = 2; i < args.length; i++)
        {
            Class<?> cls = Class.forName(args[i]);
            agent.register(new ManagedPOJO(cls.newInstance()));
        }
        agent.start();
        while (true)
        {
            Thread.sleep(1000);
        }
    }
}
