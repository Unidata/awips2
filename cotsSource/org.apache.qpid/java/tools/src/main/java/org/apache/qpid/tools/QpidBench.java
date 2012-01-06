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
package org.apache.qpid.tools;

import static org.apache.qpid.tools.QpidBench.Mode.BOTH;
import static org.apache.qpid.tools.QpidBench.Mode.CONSUME;
import static org.apache.qpid.tools.QpidBench.Mode.PUBLISH;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import javax.jms.DeliveryMode;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.TextMessage;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.thread.Threading;
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.transport.ExchangeBind;
import org.apache.qpid.transport.Header;
import org.apache.qpid.transport.MessageAcceptMode;
import org.apache.qpid.transport.MessageAcquireMode;
import org.apache.qpid.transport.MessageCreditUnit;
import org.apache.qpid.transport.MessageDeliveryMode;
import org.apache.qpid.transport.MessageFlowMode;
import org.apache.qpid.transport.MessageProperties;
import org.apache.qpid.transport.MessageSubscribe;
import org.apache.qpid.transport.MessageTransfer;
import org.apache.qpid.transport.QueueDeclare;
import org.apache.qpid.transport.SessionException;
import org.apache.qpid.transport.SessionListener;
import org.apache.qpid.util.UUIDGen;
import org.apache.qpid.util.UUIDs;

/**
 * QpidBench
 *
 */

public class QpidBench
{

    static enum Mode
    {
        PUBLISH, CONSUME, BOTH
    }

    private static class Options
    {
        private StringBuilder usage = new StringBuilder("qpid-bench <options>");

        void usage(String name, String description, Object def)
        {
            String defval = "";
            if (def != null)
            {
                defval = String.format(" (%s)", def);
            }
            usage.append(String.format("\n  %-15s%-14s %s", name, defval, description));
        }

        public String broker = "localhost";
        public int port = 5672;
        public long count = 1000000;
        public long window = 100000;
        public long sample = window;
        public int size = 1024;
        public Mode mode = BOTH;
        public boolean timestamp = false;
        public boolean message_id = false;
        public boolean message_cache = false;
        public boolean persistent = false;
        public boolean jms_publish = false;
        public boolean jms_consume = false;
        public boolean help = false;

        {
            usage("-b, --broker", "the broker hostname", broker);
        }

        public void parse__broker(String b)
        {
            this.broker = b;
        }

        public void parse_b(String b)
        {
            parse__broker(b);
        }

        {
            usage("-p, --port", "the broker port", port);
        }

        public void parse__port(String p)
        {
            this.port = Integer.parseInt(p);
        }

        public void parse_p(String p)
        {
            parse__port(p);
        }

        {
            usage("-c, --count", "the number of messages to send/receive, 0 means no limit", count);
        }

        public void parse__count(String c)
        {
            this.count = Long.parseLong(c);
        }

        public void parse_c(String c)
        {
            parse__count(c);
        }

        {
            usage("-w, --window", "the number of messages to send before blocking, 0 disables", window);
        }

        public void parse__window(String w)
        {
            this.window = Long.parseLong(w);
        }

        public void parse_w(String w)
        {
            parse__window(w);
        }

        {
            usage("--sample", "print stats after this many messages, 0 disables", sample);
        }

        public void parse__sample(String s)
        {
            this.sample = Long.parseLong(s);
        }

        {
            usage("-i, --interval", "sets both --window and --sample", window);
        }

        public void parse__interval(String i)
        {
            this.window = Long.parseLong(i);
            this.sample = window;
        }

        public void parse_i(String i)
        {
            parse__interval(i);
        }

        {
            usage("-s, --size", "the message size", size);
        }

        public void parse__size(String s)
        {
            this.size = Integer.parseInt(s);
        }

        public void parse_s(String s)
        {
            parse__size(s);
        }

        {
            usage("-m, --mode", "one of publish, consume, or both", mode);
        }

        public void parse__mode(String m)
        {
            if (m.equalsIgnoreCase("publish"))
            {
                this.mode = PUBLISH;
            }
            else if (m.equalsIgnoreCase("consume"))
            {
                this.mode = CONSUME;
            }
            else if (m.equalsIgnoreCase("both"))
            {
                this.mode = BOTH;
            }
            else
            {
                throw new IllegalArgumentException
                    ("must be one of 'publish', 'consume', or 'both'");
            }
        }

        public void parse_m(String m)
        {
            parse__mode(m);
        }

        {
            usage("--timestamp", "set timestamps on each message if true", timestamp);
        }

        public void parse__timestamp(String t)
        {
            this.timestamp = Boolean.parseBoolean(t);
        }

        {
            usage("--mesage-id", "set the message-id on each message if true", message_id);
        }

        public void parse__message_id(String m)
        {
            this.message_id = Boolean.parseBoolean(m);
        }

        {
            usage("--message-cache", "reuse the same message for each send if true", message_cache);
        }

        public void parse__message_cache(String c)
        {
            this.message_cache = Boolean.parseBoolean(c);
        }

        {
            usage("--persistent", "set the delivery-mode to persistent if true", persistent);
        }

        public void parse__persistent(String p)
        {
            this.persistent = Boolean.parseBoolean(p);
        }

        {
            usage("--jms-publish", "use the jms client for publish", jms_publish);
        }

        public void parse__jms_publish(String jp)
        {
            this.jms_publish = Boolean.parseBoolean(jp);
        }

        {
            usage("--jms-consume", "use the jms client for consume", jms_consume);
        }

        public void parse__jms_consume(String jc)
        {
            this.jms_consume = Boolean.parseBoolean(jc);
        }

        {
            usage("--jms", "sets both --jms-publish and --jms-consume", false);
        }

        public void parse__jms(String j)
        {
            this.jms_publish = this.jms_consume = Boolean.parseBoolean(j);
        }

        {
            usage("-h, --help", "prints this message", null);
        }

        public void parse__help()
        {
            this.help = true;
        }

        public void parse_h()
        {
            parse__help();
        }

        public String parse(String ... args)
        {
            Class klass = getClass();
            List<String> arguments = new ArrayList<String>();
            for (int i = 0; i < args.length; i++)
            {
                String option = args[i];

                if (!option.startsWith("-"))
                {
                    arguments.add(option);
                    continue;
                }

                String method = "parse" + option.replace('-', '_');
                try
                {
                    try
                    {
                        Method parser = klass.getMethod(method);
                        parser.invoke(this);
                    }
                    catch (NoSuchMethodException e)
                    {
                        try
                        {
                            Method parser = klass.getMethod(method, String.class);

                            String value = null;
                            if (i + 1 < args.length)
                            {
                                value = args[i+1];
                                i++;
                            }
                            else
                            {
                                return option + " requires a value";
                            }

                            parser.invoke(this, value);
                        }
                        catch (NoSuchMethodException e2)
                        {
                            return "no such option: " + option;
                        }
                    }
                }
                catch (InvocationTargetException e)
                {
                    Throwable t = e.getCause();
                    return String.format
                        ("error parsing %s: %s: %s", option, t.getClass().getName(),
                         t.getMessage());
                }
                catch (IllegalAccessException e)
                {
                    throw new RuntimeException
                        ("unable to access parse method: " + option, e);
                }
            }

            return parseArguments(arguments);
        }

        public String parseArguments(List<String> arguments)
        {
            if (arguments.size() > 0)
            {
                String args = arguments.toString();
                return "unrecognized arguments: " + args.substring(1, args.length() - 1);
            }
            else
            {
                return null;
            }
        }

        public String toString()
        {
            Class klass = getClass();
            Field[] fields = klass.getFields();
            StringBuilder str = new StringBuilder();
            for (int i = 0; i < fields.length; i++)
            {
                if (i > 0)
                {
                    str.append("\n");
                }

                String name = fields[i].getName();
                str.append(name);
                str.append(" = ");
                Object value;
                try
                {
                    value = fields[i].get(this);
                }
                catch (IllegalAccessException e)
                {
                    throw new RuntimeException
                        ("unable to access field: " + name, e);
                }
                str.append(value);
            }

            return str.toString();
        }
    }

    public static final void main(String[] args) throws Exception
    {
        final Options opts = new Options();
        String error = opts.parse(args);
        if (error != null)
        {
            System.err.println(error);
            System.exit(-1);
            return;
        }

        if (opts.help)
        {
            System.out.println(opts.usage);
            return;
        }

        System.out.println(opts);

        switch (opts.mode)
        {
        case CONSUME:
        case BOTH:
            Runnable r = new Runnable()
            {
                public void run()
                {
                    try
                    {
                        if (opts.jms_consume)
                        {
                            jms_consumer(opts);
                        }
                        else
                        {
                            native_consumer(opts);
                        }
                    }
                    catch (Exception e)
                    {
                        throw new RuntimeException(e);
                    }
                    System.out.println("Consumer Completed");
                }
            };
           
            Thread t;
            try
            {
                t = Threading.getThreadFactory().createThread(r);                      
            }
            catch(Exception e)
            {
                throw new Error("Error creating consumer thread",e);
            }
            t.start();
            break;
        }

        switch (opts.mode)
        {
        case PUBLISH:
        case BOTH:
            Runnable r = new Runnable()
            {
                public void run()
                {
                    try
                    {
                        if (opts.jms_publish)
                        {
                            jms_publisher(opts);
                        }
                        else
                        {
                            native_publisher(opts);
                        }
                    }
                    catch (Exception e)
                    {
                        throw new RuntimeException(e);
                    }
                    System.out.println("Producer Completed");
                }
            };
            Thread t;
            try
            {
                t = Threading.getThreadFactory().createThread(r);                      
            }
            catch(Exception e)
            {
                throw new Error("Error creating publisher thread",e);
            }
            t.start();
            break;
        }
    }

    private static enum Column
    {
        LEFT, RIGHT
    }

    private static final void sample(Options opts, Column col, String name, long count,
                                     long start, long time, long lastTime)
    {
        String pfx = "";
        String sfx = "";
        if (opts.mode == BOTH)
        {
            if (col == Column.RIGHT)
            {
                pfx = "               --                   ";
            }
            else
            {
                sfx = "               --";
            }
        }

        if (count == 0)
        {
            String stats = String.format("%s: %tc", name, start);
            System.out.println(String.format("%s%-36s%s", pfx, stats, sfx));
            return;
        }

        double cumulative = 1000 * (double) count / (double) (time - start);
        double interval = 1000 * ((double) opts.sample / (double) (time - lastTime));

        String stats = String.format
            ("%s: %d %.2f %.2f", name, count, cumulative, interval);
        System.out.println(String.format("%s%-36s%s", pfx, stats, sfx));
    }

    private static final javax.jms.Connection getJMSConnection(Options opts) throws Exception
    {
        String url = String.format
            ("amqp://guest:guest@clientid/test?brokerlist='tcp://%s:%d'",
             opts.broker, opts.port);
        return new AMQConnection(url);
    }

    private static final void jms_publisher(Options opts) throws Exception
    {
        javax.jms.Connection conn = getJMSConnection(opts);

        javax.jms.Session ssn = conn.createSession(false, javax.jms.Session.AUTO_ACKNOWLEDGE);
        Destination dest = ssn.createQueue("test-queue");
        Destination echo_dest = ssn.createQueue("echo-queue");
        MessageProducer prod = ssn.createProducer(dest);
        MessageConsumer cons = ssn.createConsumer(echo_dest);
        prod.setDisableMessageID(!opts.message_id);
        prod.setDisableMessageTimestamp(!opts.timestamp);
        prod.setDeliveryMode(opts.persistent ? DeliveryMode.PERSISTENT : DeliveryMode.NON_PERSISTENT);

        StringBuilder str = new StringBuilder();
        for (int i = 0; i < opts.size; i++)
        {
            str.append((char) (i % 128));
        }

        String body = str.toString();

        TextMessage cached = ssn.createTextMessage();
        cached.setText(body);

        conn.start();

        long count = 0;
        long lastTime = 0;
        long start = System.currentTimeMillis();
        while (opts.count == 0 || count < opts.count)
        {
            if (opts.window > 0 && (count % opts.window) == 0 && count > 0)
            {
                Message echo = cons.receive();
            }

            if (opts.sample > 0 && (count % opts.sample) == 0)
            {
                long time = System.currentTimeMillis();
                sample(opts, Column.LEFT, "JP", count, start, time, lastTime);
                lastTime = time;
            }

            TextMessage m;
            if (opts.message_cache)
            {
                m = cached;
            }
            else
            {
                m = ssn.createTextMessage();
                m.setText(body);
            }

            prod.send(m);
            count++;
        }

        conn.close();
    }

    private static final void jms_consumer(final Options opts) throws Exception
    {
        final javax.jms.Connection conn = getJMSConnection(opts);
        javax.jms.Session ssn = conn.createSession(false, javax.jms.Session.AUTO_ACKNOWLEDGE);
        Destination dest = ssn.createQueue("test-queue");
        Destination echo_dest = ssn.createQueue("echo-queue");
        MessageConsumer cons = ssn.createConsumer(dest);
        final MessageProducer prod = ssn.createProducer(echo_dest);
        prod.setDisableMessageID(true);
        prod.setDisableMessageTimestamp(true);
        prod.setDeliveryMode(DeliveryMode.NON_PERSISTENT);
        final TextMessage echo = ssn.createTextMessage();
        echo.setText("ECHO");

        final Object done = new Object();
        cons.setMessageListener(new MessageListener()
        {
            private long count = 0;
            private long lastTime = 0;
            private long start;

            public void onMessage(Message m)
            {
                if (count == 0)
                {
                    start = System.currentTimeMillis();
                }

                try
                {
                    boolean sample = opts.sample > 0 && (count % opts.sample) == 0;
                    long time = sample ? System.currentTimeMillis() : 0;

                    if (opts.window > 0 && (count % opts.window) == 0)
                    {
                        prod.send(echo);
                    }

                    if (sample)
                    {
                        sample(opts, Column.RIGHT, "JC", count, start, time, lastTime);
                        lastTime = time;
                    }
                }
                catch (JMSException e)
                {
                    throw new RuntimeException(e);
                }
                count++;

                if (opts.count > 0 && count >= opts.count)
                {
                    synchronized (done)
                    {
                        done.notify();
                    }
                }
            }
        });

        conn.start();
        synchronized (done)
        {
            done.wait();
        }
        conn.close();
    }

    private static final org.apache.qpid.transport.Connection getConnection
        (Options opts)
    {
        org.apache.qpid.transport.Connection conn =
            new org.apache.qpid.transport.Connection();
        conn.connect(opts.broker, opts.port, null, "guest", "guest",false);
        return conn;
    }

    private static abstract class NativeListener implements SessionListener
    {

        public void opened(org.apache.qpid.transport.Session ssn) {}

        public void resumed(org.apache.qpid.transport.Session ssn) {}

        public void exception(org.apache.qpid.transport.Session ssn,
                              SessionException exc)
        {
            exc.printStackTrace();
        }

        public void closed(org.apache.qpid.transport.Session ssn) {}

    }

    private static final void native_publisher(Options opts) throws Exception
    {
        final long[] echos = { 0 };
        org.apache.qpid.transport.Connection conn = getConnection(opts);
        org.apache.qpid.transport.Session ssn = conn.createSession();
        ssn.setSessionListener(new NativeListener()
        {
            public void message(org.apache.qpid.transport.Session ssn,
                                MessageTransfer xfr)
            {
                synchronized (echos)
                {
                    echos[0]++;
                    echos.notify();
                }
                ssn.processed(xfr);
            }
        });

        ssn.invoke(new QueueDeclare().queue("test-queue").durable(false));
        ssn.invoke(new QueueDeclare().queue("echo-queue").durable(false));
        ssn.invoke(new ExchangeBind().exchange("amq.direct").queue("test-queue").bindingKey("test-queue"));
        ssn.invoke(new ExchangeBind().exchange("amq.direct").queue("echo-queue").bindingKey("echo-queue"));

        MessageProperties cached_mp = new MessageProperties();
        DeliveryProperties cached_dp = new DeliveryProperties();
        cached_dp.setRoutingKey("test-queue");
        cached_dp.setDeliveryMode
            (opts.persistent ? MessageDeliveryMode.PERSISTENT : MessageDeliveryMode.NON_PERSISTENT);

        int size = opts.size;
        ByteBuffer body = ByteBuffer.allocate(size);
        for (int i = 0; i < size; i++)
        {
            body.put((byte) i);
        }
        body.flip();

        ssn.invoke(new MessageSubscribe()
                   .queue("echo-queue")
                   .destination("echo-queue")
                   .acceptMode(MessageAcceptMode.NONE)
                   .acquireMode(MessageAcquireMode.PRE_ACQUIRED));
        ssn.messageSetFlowMode("echo-queue", MessageFlowMode.WINDOW);
        ssn.messageFlow("echo-queue", MessageCreditUnit.MESSAGE, 0xFFFFFFFF);
        ssn.messageFlow("echo-queue", MessageCreditUnit.BYTE, 0xFFFFFFFF);

        UUIDGen gen = UUIDs.newGenerator();

        long count = 0;
        long lastTime = 0;
        long start = System.currentTimeMillis();
        while (opts.count == 0 || count < opts.count)
        {
            if (opts.window > 0 && (count % opts.window) == 0 && count > 0)
            {
                synchronized (echos)
                {
                    while (echos[0] < (count/opts.window))
                    {
                        echos.wait();
                    }
                }
            }

            if (opts.sample > 0 && (count % opts.sample) == 0)
            {
                long time = System.currentTimeMillis();
                sample(opts, Column.LEFT, "NP", count, start, time, lastTime);
                lastTime = time;
            }

            MessageProperties mp;
            DeliveryProperties dp;
            if (opts.message_cache)
            {
                mp = cached_mp;
                dp = cached_dp;
            }
            else
            {
                mp = new MessageProperties();
                dp = new DeliveryProperties();
                dp.setRoutingKey("test-queue");
                dp.setDeliveryMode
                    (opts.persistent ? MessageDeliveryMode.PERSISTENT : MessageDeliveryMode.NON_PERSISTENT);

            }

            if (opts.message_id)
            {
                mp.setMessageId(gen.generate());
            }

            if (opts.timestamp)
            {
                dp.setTimestamp(System.currentTimeMillis());
            }

            ssn.messageTransfer("amq.direct", MessageAcceptMode.NONE, MessageAcquireMode.PRE_ACQUIRED,
                                new Header(dp, mp), body.slice());
            count++;
        }

        ssn.messageCancel("echo-queue");

        ssn.sync();
        ssn.close();
        conn.close();
    }

    private static final void native_consumer(final Options opts) throws Exception
    {
        final DeliveryProperties dp = new DeliveryProperties();
        final byte[] echo = new byte[0];
        dp.setRoutingKey("echo-queue");
        dp.setDeliveryMode(MessageDeliveryMode.NON_PERSISTENT);
        final MessageProperties mp = new MessageProperties();
        final Object done = new Object();
        org.apache.qpid.transport.Connection conn = getConnection(opts);
        org.apache.qpid.transport.Session ssn = conn.createSession();
        ssn.setSessionListener(new NativeListener()
        {
            private long count = 0;
            private long lastTime = 0;
            private long start;

            public void message(org.apache.qpid.transport.Session ssn,
                                MessageTransfer xfr)
            {
                if (count == 0)
                {
                    start = System.currentTimeMillis();
                }

                boolean sample = opts.sample > 0 && (count % opts.sample) == 0;
                long time = sample ? System.currentTimeMillis() : 0;

                if (opts.window > 0 && (count % opts.window) == 0)
                {
                    ssn.messageTransfer("amq.direct",
                                        MessageAcceptMode.NONE,
                                        MessageAcquireMode.PRE_ACQUIRED,
                                        new Header(dp, mp),
                                        echo);
                }

                if (sample)
                {
                    sample(opts, Column.RIGHT, "NC", count, start, time, lastTime);
                    lastTime = time;
                }
                ssn.processed(xfr);
                count++;

                if (opts.count > 0 && count >= opts.count)
                {
                    synchronized (done)
                    {
                        done.notify();
                    }
                }
            }
        });

        ssn.invoke(new QueueDeclare().queue("test-queue").durable(false));
        ssn.invoke(new QueueDeclare().queue("echo-queue").durable(false));
        ssn.invoke(new ExchangeBind().exchange("amq.direct").queue("test-queue").bindingKey("test-queue"));
        ssn.invoke(new ExchangeBind().exchange("amq.direct").queue("echo-queue").bindingKey("echo-queue"));

        ssn.invoke(new MessageSubscribe()
                   .queue("test-queue")
                   .destination("test-queue")
                   .acceptMode(MessageAcceptMode.NONE)
                   .acquireMode(MessageAcquireMode.PRE_ACQUIRED));
        ssn.messageSetFlowMode("test-queue", MessageFlowMode.WINDOW);
        ssn.messageFlow("test-queue", MessageCreditUnit.MESSAGE, 0xFFFFFFFF);
        ssn.messageFlow("test-queue", MessageCreditUnit.BYTE, 0xFFFFFFFF);

        synchronized (done)
        {
            done.wait();
        }

        ssn.messageCancel("test-queue");

        ssn.sync();
        ssn.close();
        conn.close();
    }

}
