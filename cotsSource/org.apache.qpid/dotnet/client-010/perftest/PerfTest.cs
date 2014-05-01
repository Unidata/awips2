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

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using common.org.apache.qpid.transport.util;
using org.apache.qpid.client;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;
using Plossum.CommandLine;

namespace PerfTest
{
    [CommandLineManager(ApplicationName = "Qpid Perf Tests", Copyright = "Apache Software Foundation")]
    public class Options
    {
        [CommandLineOption(Description = "Displays this help text")] 
        public bool Help;

        [CommandLineOption(Description = "Create shared queues.", MinOccurs = 0)] 
        public Boolean Setup;

        [CommandLineOption(Description = "Run test, print report.", MinOccurs = 0)] 
        public Boolean Control;

        [CommandLineOption(Description = "Publish messages.", MinOccurs = 0)] 
        public Boolean Publish;

        [CommandLineOption(Description = "Subscribe for messages.", MinOccurs = 0)] 
        public Boolean Subscribe;

        [CommandLineOption(Description = "Test mode: [shared|fanout|topic]", MinOccurs = 0)]
        public string Mode
        {
            get { return _mMode; }
            set
            {
                if (! value.Equals("shared") && ! value.Equals("fanout") && ! value.Equals("topic"))
                    throw new InvalidOptionValueException(
                        "The mode must not be shared|fanout|topic", false);
                _mMode = value;
            }
        }

        private string _mMode = "shared";

        [CommandLineOption(Description = "Specifies the broler name", MinOccurs = 0)]
        public string Broker
        {
            get { return _broker; }
            set
            {
                if (String.IsNullOrEmpty(value))
                    throw new InvalidOptionValueException(
                        "The broker name must not be empty", false);
                _broker = value;
            }
        }

        private string _broker = "localhost";

        [CommandLineOption(Description = "Specifies the port name", MinOccurs = 0)]
        public int Port
        {
            get { return _port; }
            set { _port = value; }
        }

        private int _port = 5672;

        #region Publisher

        [CommandLineOption(Description = "Create N publishers.", MinOccurs = 0)]
        public int Pubs
        {
            get { return _pubs; }
            set { _pubs = value; }
        }

        private int _pubs = 1;

        [CommandLineOption(Description = "Each publisher sends N messages.", MinOccurs = 0)]
        public double Count
        {
            get { return _count; }
            set { _count = value; }
        }

        private double _count = 5000;

        [CommandLineOption(Description = "Size of messages in bytes.", MinOccurs = 0)]
        public long Size
        {
            get { return _size; }
            set { _size = value; }
        }

        private long _size = 1024;

        [CommandLineOption(Description = "Publisher use confirm-mode.", MinOccurs = 0)] 
        public Boolean Confirm = true;

        [CommandLineOption(Description = "Publish messages as durable.", MinOccurs = 0)] 
        public Boolean Durable;

        [CommandLineOption(Description = "Make data for each message unique.", MinOccurs = 0)] 
        public Boolean UniqueData;

        [CommandLineOption(Description = "Wait for confirmation of each message before sending the next one.",
            MinOccurs = 0)]
        public Boolean SyncPub;

        [CommandLineOption(Description = ">=0 delay between msg publish.", MinOccurs = 0)]
        public double IntervalPub
        {
            get { return _interval_pub; }
            set { _interval_pub = value; }
        }

        private double _interval_pub;

        #endregion

        #region Subscriber

        [CommandLineOption(Description = "Create N subscribers.", MinOccurs = 0)]
        public int Subs
        {
            get { return _subs; }
            set { _subs = value; }
        }

        private int _subs = 1;

        [CommandLineOption(Description = "N>0: Subscriber acks batches of N.\n N==0: Subscriber uses unconfirmed mode",
            MinOccurs = 0)]
        public int SubAck
        {
            get { return _suback; }
            set { _suback = value; }
        }

        private int _suback;

        [CommandLineOption(Description = ">=0 delay between msg consume", MinOccurs = 0)]
        public double IntervalSub
        {
            get { return _interval_sub; }
            set { _interval_sub = value; }
        }

        private double _interval_sub;

        #endregion

        [CommandLineOption(Description = "Create N queues.", MinOccurs = 0)]
        public int Queues
        {
            get { return _qt; }
            set { _qt = value; }
        }

        private int _qt = 1;

        [CommandLineOption(Description = "Desired number of iterations of the test.", MinOccurs = 0)]
        public int Iterations
        {
            get { return _iterations; }
            set { _iterations = value; }
        }

        private int _iterations = 1;

        [CommandLineOption(Description = "If non-zero, the transaction batch size.", MinOccurs = 0)]
        public int Tx
        {
            get { return _tx; }
            set { _tx = value; }
        }

        private int _tx;

        [CommandLineOption(Description = "Make queue durable (implied if durable set.", MinOccurs = 0)] 
        public Boolean QueueDurable;

        [CommandLineOption(Description = "Queue policy: count to trigger 'flow to disk'", MinOccurs = 0)]
        public double QueueMaxCount
        {
            get { return _queueMaxCount; }
            set { _queueMaxCount = value; }
        }

        private double _queueMaxCount;

        [CommandLineOption(Description = "Queue policy: accumulated size to trigger 'flow to disk'", MinOccurs = 0)]
        public double QueueMaxSize
        {
            get { return _queueMaxSize; }
            set { _queueMaxSize = value; }
        }

        private double _queueMaxSize;

        public double SubQuota
        {
            get { return _subQuota; }
            set { _subQuota = value; }
        }

        private double _subQuota;
    }

    internal interface Startable
    {
        void Start();
    }

    public abstract class PerfTestClient : Startable
    {
        private readonly IClient _connection;
        private readonly IClientSession _session;
        private readonly Options _options;

        public IClientSession Session
        {
            get { return _session; }
        }

        public Options Options
        {
            get { return _options; }
        }

        protected PerfTestClient(Options options)
        {
            _options = options;
            _connection = new Client();
            _connection.Connect(options.Broker, options.Port, "test", "guest", "guest");
            _session = _connection.CreateSession(50000);
        }

        public abstract void Start();
    }

    public class SetupTest : PerfTestClient
    {
        public SetupTest(Options options)
            : base(options)
        {
        }

        private void queueInit(String name, Boolean durable, Dictionary<String, Object> arguments)
        {
            Session.QueueDeclare(name, null, arguments, durable ? Option.DURABLE : Option.NONE);
            Session.QueuePurge(name);
            Session.ExchangeBind(name, "amq.direct", name);
            Session.Sync();
        }

        public override void Start()
        {
            queueInit("pub_start", false, null);
            queueInit("pub_done", false, null);
            queueInit("sub_ready", false, null);
            queueInit("sub_done", false, null);
            if (Options.Mode.Equals("shared"))
            {
                Dictionary<String, Object> settings = new Dictionary<string, object>();
                if (Options.QueueMaxCount > 0)
                    settings.Add("qpid.max_count", Options.QueueMaxCount);
                if (Options.QueueMaxSize > 0)
                    settings.Add("qpid.max_size", Options.QueueMaxSize);
                for (int i = 0; i < Options.Queues; ++i)
                {
                    string qname = "perftest" + i;
                    queueInit(qname, Options.Durable || Options.QueueDurable, settings);
                }
            }
        }
    }

    public class SubscribeThread : PerfTestClient
    {
        private readonly string _queue;

        public SubscribeThread(Options options, string key, string exchange)
            : base(options)
        {
            _queue = "perftest" + (new UUID(10, 10));
            Session.QueueDeclare(_queue, null, null, Option.EXCLUSIVE, Option.AUTO_DELETE,
                                 Options.Durable ? Option.DURABLE : Option.NONE);
            Session.ExchangeBind(_queue, exchange, key);
        }

        public SubscribeThread(Options options, string key)
            : base(options)
        {
            _queue = key;
        }

        public override void Start()
        {
            if (Options.Tx > 0)
            {
                Session.TxSelect();
                Session.Sync();
            }
            CircularBuffer<IMessage> buffer = new CircularBuffer<IMessage>(100);
            // Create a listener and subscribe it to the queue named "message_queue"
            IMessageListener listener = new SyncListener(buffer);

            string dest = "dest" + UUID.RandomUuid();
            Session.AttachMessageListener(listener, dest);
            Session.MessageSubscribe(_queue, dest,
                                     Options.Tx > 0 || Options.SubAck > 0
                                         ? MessageAcceptMode.EXPLICIT
                                         : MessageAcceptMode.NONE,
                                     MessageAcquireMode.PRE_ACQUIRED, null, 0, null);
            // issue credits     
            Session.MessageSetFlowMode(dest, MessageFlowMode.WINDOW);
            Session.MessageFlow(dest, MessageCreditUnit.BYTE, ClientSession.MESSAGE_FLOW_MAX_BYTES);

            // Notify controller we are ready.
            IMessage message = new Message();
            message.DeliveryProperties.SetRoutingKey("sub_ready");

            message.AppendData(Encoding.UTF8.GetBytes("ready"));
            Session.MessageTransfer("amq.direct", message);

            if (Options.Tx > 0)
            {
                Session.TxCommit();
                Session.Sync();
            }


            for (int j = 0; j < Options.Iterations; ++j)
            {
               
                //need to allocate some more credit
                Session.MessageFlow(dest, MessageCreditUnit.MESSAGE, (long)Options.SubQuota);
                
                RangeSet range = new RangeSet();
                IMessage msg;
                DateTime start = DateTime.Now;
                for (long i = 0; i < Options.SubQuota; ++i)
                {                   
                    msg = buffer.Dequeue();
                    if (Options.Tx > 0 && ((i + 1)%Options.Tx == 0))
                    {
                        Session.TxCommit();
                        Session.Sync();
                    }
                    if (Options.IntervalSub > 0)
                    {
                        Thread.Sleep((int) Options.IntervalSub*1000);
                    }
                    range.Add(msg.Id);
                }
                if (Options.Tx > 0 || Options.SubAck > 0)
                    Session.MessageAccept(range);
                range.Clear();
                if (Options.Tx > 0)
                {
                    Session.TxSelect();
                    Session.Sync();
                }
                DateTime end = DateTime.Now;

                // Report to publisher.
                message.DeliveryProperties.SetRoutingKey("sub_done");
                message.ClearData();
                message.AppendData(BitConverter.GetBytes(Options.SubQuota / end.Subtract(start).TotalMilliseconds ));
                Session.MessageTransfer("amq.direct", message);
                if (Options.Tx > 0)
                {
                    Session.TxSelect();
                    Session.Sync();
                }
            }
            Session.Close();
        }
    }

    public class SyncListener : IMessageListener
    {
        private readonly CircularBuffer<IMessage> _buffer;

        public SyncListener(CircularBuffer<IMessage> buffer)
        {
            _buffer = buffer;
        }

        public void MessageTransfer(IMessage m)
        {
            _buffer.Enqueue(m);
        }
    }


    public class PublishThread : PerfTestClient
    {
        private readonly string _exchange;
        private readonly string _key;

        public PublishThread(Options options, string key, string exchange)
            : base(options)
        {
            _key = key;
            _exchange = exchange;
        }


        public override void Start()
        {
            byte[] data = new byte[Options.Size];
            // randomly populate data 
            Random r = new Random(34);
            r.NextBytes(data);
            IMessage message = new Message();
            message.AppendData(data);

            message.DeliveryProperties.SetRoutingKey(_key);

            if (Options.Durable)
                message.DeliveryProperties.SetDeliveryMode(MessageDeliveryMode.PERSISTENT);

            if (Options.Tx > 0)
            {
                Session.TxSelect();
                Session.Sync();
            }

            CircularBuffer<IMessage> buffer = new CircularBuffer<IMessage>(100);
            // Create a listener and subscribe it to the queue named "pub_start"          
            IMessageListener listener = new SyncListener(buffer);
            string localQueue = "localQueue-" + UUID.RandomUuid().ToString();
            Session.QueueDeclare(localQueue, null, null, Option.AUTO_DELETE);
            Session.ExchangeBind(localQueue, "amq.direct", "pub_start");
            Session.AttachMessageListener(listener, localQueue);
            Session.MessageSubscribe(localQueue);
            if (Options.Tx > 0)
            {
                Session.TxCommit();
                Session.Sync();
            }
            buffer.Dequeue();

            for (int j = 0; j < Options.Iterations; ++j)
            {
                DateTime start = DateTime.Now;                
                for (long i = 0; i < Options.Count; ++i)
                {
                    Session.MessageTransfer(_exchange, message);

                    if (Options.SyncPub)
                    {
                        Session.Sync();
                    }
                    if (Options.Tx > 0 && (i + 1)%Options.Tx == 0)
                    {
                        Session.TxSelect();
                        Session.Sync();
                    }
                    if (Options.IntervalPub > 0)
                    {
                        Thread.Sleep((int) Options.IntervalSub*1000);
                    }
                }
                Session.Sync();
                DateTime end = DateTime.Now;

                // Report to publisher.
                message.DeliveryProperties.SetRoutingKey("pub_done");
                message.ClearData();
                double time = end.Subtract(start).TotalMilliseconds;
                byte[] rate = BitConverter.GetBytes( Options.Count / time );
                message.AppendData(rate);
                Session.MessageTransfer("amq.direct", message);
                if (Options.Tx > 0)
                {
                    Session.TxSelect();
                    Session.Sync();
                }
            }
            Session.Close();
        }
    }

    public class Controller : PerfTestClient
    {
        public Controller(Options options)
            : base(options)
        {
        }

        private void process(int size, string queue)
        {
            CircularBuffer<IMessage> buffer = new CircularBuffer<IMessage>(100);
            IMessageListener listener = new SyncListener(buffer);
            string localQueue = "queue-" + UUID.RandomUuid();
            Session.QueueDeclare(localQueue, null, null, Option.AUTO_DELETE);
            Session.ExchangeBind(localQueue, "amq.direct", queue);
            Session.AttachMessageListener(listener, localQueue);
            Session.MessageSubscribe(localQueue);
            for (int i = 0; i < size; ++i)
            {
                buffer.Dequeue();
            }
        }

        private double processRate(int size, string queue)
        {
            CircularBuffer<IMessage> buffer = new CircularBuffer<IMessage>(100);
            IMessageListener listener = new SyncListener(buffer);
            string localQueue = "queue-" + UUID.RandomUuid();
            Session.QueueDeclare(localQueue, null, null, Option.AUTO_DELETE);
            Session.ExchangeBind(localQueue, "amq.direct", queue);
            Session.AttachMessageListener(listener, localQueue);
            Session.MessageSubscribe(localQueue);
            double rate = 0;
            RangeSet range = new RangeSet();
            for (int i = 0; i < size; ++i)
            {
                IMessage m = buffer.Dequeue();
                range.Add(m.Id);
                BinaryReader reader = new BinaryReader(m.Body, Encoding.UTF8);
                byte[] body = new byte[m.Body.Length - m.Body.Position];
                reader.Read(body, 0, body.Length);
                rate += BitConverter.ToDouble(body,0);
            }
            Session.MessageAccept(range);
            return rate;
        }

        private void send(int size, string queue, string data)
        {
            IMessage message = new Message();
            message.DeliveryProperties.SetRoutingKey(queue);
            message.AppendData(Encoding.UTF8.GetBytes(data));
            for (int i = 0; i < size; ++i)
            {
                Session.MessageTransfer("amq.direct", message);
            }
        }

        public override void Start()
        {
            process(Options.Subs, "sub_ready");
            for (int j = 0; j < Options.Iterations; ++j)
            {
                DateTime start = DateTime.Now;
                send(Options.Pubs, "pub_start", "start"); // Start publishers
                double pubrate = processRate(Options.Pubs, "pub_done");
                double subrate = processRate(Options.Subs, "sub_done");
                DateTime end = DateTime.Now;

                double transfers = (Options.Pubs*Options.Count) + (Options.Subs*Options.SubQuota);
                double time = end.Subtract(start).TotalSeconds;
                double txrate = transfers/time;
                double mbytes = (txrate*Options.Size) / (1024 * 1024) ;

                Console.WriteLine("Total: " + transfers + " transfers of " + Options.Size + " bytes in "
                                  + time + " seconds.\n" +
                                  "Publish transfers/sec: " + pubrate * 1000 + "\n" +
                                  "Subscribe transfers/sec:  " + subrate * 1000 + "\n" +
                                  "Total transfers/sec:      " + txrate + "\n" +
                                  "Total Mbytes/sec:         " + mbytes);
                Console.WriteLine("done");
            }
            
        }
    }


    public class PerfTest
    {
        private static int Main(string[] args)
        {
            Options options = new Options();
            CommandLineParser parser = new CommandLineParser(options);
            parser.Parse();
            if (parser.HasErrors)
            {
                Console.WriteLine(parser.UsageInfo.GetErrorsAsString(78));
                return -1;
            }
            if (options.Help)
            {
                Console.WriteLine(parser.UsageInfo.GetOptionsAsString(78));
                return 0;
            }
            bool singleProcess =
                (!options.Setup && !options.Control && !options.Publish && !options.Subscribe);
            if (singleProcess)
            {
                options.Setup = options.Control = options.Publish =  true;
                options.Subscribe = true;
            }
                

            string exchange = "amq.direct";
            switch (options.Mode)
            {
                case "shared":
                    options.SubQuota = (options.Pubs*options.Count)/options.Subs;
                    break;
                case "fanout":
                    options.SubQuota = (options.Pubs*options.Count);
                    exchange = "amq.fanout";
                    break;
                case "topic":
                    options.SubQuota = (options.Pubs*options.Count);
                    exchange = "amq.topic";
                    break;
            }

            if (options.Setup)
            {
                SetupTest setup = new SetupTest(options);
                setup.Start(); // Set up queues
            }

            Thread contT = null;
            if ( options.Control)
            {
                Controller c = new Controller(options);
                contT = new Thread(c.Start);
                contT.Start();
            }

            Thread[] publishers = null;
            Thread[] subscribers = null;

            // Start pubs/subs for each queue/topic.
            for (int i = 0; i < options.Queues; ++i)
            {
                string key = "perftest" + i; // Queue or topic name.
                if (options.Publish)
                {
                    int n = singleProcess ? options.Pubs : 1;
                    publishers = new Thread[n];
                    for (int j = 0; j < n; ++j)
                    {
                        PublishThread pt = new PublishThread(options, key, exchange);
                        publishers[i] = new Thread(pt.Start);
                        publishers[i].Start();
                    }
                }
                if ( options.Subscribe)
                {
                    int n = singleProcess ? options.Subs : 1;
                    subscribers = new Thread[n];
                    for (int j = 0; j < n; ++j)
                    {
                        SubscribeThread st;
                        if (options.Mode.Equals("shared"))
                            st = new SubscribeThread(options, key);
                        else
                            st = new SubscribeThread(options, key, exchange);
                        subscribers[i] = new Thread(st.Start);
                        subscribers[i].Start();
                    }
                }
            }

            if (options.Control)
            {
                contT.Join();
            }


            // Wait for started threads.
            if (options.Publish)
            {
                foreach (Thread t in publishers)
                {
                    t.Join();
                }
            }

            if (options.Subscribe)
            {
                foreach (Thread t in subscribers)
                {
                    t.Join();
                }
            }


            return 0;
        }
    }
}
