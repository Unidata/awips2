/*
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
*/

namespace Apache.Qpid.Test.Channel.WcfPerftest
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Configuration;
    using System.Diagnostics;
    using System.IO;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.ServiceModel.Description;
    using System.Threading;
    using System.Transactions;
    using System.Text;
    using System.Xml;
    using Apache.Qpid.AmqpTypes;
    using Apache.Qpid.Channel;

    // this program implements a subset of the functionality in qpid\cpp\src\tests\perftest.cpp

    // for a given broker, create reader and writer channels to queues/exchanges
    // lazilly creates binding and channel factories

    public class QueueChannelFactory
    {
        private static AmqpBinding brokerBinding;
        private static IChannelFactory<IInputChannel> readerFactory;
        private static IChannelFactory<IOutputChannel> writerFactory;
        private static string brokerAddr = "127.0.0.1";
        private static int brokerPort = 5672;

        public static void SetBroker(string addr, int port)
        {
            brokerAddr = addr;
            brokerPort = port;
        }

        private static void InitializeBinding()
        {
            AmqpBinaryBinding binding = new AmqpBinaryBinding();
            binding.BrokerHost = brokerAddr;
            binding.BrokerPort = brokerPort;
            binding.TransferMode = TransferMode.Streamed;
            binding.PrefetchLimit = 5000;
            binding.Shared = true;
            brokerBinding = binding;
        }

        public static IInputChannel CreateReaderChannel(string queueName)
        {
            lock (typeof(QueueChannelFactory))
            {
                if (brokerBinding == null)
                {
                    InitializeBinding();
                }

                if (readerFactory == null)
                {
                    readerFactory = brokerBinding.BuildChannelFactory<IInputChannel>();
                    readerFactory.Open();
                }

                IInputChannel channel = readerFactory.CreateChannel(new EndpointAddress(
                    new Uri("amqp:" + queueName)));
                channel.Open();

                return channel;
            }
        }

        public static IOutputChannel CreateWriterChannel(string exchangeName, string routingKey)
        {
            lock (typeof(QueueChannelFactory))
            {
                if (brokerBinding == null)
                {
                    InitializeBinding();
                }

                if (writerFactory == null)
                {
                    writerFactory = brokerBinding.BuildChannelFactory<IOutputChannel>();
                    writerFactory.Open();
                }

                IOutputChannel channel = writerFactory.CreateChannel(new EndpointAddress(
                    "amqp:" + exchangeName +
                    "?routingkey=" + routingKey));
                channel.Open();

                return channel;
            }
        }
    }

    public enum ClientType
    {
        Publisher,
        Subscriber,
        InteropDemo
    }

    public class Options
    {
        public string broker;
        public int port;
        public UInt64 messageCount;
        public int messageSize;
        public ClientType type;
        public string baseName;
        public int subTxSize;
        public int pubTxSize;
        public bool durable;

        public Options()
        {
            this.broker = "127.0.0.1";
            this.port = 5672;
            this.messageCount = 500000;
            this.messageSize = 1024;
            this.type = ClientType.InteropDemo;  // default: once as pub and once as sub
            this.baseName = "perftest";
            this.pubTxSize = 0;
            this.subTxSize = 0;
            this.durable = false;
        }

        public void Parse(string[] args)
        {
            int argCount = args.Length;
            int current = 0;
            bool typeSelected = false;

            while (current < argCount)
            {
                string arg = args[current];
                if (arg == "--publish")
                {
                    if (typeSelected)
                        throw new ArgumentException("too many roles");

                    this.type = ClientType.Publisher;
                    typeSelected = true;
                }
                else if (arg == "--subscribe")
                {
                    if (typeSelected)
                        throw new ArgumentException("too many roles");

                    this.type = ClientType.Subscriber;
                    typeSelected = true;
                }
                else if (arg == "--size")
                {
                    arg = args[++current];
                    int i = int.Parse(arg);
                    if (i > 0)
                    {
                        this.messageSize = i;
                    }
                }
                else if (arg == "--count")
                {
                    arg = args[++current];
                    UInt64 i = UInt64.Parse(arg);
                    if (i > 0)
                    {
                        this.messageCount = i;
                    }
                }
                else if (arg == "--broker")
                {
                    this.broker = args[++current];
                }
                else if (arg == "--port")
                {
                    arg = args[++current];
                    int i = int.Parse(arg);
                    if (i > 0)
                    {
                        this.port = i;
                    }
                }
                else if (arg == "--base-name")
                {
                    this.baseName = args[++current];
                }

                else if (arg == "--tx")
                {
                    arg = args[++current];
                    int i = int.Parse(arg);
                    if (i > 0)
                    {
                        this.subTxSize = i;
                        this.pubTxSize = i;
                    }
                }

                else if (arg == "--durable")
                {
                    arg = args[++current];
                    if (arg.Equals("yes"))
                    {
                        this.durable = true;
                    }
                }
                
                current++;
            }
        }
    }


    public class Client
    {
        protected Options opts;

        public static void Expect(string actual, string expect)
        {
            if (expect != actual)
            {
                throw new Exception("Expecting " + expect + " but received " + actual);
            }
        }

        public static void Close(IChannel channel)
        {
            if (channel == null)
            {
                return;
            }

            try
            {
                channel.Close();
            }
            catch (Exception e)
            {
                Console.WriteLine("channel close exception {0}", e);
            }
        }

        public string Fqn(string name)
        {
            return opts.baseName + '_' + name;
        }
    }


    public class WcfPerftest
    {
        static void WarmUpTransactionSubsystem(Options opts)
        {
            // see if any use of transactions is expected
            if ((opts.type == ClientType.Publisher) && (opts.pubTxSize == 0))
                return;

            if ((opts.type == ClientType.Subscriber) && (opts.subTxSize == 0))
                return;

            if (opts.type == ClientType.InteropDemo)
            {
                if ((opts.subTxSize == 0) && (opts.pubTxSize == 0))
                    return;
            }

            Console.WriteLine("Initializing transactions");
            IRawBodyUtility bodyUtil = new RawEncoderUtility();

            // send a transacted message to nowhere to force the initial registration with MSDTC
            IOutputChannel channel = QueueChannelFactory.CreateWriterChannel("", Guid.NewGuid().ToString());
            Message msg = bodyUtil.CreateMessage("sacrificial transacted message from WcfPerftest");
            using (TransactionScope ts = new TransactionScope())
            {
                channel.Send(msg);
                // abort/rollback
                ts.Dispose();
            }
            channel.Close();
            Console.WriteLine("transaction resource manager ready");
        }

        static void InteropDemo(Options opts)
        {
            string perftest_cpp_exe = "perftest.exe";
            string commonArgs = String.Format(" --count {0} --size {1}", opts.messageCount, opts.messageSize);

            if (opts.durable)
            {
                commonArgs += " --durable yes";
            }

            Console.WriteLine("===== WCF Subscriber and C++ Publisher =====");

            Process setup = new Process();
            setup.StartInfo.FileName = perftest_cpp_exe;
            setup.StartInfo.UseShellExecute = false;
            setup.StartInfo.Arguments = "--setup" + commonArgs;
            try
            {
                setup.Start();
            }
            catch (Win32Exception win32e)
            {
                Console.WriteLine("Cannot execute {0}: PATH not set?", perftest_cpp_exe);
                Console.WriteLine("   Error: {0}", win32e.Message);
                return;
            }
            setup.WaitForExit();

            Process control = new Process();
            control.StartInfo.FileName = perftest_cpp_exe;
            control.StartInfo.UseShellExecute = false;
            control.StartInfo.Arguments = "--control" + commonArgs;
            control.Start();

            Process publish = new Process();
            publish.StartInfo.FileName = perftest_cpp_exe;
            publish.StartInfo.UseShellExecute = false;
            publish.StartInfo.Arguments = "--publish" + commonArgs;
            publish.Start();

            SubscribeThread subscribeWcf = new SubscribeThread(opts.baseName + "0", opts);
            Thread subThread = new Thread(subscribeWcf.Run);
            subThread.Start();

            subThread.Join();
            publish.WaitForExit();
            control.WaitForExit();

            Console.WriteLine();
            Console.WriteLine("=====  WCF Publisher and C++ Subscriber =====");

            setup = new Process();
            setup.StartInfo.FileName = perftest_cpp_exe;
            setup.StartInfo.UseShellExecute = false;
            setup.StartInfo.Arguments = "--setup" + commonArgs;
            setup.Start();
            setup.WaitForExit();

            control = new Process();
            control.StartInfo.FileName = perftest_cpp_exe;
            control.StartInfo.UseShellExecute = false;
            control.StartInfo.Arguments = "--control" + commonArgs;
            control.Start();

            PublishThread pub = new PublishThread(opts.baseName + "0", "", opts);
            Thread pubThread = new Thread(pub.Run);
            pubThread.Start();

            Process subscribeCpp = new Process();
            subscribeCpp.StartInfo.FileName = perftest_cpp_exe;
            subscribeCpp.StartInfo.UseShellExecute = false;
            subscribeCpp.StartInfo.Arguments = "--subscribe" + commonArgs;
            subscribeCpp.Start();

            subscribeCpp.WaitForExit();
            pubThread.Join();
            control.WaitForExit();
        }

        static void Main(string[] mainArgs)
        {
            Options opts = new Options();
            opts.Parse(mainArgs);
            QueueChannelFactory.SetBroker(opts.broker, opts.port);

            WarmUpTransactionSubsystem(opts);

            if (opts.type == ClientType.Publisher)
            {
                PublishThread pub = new PublishThread(opts.baseName + "0", "", opts);
                Thread pubThread = new Thread(pub.Run);
                pubThread.Start();
                pubThread.Join();
            }
            else if (opts.type == ClientType.Subscriber)
            {
                SubscribeThread sub = new SubscribeThread(opts.baseName + "0", opts);
                Thread subThread = new Thread(sub.Run);
                subThread.Start();
                subThread.Join();
            }
            else
            {
                InteropDemo(opts);   
            }

            if (System.Diagnostics.Debugger.IsAttached)
            {
                Console.WriteLine("Hit return to continue...");
                Console.ReadLine();
            }
        }
    }

    public class PublishThread : Client
    {
        string destination; // exchange/queue
        string routingKey;
        int msgSize;
        UInt64 msgCount;
        IOutputChannel publishQueue;

        public PublishThread(string key, string q, Options opts)
        {
            this.routingKey = key;
            this.destination = q;
            this.msgSize = opts.messageSize;
            this.msgCount = opts.messageCount;
            this.opts = opts;
        }

        static void StampSequenceNo(byte[] data, UInt64 n)
        {
            int wordLen = IntPtr.Size;  // mimic size_t in C++

            if (data.Length < wordLen)
                throw new ArgumentException("message size");
            for (int i = 0; i < wordLen; i++)
            {
                data[i] = (byte) (n & 0xff);
                n >>= 8;
            }
        }

        public void Run()
        {
            IRawBodyUtility bodyUtil = new RawEncoderUtility();

            IInputChannel startQueue = null;
            IOutputChannel doneQueue = null;
            UInt64 batchSize = (UInt64)opts.pubTxSize;
            bool txPending = false;
            AmqpProperties amqpProperties = null;

            if (opts.durable)
            {
                amqpProperties = new AmqpProperties();
                amqpProperties.Durable = true;
            }

            try
            {
                publishQueue = QueueChannelFactory.CreateWriterChannel(this.destination, this.routingKey);
                doneQueue = QueueChannelFactory.CreateWriterChannel("", this.Fqn("pub_done"));
                startQueue = QueueChannelFactory.CreateReaderChannel(this.Fqn("pub_start"));

                // wait for our start signal
                Message msg;
                msg = startQueue.Receive(TimeSpan.MaxValue);
                Expect(bodyUtil.GetText(msg), "start");
                msg.Close();

                Stopwatch stopwatch = new Stopwatch();
                AsyncCallback sendCallback = new AsyncCallback(this.AsyncSendCB);

                byte[] data = new byte[this.msgSize];
                IAsyncResult sendResult = null;

                Console.WriteLine("sending {0}", this.msgCount);
                stopwatch.Start();

                if (batchSize > 0)
                {
                    Transaction.Current = new CommittableTransaction();
                }

                for (UInt64 i = 0; i < this.msgCount; i++)
                {
                    StampSequenceNo(data, i);
                    msg = bodyUtil.CreateMessage(data);
                    if (amqpProperties != null)
                    {
                        msg.Properties.Add("AmqpProperties", amqpProperties);
                    }

                    sendResult = publishQueue.BeginSend(msg, TimeSpan.MaxValue, sendCallback, msg);

                    if (batchSize > 0)
                    {
                        txPending = true;
                        if (((i + 1) % batchSize) == 0)
                        {
                            ((CommittableTransaction)Transaction.Current).Commit();
                            txPending = false;
                            Transaction.Current = new CommittableTransaction();
                        }
                    }
                }

                if (txPending)
                {
                    ((CommittableTransaction)Transaction.Current).Commit();
                }

                Transaction.Current = null;

                sendResult.AsyncWaitHandle.WaitOne();
                stopwatch.Stop();

                double mps = (msgCount / stopwatch.Elapsed.TotalSeconds);

                msg = bodyUtil.CreateMessage(String.Format("{0:0.##}", mps));
                doneQueue.Send(msg, TimeSpan.MaxValue);
                msg.Close();
            }
            finally
            {
                Close((IChannel)doneQueue);
                Close((IChannel)publishQueue);
                Close(startQueue);
            }
        }

        void AsyncSendCB(IAsyncResult result)
        {
            publishQueue.EndSend(result);
            ((Message)result.AsyncState).Close();
        }
    }

    public class SubscribeThread : Client
    {
        string queue;
        int msgSize;
        UInt64 msgCount;
        IInputChannel subscribeQueue;

        public SubscribeThread(string q, Options opts)
        {
            this.queue = q;
            this.msgSize = opts.messageSize;
            this.msgCount = opts.messageCount;
            this.opts = opts;
        }

        static UInt64 GetSequenceNumber(byte[] data)
        {
            int wordLen = IntPtr.Size;  // mimic size_t in C++

            if (data.Length < wordLen)
                throw new ArgumentException("message size");
            UInt64 n = 0;
            for (int i = (wordLen - 1); i >= 0; i--)
            {
                n = (256 * n) + data[i];
            }
            return n;
        }

        public void Run()
        {
            IRawBodyUtility bodyUtil = new RawEncoderUtility();

            IOutputChannel readyQueue = null;
            IOutputChannel doneQueue = null;
            UInt64 batchSize = (UInt64)opts.subTxSize;
            bool txPending = false;
            byte[] data = null;

            try
            {
                this.subscribeQueue = QueueChannelFactory.CreateReaderChannel(this.queue);
                readyQueue = QueueChannelFactory.CreateWriterChannel("", this.Fqn("sub_ready"));
                doneQueue = QueueChannelFactory.CreateWriterChannel("", this.Fqn("sub_done"));

                Message msg = bodyUtil.CreateMessage("ready");
                readyQueue.Send(msg, TimeSpan.MaxValue);
                msg.Close();


                Stopwatch stopwatch = new Stopwatch();
                stopwatch.Start();

                Console.WriteLine("receiving {0}", this.msgCount);
                UInt64 expect = 0;

                if (batchSize > 0)
                {
                    Transaction.Current = new CommittableTransaction();
                }

                for (UInt64 i = 0; i < this.msgCount; i++)
                {
                    msg = subscribeQueue.Receive(TimeSpan.MaxValue);

                    data = bodyUtil.GetBytes(msg, data);
                    msg.Close();
                    if (data.Length != this.msgSize)
                    {
                        throw new Exception("subscribe message size mismatch");
                    }

                    UInt64 n = GetSequenceNumber(data);
                    if (n != expect)
                    {
                        throw new Exception(String.Format("message sequence error. expected {0} got {1}", expect, n));
                    }
                    expect = n + 1;

                    if (batchSize > 0)
                    {
                        txPending = true;
                        if (((i + 1) % batchSize) == 0)
                        {
                            ((CommittableTransaction)Transaction.Current).Commit();
                            txPending = false;
                            Transaction.Current = new CommittableTransaction();
                        }
                    }
                }

                if (txPending)
                {
                    ((CommittableTransaction)Transaction.Current).Commit();
                }

                Transaction.Current = null;

                stopwatch.Stop();

                double mps = (msgCount / stopwatch.Elapsed.TotalSeconds);

                msg = bodyUtil.CreateMessage(String.Format("{0:0.##}", mps));
                doneQueue.Send(msg, TimeSpan.MaxValue);
                msg.Close();

                subscribeQueue.Close();
            }
            finally
            {
                Close((IChannel)doneQueue);
                Close((IChannel)this.subscribeQueue);
                Close(readyQueue);
            }
        }
    }
}
