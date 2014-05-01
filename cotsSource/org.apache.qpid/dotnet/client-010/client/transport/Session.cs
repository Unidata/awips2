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
using org.apache.qpid.transport.util;
using Frame = org.apache.qpid.transport.network.Frame;
using Logger = org.apache.qpid.transport.util.Logger;


namespace org.apache.qpid.transport
{
    /// <summary>
    ///  Session
    /// 
    /// </summary>
    public class Session : Invoker, ISession
    {
        private static readonly Logger log = Logger.Get(typeof (Session));
        private static readonly bool ENABLE_REPLAY;

        static Session()
        {
            const string enableReplay = "enable_command_replay";
            try
            {
                String var = Environment.GetEnvironmentVariable(enableReplay);
                if (var != null)
                {
                    ENABLE_REPLAY = bool.Parse(var);
                }
            }
            catch (Exception)
            {
                ENABLE_REPLAY = false;
            }
        }

        private readonly byte[] _name;
        private const long _timeout = 600000;
        private bool _autoSync = false;

        // channel may be null
        private Channel _channel;

        // incoming command count
        private int _commandsIn = 0;
        // completed incoming commands
        private readonly Object _processedLock = new Object();
        private RangeSet _processed = new RangeSet();
        private int _maxProcessed = - 1;
        private int _syncPoint = -1;

        // outgoing command count
        private int _commandsOut = 0;
        private readonly Dictionary<int, Method> _commands = new Dictionary<int, Method>();
        private int _maxComplete = - 1;
        private bool _needSync = false;
        private bool _closed;
        private readonly Dictionary<int, IFuture> _results = new Dictionary<int, IFuture>();
        private readonly List<ExecutionException> _exceptions = new List<ExecutionException>();


        public bool IsClosed
        {
            get
            {
                lock (this)
                {
                    return _closed;
                }
            }
            set
            {
                lock (this)
                {
                    _closed = value;
                }
            }
        }

        public string Name
        {
            get
            {
                ASCIIEncoding enc = new ASCIIEncoding();
                return enc.GetString(_name);
            }
        }

        public Session(byte[] name)
        {
            _name = name;
        }

        public byte[] GetName()
        {
            return _name;
        }

        public void SetAutoSync(bool value)
        {
            lock (_commands)
            {
                _autoSync = value;
            }
        }

        public Dictionary<int, Method> GetOutstandingCommands()
        {
            return _commands;
        }

        public int GetCommandsOut()
        {
            return _commandsOut;
        }

        public int CommandsIn
        {
            get { return _commandsIn; }
            set { _commandsIn = value; }
        }

        public int NextCommandId()
        {
            return _commandsIn++;
        }

        public void Identify(Method cmd)
        {
            int id = NextCommandId();
            cmd.Id = id;

            if (log.IsDebugEnabled())
            {
                log.Debug("ID: [{0}] %{1}", _channel, id);
            }

            //if ((id % 65536) == 0)
            if ((id & 0xff) == 0)
            {
                FlushProcessed(Option.TIMELY_REPLY);
            }
        }

        public void Processed(Method command)
        {
            Processed(command.Id);
        }

        public void Processed(int command)
        {
            Processed(new Range(command, command));
        }

        public void Processed(int lower, int upper)
        {
            Processed(new Range(lower, upper));
        }

        public void Processed(Range range)
        {
            log.Debug("{0} processed({1})", this, range);

            bool flush;
            lock (_processedLock)
            {
                _processed.Add(range);
                Range first = _processed.GetFirst();
                int lower = first.Lower;
                int upper = first.Upper;
                int old = _maxProcessed;
                if (Serial.Le(lower, _maxProcessed + 1))
                {
                    _maxProcessed = Serial.Max(_maxProcessed, upper);
                }
                flush = Serial.Lt(old, _syncPoint) && Serial.Ge(_maxProcessed, _syncPoint);
                _syncPoint = _maxProcessed;
            }
            if (flush)
            {
                FlushProcessed();
            }
        }

        public void FlushProcessed(params Option[] options)
        {
            RangeSet copy;
            lock (_processedLock)
            {
                copy = _processed.Copy();
            }
            SessionCompleted(copy, options);
        }

        public void KnownComplete(RangeSet kc)
        {
            lock (_processedLock)
            {
                RangeSet newProcessed = new RangeSet();
                foreach (Range pr in _processed)
                {
                    foreach (Range kr in kc)
                    {
                        foreach (Range r in pr.Subtract(kr))
                        {
                            newProcessed.Add(r);
                        }
                    }
                }
                _processed = newProcessed;
            }
        }

        public void SyncPoint()
        {
            int id = CommandsIn - 1;
            log.Debug("{0} synced to {1}", this, id);
            bool flush;
            lock (_processedLock)
            {
                _syncPoint = id;
                flush = Serial.Ge(_maxProcessed, _syncPoint);
            }
            if (flush)
            {
                FlushProcessed();
            }
        }

        public void Attach(Channel channel)
        {
            _channel = channel;
            _channel.Session = this;
        }

        public Method GetCommand(int id)
        {
            lock (_commands)
            {
                return _commands[id];
            }
        }

        public bool Complete(int lower, int upper)
        {
            //avoid autoboxing
            if (log.IsDebugEnabled())
            {
                log.Debug("{0} complete({1}, {2})", this, lower, upper);
            }
            lock (_commands)
            {
                int old = _maxComplete;
                for (int id = Serial.Max(_maxComplete, lower); Serial.Le(id, upper); id++)
                {
                    _commands.Remove(id);
                }
                if (Serial.Le(lower, _maxComplete + 1))
                {
                    _maxComplete = Serial.Max(_maxComplete, upper);
                }
                log.Debug("{0} commands remaining: {1}", this, _commands);
                Monitor.PulseAll(_commands);
                return Serial.Gt(_maxComplete, old);
            }
        }

        protected override void Invoke(Method m)
        {
            if (IsClosed)
            {
                List<ExecutionException> exc = GetExceptions();
                if (exc.Count > 0)
                {
                    throw new SessionException(exc);
                }
                else if (_close != null)
                {
                    throw new ConnectionException(_close);
                }
                else
                {
                    throw new SessionClosedException();
                }
            }

            if (m.EncodedTrack == Frame.L4)
            {
                lock (_commands)
                {
                    int next = _commandsOut++;
                    m.Id = next;
                    if (next == 0)
                    {
                        SessionCommandPoint(0, 0);
                    }
                    if (ENABLE_REPLAY)
                    {
                        _commands.Add(next, m);
                    }
                    if (_autoSync)
                    {
                        m.Sync = true;
                    }
                    _needSync = ! m.Sync;
                    _channel.Method(m);
                    if (_autoSync)
                    {
                        Sync();
                    }

                    // flush every 64K commands to avoid ambiguity on
                    // wraparound
                    if ((next%65536) == 0)
                    {
                        SessionFlush(Option.COMPLETED);
                    }
                }
            }
            else
            {
                _channel.Method(m);
            }
        }

        public void Sync()
        {
            Sync(_timeout);
        }

        public void Sync(long timeout)
        {
            log.Debug("{0} sync()", this);
            lock (_commands)
            {
                int point = _commandsOut - 1;

                if (_needSync && Serial.Lt(_maxComplete, point))
                {
                    ExecutionSync(Option.SYNC);
                }

                DateTime start = DateTime.Now;
                long elapsed = 0;

                while (!IsClosed && elapsed < timeout && Serial.Lt(_maxComplete, point))
                {
                    log.Debug("{0}   waiting for[{1}]: {2}, {3}", this, point,
                              _maxComplete, _commands);
                    Monitor.Wait(_commands, (int) (timeout - elapsed));
                    elapsed = DateTime.Now.Subtract(start).Milliseconds;
                }

                if (Serial.Lt(_maxComplete, point))
                {
                    if (IsClosed)
                    {
                        throw new SessionException(GetExceptions());
                    }
                    else
                    {
                        throw new Exception
                            (String.Format
                                 ("timed out waiting for sync: complete = {0}, point = {1}", _maxComplete, point));
                    }
                }
            }
        }


        public void Result(int command, Struct result)
        {
            IFuture future;
            lock (_results)
            {
                if (_results.ContainsKey(command))
                {
                    future = _results[command];
                    _results.Remove(command);
                }
                else
                {
                    throw new Exception(String.Format("Cannot ger result {0} for {1}", command, result));
                }
            }
            future.Result = result;
        }

        public void AddException(ExecutionException exc)
        {
            lock (_exceptions)
            {
                _exceptions.Add(exc);
            }
        }

        private ConnectionClose _close = null;

        public void CloseCode(ConnectionClose close)
        {
            _close = close;
        }

        public List<ExecutionException> GetExceptions()
        {
            lock (_exceptions)
            {
                return new List<ExecutionException>(_exceptions);
            }
        }

        public override IFuture Invoke(Method m, IFuture future)     
        {
            lock (_commands)
            {
                future.Session = this;
                int command = _commandsOut;
                lock (_results)
                {
                    _results.Add(command, future);
                }
                Invoke(m);
            }
            return future;
        }


        public void MessageTransfer(String destination,
                                    MessageAcceptMode acceptMode,
                                    MessageAcquireMode acquireMode,
                                    Header header,
                                    byte[] body,
                                    params Option[] options)
        {
            MemoryStream mbody = new MemoryStream();
            mbody.Write(body,0, body.Length);
            MessageTransfer(destination, acceptMode, acquireMode, header,
                            mbody, options);
        }

        public void MessageTransfer(String destination,
                                    MessageAcceptMode acceptMode,
                                    MessageAcquireMode acquireMode,
                                    Header header,
                                    String body,
                                    params Option[] options)
        {
            MessageTransfer(destination, acceptMode, acquireMode, header,
                            new MemoryStream(Convert.ToByte(body)), options);
        }

        public void Close()
        {
            SessionRequestTimeout(0);
            SessionDetach(_name);
            lock (_commands)
            {
                DateTime start = DateTime.Now;
                long elapsed = 0;

                while (!IsClosed && elapsed < _timeout)
                {
                    Monitor.Wait(_commands, (int) (_timeout - elapsed));
                    elapsed = DateTime.Now.Subtract(start).Milliseconds;
                }
            }
        }

        public void Exception(Exception t)
        {
            log.Error(t, "Caught exception");
        }

        public void Closed()
        {
            IsClosed = true;
            lock (_commands)
            {
                Monitor.PulseAll(_commands);
            }
            lock (_results)
            {
                foreach (IFuture result in _results.Values)
                {
                    lock (result)
                    {
                        Monitor.PulseAll(result);
                    }
                }
            }
            _channel.Session = null;
            _channel = null;
        }

        public override String ToString()
        {
            return String.Format("session:{0}", _name);
        }
    }
}
