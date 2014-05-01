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

namespace org.apache.qpid.transport
{
    /// <summary> 
    /// SessionDelegate
    /// 
    /// </summary>
    public abstract class SessionDelegate : MethodDelegate<Session>, IProtocolDelegate<Session>
    {
        public void Init(Session ssn, ProtocolHeader hdr)
        {
        }

        public void Control(Session ssn, Method method)
        {
            method.Dispatch(ssn, this);
        }

        public void Command(Session ssn, Method method)
        {
            ssn.Identify(method);
            method.Dispatch(ssn, this);
            if (!method.HasPayload())
            {
                ssn.Processed(method);
            }
        }

        public void Error(Session ssn, ProtocolError error)
        {
        }

        public override void ExecutionResult(Session ssn, ExecutionResult result)
        {
            ssn.Result(result.GetCommandId(), result.GetValue());
        }

        public override void ExecutionException(Session ssn, ExecutionException exc)
        {
            ssn.AddException(exc);
        }

        public override void SessionCompleted(Session ssn, SessionCompleted cmp)
        {           
                RangeSet ranges = cmp.GetCommands();
                RangeSet known = null;
                if (cmp.GetTimelyReply())
                {
                    known = new RangeSet();
                }

                if (ranges != null)
                {
                    foreach (Range range in ranges)
                    {
                        bool advanced = ssn.Complete(range.Lower, range.Upper);
                        if (advanced && known != null)
                        {
                            known.Add(range);
                        }
                    }
                }

                if (known != null)
                {
                    ssn.SessionKnownCompleted(known);
                }           
        }

        public override void SessionKnownCompleted(Session ssn, SessionKnownCompleted kcmp)
        {
            RangeSet kc = kcmp.GetCommands();
            if (kc != null)
            {
                ssn.KnownComplete(kc);
            }
        }

        public override void SessionFlush(Session ssn, SessionFlush flush)
        {
            if (flush.GetCompleted())
            {
                ssn.FlushProcessed();
            }
            if (flush.GetConfirmed())
            {
                ssn.FlushProcessed();
            }
            if (flush.GetExpected())
            {
               // to be done
                //throw new Exception("not implemented");
            }
        }

        public override void SessionCommandPoint(Session ssn, SessionCommandPoint scp)
        {
            ssn.CommandsIn = scp.GetCommandId();
        }

        public override void ExecutionSync(Session ssn, ExecutionSync sync)
        {
            ssn.SyncPoint();
        }
    }
}
