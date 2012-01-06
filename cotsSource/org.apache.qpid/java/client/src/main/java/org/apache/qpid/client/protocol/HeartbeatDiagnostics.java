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
package org.apache.qpid.client.protocol;

class HeartbeatDiagnostics
{
    private static final Diagnostics _impl = init();

    private static Diagnostics init()
    {
        return Boolean.getBoolean("amqj.heartbeat.diagnostics") ? new On() : new Off();
    }

    static void sent()
    {
        _impl.sent();
    }

    static void timeout()
    {
        _impl.timeout();
    }

    static void received(boolean heartbeat)
    {
        _impl.received(heartbeat);
    }

    static void init(int delay, int timeout)
    {
        _impl.init(delay, timeout);
    }

    private static interface Diagnostics
    {
        void sent();
        void timeout();
        void received(boolean heartbeat);
        void init(int delay, int timeout);
    }

    private static class On implements Diagnostics
    {
        private final String[] messages = new String[50];
        private int i;

        private void save(String msg)
        {
            messages[i++] = msg;
            if(i >= messages.length){
                i = 0;//i.e. a circular buffer
            }
        }

        public void sent()
        {
            save(System.currentTimeMillis() + ": sent heartbeat");
        }

        public void timeout()
        {
            for(int i = 0; i < messages.length; i++)
            {
                if(messages[i] != null)
                {
                    System.out.println(messages[i]);
                }
            }
            System.out.println(System.currentTimeMillis() + ": timed out");
        }

        public void received(boolean heartbeat)
        {
            save(System.currentTimeMillis() + ": received " + (heartbeat ? "heartbeat" : "data"));
        }

        public void init(int delay, int timeout)
        {
            System.out.println(System.currentTimeMillis() + ": initialised delay=" + delay + ", timeout=" + timeout);
        }
    }

    private static class Off implements Diagnostics
    {
        public void sent()
        {

        }
        public void timeout()
        {

        }
        public void received(boolean heartbeat)
        {

        }

        public void init(int delay, int timeout)
        {

        }
    }
}
