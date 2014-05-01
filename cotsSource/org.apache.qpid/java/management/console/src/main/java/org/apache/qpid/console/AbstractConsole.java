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

public class AbstractConsole implements Console
{
    public AbstractConsole()
    {
    }

    public void agentRemoved(Agent agent)
    {
    }

    public void brokerConnected(Broker broker)
    {
    }

    public void brokerDisconnected(Broker broker)
    {
    }

    public void brokerInformation(Broker broker)
    {
    }

    public void eventRecieved(Broker broker, QMFEvent anEvent)
    {
    }

    public void hearbeatRecieved(Agent agent, long timestamp)
    {
    }

    public void methodResponse(Broker broker, long seq, MethodResult response)
    {
    }

    public void newAgent(Agent agent)
    {
    }

    public void newClass(short kind, ClassKey key)
    {
    }

    public void newPackage(String packageName)
    {
    }

    public void objectProperties(Broker broker, QMFObject obj)
    {
    }

    public void objectStatistics(Broker broker, QMFObject obj)
    {
    }

    public Class typeMapping(ClassKey key)
    {
        return QMFObject.class;
    }
}