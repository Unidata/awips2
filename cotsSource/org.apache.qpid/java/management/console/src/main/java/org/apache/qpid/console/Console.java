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

public interface Console
{
    void agentRemoved(Agent agent);

    void brokerConnected(Broker broker);

    void brokerDisconnected(Broker broker);

    void brokerInformation(Broker broker);

    void eventRecieved(Broker broker, QMFEvent anEvent);

    void hearbeatRecieved(Agent agent, long timestamp);

    void methodResponse(Broker broker, long seq, MethodResult response);

    void newAgent(Agent agent);

    void newClass(short kind, ClassKey key);

    void newPackage(String packageName);

    void objectProperties(Broker broker, QMFObject obj);

    void objectStatistics(Broker broker, QMFObject obj);

    @SuppressWarnings("unchecked")
    Class typeMapping(ClassKey key);
}