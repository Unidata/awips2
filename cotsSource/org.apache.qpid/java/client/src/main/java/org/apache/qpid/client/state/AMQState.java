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
package org.apache.qpid.client.state;

/**
 * States used in the AMQ protocol. Used by the finite state machine to determine
 * valid responses.
 */
public enum AMQState
{

    CONNECTION_NOT_STARTED(1, "CONNECTION_NOT_STARTED"),

    CONNECTION_NOT_TUNED(2, "CONNECTION_NOT_TUNED"),

    CONNECTION_NOT_OPENED(3, "CONNECTION_NOT_OPENED"),

    CONNECTION_OPEN(4, "CONNECTION_OPEN"),

    CONNECTION_CLOSING(5, "CONNECTION_CLOSING"),

    CONNECTION_CLOSED(6, "CONNECTION_CLOSED");


    private final int _id;

    private final String _name;

    private AMQState(int id, String name)
    {
        _id = id;
        _name = name;
    }

    public String toString()
    {
        return "AMQState: id = " + _id + " name: " + _name;
    }



}
