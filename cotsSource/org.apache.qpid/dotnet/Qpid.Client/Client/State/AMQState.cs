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
namespace Apache.Qpid.Client.State
{
    public enum AMQState
    {
        CONNECTION_NOT_STARTED,    
        CONNECTION_NOT_TUNED,    
        CONNECTION_NOT_OPENED,
        CONNECTION_OPEN,
        CONNECTION_CLOSING,    
        CONNECTION_CLOSED,
        ALL // all is a special state used in the state manager. It is not valid to be "in" the state "all".
    }
}


