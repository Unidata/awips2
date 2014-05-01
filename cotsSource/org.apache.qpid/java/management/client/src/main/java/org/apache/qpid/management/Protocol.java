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
package org.apache.qpid.management;

/**
 * Protocol defined constants.
 * 
 * @author Andrea Gazzarini
 */
public interface Protocol
{
    String MAGIC_NUMBER = "AM2";
    
    char SCHEMA_REQUEST_OPCODE = 'S';
    char SCHEMA_RESPONSE_OPCODE = Character.toLowerCase(SCHEMA_REQUEST_OPCODE);
    
    char OPERATION_INVOCATION_REQUEST_OPCODE = 'M';
    char OPERATION_INVOCATION_RESPONSE_OPCODE = Character.toLowerCase(OPERATION_INVOCATION_REQUEST_OPCODE);
    
    char INSTRUMENTATION_CONTENT_RESPONSE_OPCODE = 'i';
    char CONFIGURATION_CONTENT_RESPONSE_OPCDE = 'c';
    char EVENT_CONTENT_RESPONSE_OPCDE = 'e';
    char INSTR_AND_CONFIG_CONTENT_RESPONSE_OPCODE = 'g';
    
    char HEARTBEAT_INDICATION_RESPONSE_OPCODE = 'h';
    
    int CLASS = 1;
    int EVENT = 2;
    
    String DEFAULT_QMAN_HOSTNAME = "localhost";
    int DEFAULT_QMAN_PORT_NUMBER = 8080;
    
    String DEFAULT_ENDPOINT_URI = "http://localhost:8080/qman/services/adapter";
}
