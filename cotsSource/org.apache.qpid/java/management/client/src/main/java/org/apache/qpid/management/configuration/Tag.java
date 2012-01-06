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

package org.apache.qpid.management.configuration;

/**
 * Configuration Tag catalogue.
 * 
 * @author Andrea Gazzarini
 */
public enum Tag {
	CONFIGURATION { @Override public String toString() { return "configuration"; }},
	BROKER { @Override public String toString() { return "broker"; }},
	HOST { @Override public String toString() { return "host"; }},
	PORT { @Override public String toString() { return "port"; }},
    MAX_POOL_CAPACITY { @Override public String toString() { return "max-pool-capacity"; }},
    MAX_WAIT_TIMEOUT { @Override public String toString() { return "max-wait-timeout"; }},
    INITIAL_POOL_CAPACITY { @Override public String toString() { return "initial-pool-capacity"; }},    
	VIRTUAL_HOST { @Override public String toString() { return "virtual-host"; }},
	USER { @Override public String toString() { return "user"; }},
	PASSWORD { @Override public String toString() { return "password"; }},
	BROKERS { @Override public String toString() { return "brokers"; }},
	WORK_MANAGER { @Override public String toString() { return "work-manager"; }},
	POOL_CAPACITY  { @Override public String toString() { return "pool-capacity"; }},
	KEEP_ALIVE_TIME { @Override public String toString() { return "keep-alive-time"; }};
	
	/**
	 * Returns the enum entry associated to the given tag name.
	 * 
	 * @param name the name of tag.
	 * @return the enum entry associated to the given tag name.
	 */
	public static Tag get(String name) {
		return valueOf(name.replaceAll("-", "_").toUpperCase());
	}
}
