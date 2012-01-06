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
package org.apache.qpid.management.domain.handler.impl;

import java.net.URI;
import java.util.Date;
import java.util.Map;
import java.util.UUID;

/**
 * Management interface for Qpid domain object.
 * 
 * @author Andrea Gazzarini
 */
public interface QpidDomainObjectMBean
{		
	/**
	 * A method that is throwing an exception, everytime.
	 * 
	 * @throws Exception each time the method is called.
	 */
	void throwsException() throws Exception;
	
	/**
	 * Sample echo method that return an empty result object.
	 * That is, an object with only status code / text valorized 
	 * (no output parameters).
	 * 
	 * @return an empty result object.
	 */
	InvocationResult voidWithoutArguments(); 
	
	/**
	 * Echo method that accepts and returns wrapper types.
	 * 
	 * @param aLong a java.lang.Long
	 * @param aBoolean a java.lang.Boolean
	 * @param aDouble a java.lang.Double
	 * @param aFloat a java.lang.Float
	 * @param anInteger a java.lang.Integer
	 * @param aShort a java.lang.Short
	 * @param aString a java.lang.String
	 * @param anURI a java.net.URI
	 * @param aDate a java.util.Date
	 * @return a result object with the same given parameters (as output parameters)
	 */
	InvocationResult echoWithSimpleTypes(
			Long aLong,
			Boolean aBoolean,
			Double aDouble,
			Float aFloat,
			Integer anInteger,
			Short aShort,
			String aString,
			URI anURI,
			Date aDate);

	/**
	 *  Echo method that accepts and returns wrapper type arrays .
	 *  
	 * @param longs an array of java.lang.Long
	 * @param booleans an array of java.lang.Boolean
	 * @param doubles an array of java.lang.Double
	 * @param floats an array of java.lang.Float
	 * @param integers an array of java.lang.Integer
	 * @param shorts an array of java.lang.Short
	 * @param strings an array of java.lang.String
	 * @param uris an array of java.net.URI
	 * @param dates an array of java.util.Date
	 * @return a result object with the same input parameters (as output parameters).
	 */
	InvocationResult echoWithArrays(
			Long [] longs,
			Boolean [] booleans,
			Double [] doubles,
			Float [] floats,
			Integer [] integers,
			Short [] shorts,
			String [] strings,
			URI [] uris,
			Date [] dates);

	/**
	 * Echo method that accepts and returns primitive type arrays.
	 * 
	 * @param longs an array of long.
	 * @param booleans an array of boolean.
	 * @param doubles an array of double.
	 * @param floats an array of float.
	 * @param integers an array of int.
	 * @param shorts an array of short.
	  * @return a result object with the same input parameters (as output parameters).
	 */
	InvocationResult echoWithSimpleTypeArrays(
			long [] longs,
			boolean [] booleans,
			double [] doubles,
			float [] floats,
			int [] integers,
			short [] shorts);	
	
	/**
	 * Echo method that accepts and returns a byte array.
	 * 
	 * @param byteArray a byte array
	 * @return a result containing the input byte array (as output parameter)
	 */
	InvocationResult echoWithByteArray(byte [] byteArray);
	
	/**
	 * Echo method that accepts and returns an UUID.
	 * 
	 * @param uuid a java.util.UUID.
	 * @return a result containing the input UUID (as output parameter)
	 */
	InvocationResult echoWithUUID(UUID uuid);
	
	/**
	 * Echo method that accepts and returns a Map.
	 * 
	 * @param map a java.util.Map.
	 * @return a result containing the input Map (as output parameter)
	 */
	InvocationResult echoWithMap(Map<String,Object> map);

	/**
	 * Returns the VHostRef property value.
	 * 
	 * @return the VHostRef property value.
	 */
	UUID getVhostRef();

	/**
	 * Returns the name property value.
	 * 
	 * @return the name  property value.
	 */
	String getName();

	/**
	 * Returns the durable property value.
	 * 
	 * @return the durable property value.
	 */
	Boolean getDurable();

	/**
	 * Returns the arguments property value.
	 * 
	 * @return the arguments property value.
	 */
	Map<String, Object> getArguments();

	/**
	 * Returns the msgTotalEnqueues property value.
	 * 
	 * @return the msgTotalEnqueues property value.
	 */
	Long getMsgTotalEnqueues();

	/**
	 * Returns the consumerCount property value.
	 * 
	 * @return the consumerCount property value.
	 */	
	Integer getConsumerCount();
	
	/**
	 * Returns the mgmtPubInterval property value.
	 * 
	 * @return the mgmtPubInterval property value.
	 */	
	Short getMgmtPubInterval();
	
	/**
	 * Sets the mgmtPubInterval property value.
	 * 
	 * @param the mgmtPubInterval property value.
	 */		
	void setMgmtPubInterval(Short mgmtPubInterval);
	
	/**
	 * Returns the expireTime property value.
	 * 
	 * @return the expireTime property value.
	 */	
	Date getExpireTime();
	
	/**
	 * Sets the expireTime property value.
	 * 
	 * @return the expireTime property value.
	 */	
	void setExpireTime(Date expireTime);
	
	/**
	 * Returns the type property value.
	 * 
	 * @return the type property value.
	 */		
	void setType(String type);
	
	/**
	 * Sets the type property value.
	 * 
	 * @return the type property value.
	 */	
	String getType();
	
//	/**
//	 * Returns the byteArray property value.
//	 * 
//	 * @return the byteArray property value.
//	 */
//	byte[] getByteArray();
}