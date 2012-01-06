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
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.qpid.management.domain.services.MethodInvocationException;

/**
 * This is a sample entity used on QMan test case.
 * 
 * @author Andrea Gazzarini
 */
public class QpidDomainObject implements QpidDomainObjectMBean
{
	private UUID _vhostRef;
	private String _name;
	private Boolean _durable;
	private Map<String, Object> _arguments;
	private Long _msgTotalEnqueues;
	private Integer _consumerCount;
	private Short _mgmtPubInterval;
	private Date _expireTime;
	private String _type;
	private byte [] _byteArray;
	
	/**
	 * Builds a new QpidDomainObject with default values for 
	 * its properties.
	 */
	public QpidDomainObject()
	{
		_vhostRef = UUID.randomUUID();
		_name = "Initial Name";
		_durable = Boolean.TRUE;
		_arguments = new HashMap<String, Object>();	
		_arguments.put("Key1", "aStringValue");
		_arguments.put("Key2", Long.MIN_VALUE);
		_arguments.put("Key3", Integer.MAX_VALUE);
		_arguments.put("Key4", Double.MIN_VALUE);
		_arguments.put("Key4", Float.MAX_VALUE);

		_msgTotalEnqueues = Long.MAX_VALUE-10;
		_consumerCount =  Integer.MIN_VALUE+10;
		_mgmtPubInterval = Short.MAX_VALUE;
		_expireTime = new Date(Long.MAX_VALUE);
		_byteArray = new byte[]{1,2,3,5,6,7,8,7,56};
	}
	
	/**
	 * A method that is throwing an exception, everytime.
	 * 
	 * @throws Exception each time the method is called.
	 */
	public void throwsException() throws Exception
	{
		throw new MethodInvocationException(-1,"KO");
	}
	
	/**
	 * Sample echo method that return an empty result object.
	 * That is, an object with only status code / text valorized 
	 * (no output parameters).
	 * 
	 * @return an empty result object.
	 */
	public InvocationResult voidWithoutArguments()
	{
		return new InvocationResult(0,"OK,null",null);
	} 
	
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
	public InvocationResult echoWithSimpleTypeArrays(
			long [] longs,
			boolean [] booleans,
			double [] doubles,
			float [] floats,
			int [] integers,
			short [] shorts)
	{	
		InvocationResult result = new InvocationResult(0,"OK",null);
		Map<String, Object> outputParameters = new HashMap<String, Object>();
		outputParameters.put(long.class.getName(), longs);
		outputParameters.put(boolean.class.getName(), booleans);
		outputParameters.put(double.class.getName(), doubles);
		outputParameters.put(float.class.getName(), floats);
		outputParameters.put(int.class.getName(), integers);
		outputParameters.put(short.class.getName(), shorts);
		result.setOutputSection(outputParameters);
		return result;
	}
	
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
	public InvocationResult echoWithSimpleTypes(
			Long aLong,
			Boolean aBoolean,
			Double aDouble,
			Float aFloat,
			Integer anInteger,
			Short aShort,
			String aString,
			URI anURI,
			Date aDate)
	{
		InvocationResult result = new InvocationResult(0,"OK",null);
		Map<String, Object> outputParameters = new HashMap<String, Object>();
		outputParameters.put("p1", aLong);
		outputParameters.put("p2", aBoolean);
		outputParameters.put("p3", aDouble);
		outputParameters.put("p4", aFloat);
		outputParameters.put("p5", anInteger);
		outputParameters.put("p6", aShort);
		outputParameters.put("p7", aString);
		outputParameters.put("p8", anURI);
		outputParameters.put("p9", aDate);
		result.setOutputSection(outputParameters);
		return result;
	}

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
	public InvocationResult echoWithArrays(
			Long [] longs,
			Boolean [] booleans,
			Double [] doubles,
			Float [] floats,
			Integer [] integers,
			Short [] shorts,
			String [] strings,
			URI [] uris,
			Date [] dates)
	{
		InvocationResult result = new InvocationResult(0,"OK",null);
		Map<String, Object> outputParameters = new HashMap<String, Object>();
		outputParameters.put(Long.class.getName(), longs);
		outputParameters.put(Boolean.class.getName(), booleans);
		outputParameters.put(Double.class.getName(), doubles);
		outputParameters.put(Float.class.getName(), floats);
		outputParameters.put(Integer.class.getName(), integers);
		outputParameters.put(Short.class.getName(), shorts);
		outputParameters.put(String.class.getName(), strings);
		outputParameters.put(URI.class.getName(), uris);
		outputParameters.put(Date.class.getName(), dates);
		result.setOutputSection(outputParameters);
		return result;
	}
	
	/**
	 * Echo method that accepts and returns a byte array.
	 * 
	 * @param byteArray a byte array
	 * @return a result containing the input byte array (as output parameter)
	 */
	public InvocationResult echoWithByteArray(byte [] byteArray)
	{ 
		InvocationResult result = new InvocationResult(0,"OK",null);
		Map<String, Object> outputParameters = new HashMap<String, Object>();
		outputParameters.put(byte[].class.getName(),byteArray);
		result.setOutputSection(outputParameters);
		return result;
	}
	
	/**
	 * Echo method that accepts and returns an UUID.
	 * 
	 * @param uuid a java.util.UUID.
	 * @return a result containing the input UUID (as output parameter)
	 */
	public InvocationResult echoWithUUID(UUID uuid) 
	{ 
		InvocationResult result = new InvocationResult(0,"OK",null);
		Map<String, Object> outputParameters = new HashMap<String, Object>();
		outputParameters.put("uuid",uuid);
		result.setOutputSection(outputParameters);
		return result;
	}
	
	/**
	 * Echo method that accepts and returns a Map.
	 * 
	 * @param map a java.util.Map.
	 * @return a result containing the input Map (as output parameter)
	 */
	public InvocationResult echoWithMap(Map<String,Object> map) 
	{
		InvocationResult result = new InvocationResult(0,"OK",null);
		Map<String, Object> outputParameters = new HashMap<String, Object>();
		outputParameters.put("map",map);
		result.setOutputSection(outputParameters);
		return result;
	}

	public UUID getVhostRef()
	{
		return _vhostRef;
	}

	public String getName()
	{
		return _name;
	}

	public Boolean getDurable()
	{
		return _durable;
	}

	public Map<String, Object> getArguments()
	{
		return _arguments;
	}

	public Long getMsgTotalEnqueues()
	{
		return _msgTotalEnqueues;
	}

	public Integer getConsumerCount()
	{
		return _consumerCount;
	}

	public Date getExpireTime()
	{
		return _expireTime;
	}

	public Short getMgmtPubInterval()
	{
		return _mgmtPubInterval;
	}

	public void setExpireTime(Date expireTime)
	{
		this._expireTime = expireTime;
	}

	public void setMgmtPubInterval(Short value)
	{
		this._mgmtPubInterval = value;
	}
	
	public void setType(String type)
	{
		this._type = type;
	}
	
	public String getType()
	{
		return _type;
	}
	
	public byte[] getByteArray()
	{
		return _byteArray;
	}
}