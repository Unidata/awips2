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
package org.apache.qpid.management.web.action;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * Console Model.
 * It is a simple Data Transfer Object encapsulating all information about QMan 
 * console (System Overview)
 * 
 * @author Andrea Gazzarini
 */
public class ConsoleModel implements Serializable
{
	private static final long serialVersionUID = -7676132151242738376L;
	private String _version;
	private String _versionName;
	private Date _startDate;
	private String _host;
	private int _port;
	
	private String _osName;
	private String _osVersion;
	private String _archName;
	private Integer _processors;

	private String [] _bootClasspath;
	private String [] _classpath;
	private String [] _inputArguments;
	private String [] _systemProperties;
	
	public String getVersion()
	{
		return _version;
	}
	public void setVersion(String version)
	{
		this._version = version;
	}
	public String getVersionName()
	{
		return _versionName;
	}
	public void setVersionName(String versionName)
	{
		this._versionName = versionName;
	}
	public Date getStartDate()
	{
		return _startDate;
	}
	public void setStartDate(Date startDate)
	{
		this._startDate = startDate;
	}
	public String getHost()
	{
		return _host;
	}
	public void setHost(String host)
	{
		this._host = host;
	}
	public int getPort()
	{
		return _port;
	}
	public void setPort(int  port)
	{
		this._port = port;
	}
	public String getOsName()
	{
		return _osName;
	}
	public void setOsName(String osName)
	{
		this._osName = osName;
	}
	public String getOsVersion()
	{
		return _osVersion;
	}
	public void setOsVersion(String osVersion)
	{
		this._osVersion = osVersion;
	}
	public String getArchName()
	{
		return _archName;
	}
	public void setArchName(String archName)
	{
		this._archName = archName;
	}
	public Integer getProcessors()
	{
		return _processors;
	}
	public void setProcessors(Integer processors)
	{
		this._processors = processors;
	}
	public List<String> getBootClasspath()
	{	
		return Arrays.asList(_bootClasspath);
	}
	public void setBootClasspath(String [] bootClasspath)
	{
		this._bootClasspath = bootClasspath;
	}
	public String [] getClasspath()
	{
		return _classpath;
	}
	public void setClasspath(String [] classpath)
	{
		this._classpath = classpath;
	}
	public String [] getInputArguments()
	{
		return _inputArguments;
	}
	public void setInputArguments(String [] inputArguments)
	{
		this._inputArguments = inputArguments;
	}
	public String [] getSystemProperties()
	{
		return _systemProperties;
	}
	public void setSystemProperties(String [] systemProperties)
	{
		this._systemProperties = systemProperties;
	}
}