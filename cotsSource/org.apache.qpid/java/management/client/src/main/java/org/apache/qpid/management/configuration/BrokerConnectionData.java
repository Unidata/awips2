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
 * Value object which is holding connection data for a specific broker.
 * 
 * @author Andrea Gazzarini
 */
public class BrokerConnectionData
{
    private String _host;
    private int _port;
    private String _virtualHost;
    private String _username;
    private String _password;
    private int _maxPoolCapacity;
    private int _initialPoolCapacity;
    private long _maxWaitTimeout;
    
    /**
     * Builds a connection data with the given parameters.
     * 
 	 * @param host the hostname where the broker is running.
	 * @param port the port where the broker is running.
	 * @param username the username for connecting with the broker.
	 * @param password the password for connecting with the broker.
	 * @param virtualHost the virtual host.
	 * @param initialPoolCapacity the number of connections that must  be immediately opened.
	 * @param maxPoolCapacity the maximum number of opened connection.
	 * @param maxWaitTimeout the maximum amount of time that a client will wait for obtaining a connection.
     */
    public BrokerConnectionData(
    		String host, 
    		int port, 
    		String virtualHost,
			String username, 
			String password, 
			int initialPoolCapacity,
			int maxPoolCapacity, 
			long waitTimeout) {

    	this._host = host;
		this._port = port;
		this._virtualHost = virtualHost;
		this._username = username;
		this._password = password;
		_maxPoolCapacity = maxPoolCapacity;
		_initialPoolCapacity = initialPoolCapacity;
		_maxWaitTimeout = waitTimeout;
	}

	/**
     * Builds a new empty broker connection data object.
     */
    BrokerConnectionData()
    {
    }
    
    /**
     * Sets the value of host property for this connection data.
     * 
     * @param host the host name.
     */
    void setHost (String host)
    {
        this._host = host;
    }

    /**
     * Sets the value of port property for this connection data.
     * 
     * @param port the port.
     */
    void setPort (String port)
    {
        this._port = Integer.parseInt(port);
    }

    /**
     * Sets the value of virtual host property for this connection data.
     * 
     * @param virtualHost the virtual host.
     */
    void setVirtualHost (String virtualHost)
    {
        this._virtualHost = virtualHost;
    }
    
    /**
     * Sets the value of username property for this connection data.
     * 
     * @param username the username.
     */
    void setUsername(String username)
    {
        this._username = username;
    }
    
    /**
     * Sets the value of password property for this connection data.
     * 
     * @param password the password.
     */
    void setPassword(String password) 
    {
        this._password = password;
    }

    /**
     * Returns the value of the host property.
     * 
     * @return the value of the host property.
     */
    public String getHost ()
    {
        return _host;
    }

    /**
     * Returns the value of the port property.
     * 
     * @return the value of the port  property.
     */
    public int getPort ()
    {
        return _port;
    }

    /**
     * Returns the value of the virtual host property.
     * 
     * @return the value of the virtual host property.
     */
    public String getVirtualHost ()
    {
        return _virtualHost;
    }

    /**
     * Returns the value of the username property.
     * 
     * @return the value of the username property.
     */
    public String getUsername ()
    {
        return _username;
    }

    /**
     * Returns the value of the password property.
     * 
     * @return the value of the password property.
     */
    public String getPassword ()
    {
        return _password;
    }
    
    // sofia:5663@pippo/sung1
    @Override
    public String toString ()
    {
        return new StringBuilder()
            .append(_host)
            .append(':')
            .append(_port)
            .append('@')
            .append(_virtualHost)
            .toString();
    }

    /**
     * Sets the max number of allowed connections that can be opened. 
     * 
     * @param value the max number of allowed connections that can be opened.
     * @throws NumberFormatException if the given value is not a valid integer.
     */
    public void setMaxPoolCapacity (String value)
    {
        _maxPoolCapacity = Integer.parseInt(value);
    }
    
    /**
     * Sets the max wait timeout for retrieving an available connections from the pool. 
     * 
     * @param value the max wait timeout for retrieving an available connections from the pool..
     * @throws NumberFormatException if the given value is not a valid long.
     */
    public void setMaxWaitTimeout (String value)
    {
        this._maxWaitTimeout = Long.parseLong(value);
    }
    
    /**
     * Returns the max number of allowed connections that can be opened.
     * 
     * @return the max number of allowed connections that can be opened.
     */
    public int getMaxPoolCapacity ()
    {
        return _maxPoolCapacity;
    }
    
    /**
     * Returns the max wait timeout for retrieving an available connections from the pool.
     * 
     * @return the max wait timeout for retrieving an available connections from the pool.
     */
    public long getMaxWaitTimeout () 
    {
        return _maxWaitTimeout;
    }

    /**
     * Sets the initial connection pool capacity.
     * 
     * @param capacity the initial connection pool capacity.
     */
    public void setInitialPoolCapacity (String capacity)
    {
        _initialPoolCapacity = Integer.parseInt(capacity);
    }
    
    /**
     * Returns the initial connection pool capacity.
     * 
     * @return the initial connection pool capacity.
     */
    public int getInitialPoolCapacity ()
    {
        return _initialPoolCapacity;
    }
    
    @Override
    public boolean equals(Object object) {
    	try 
    	{
			BrokerConnectionData connectionData = (BrokerConnectionData) object;
			return (_host.equals(connectionData._host) )
					&& (_port == connectionData._port)
					&& (_virtualHost.equals(connectionData._virtualHost));
		} catch (Exception exception) {
			return false;
		}
    }
    
    @Override
    public int hashCode() {
    	return _host.hashCode()+_port+_virtualHost.hashCode();
    }
}
