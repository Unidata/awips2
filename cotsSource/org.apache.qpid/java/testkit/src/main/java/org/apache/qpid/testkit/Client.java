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
package org.apache.qpid.testkit;


import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;

import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.Session;

public abstract class Client
{
	protected Connection con;
	protected Session ssn;
    protected boolean durable = false;
    protected boolean transacted = false;
    protected int txSize = 10;
    protected int ack_mode = Session.AUTO_ACKNOWLEDGE;
    protected String contentType = "application/octet-stream";
    protected Destination dest = null;
        
    protected long reportFrequency = 60000;  // every min
    protected DateFormat df = new SimpleDateFormat("yyyy.MM.dd 'at' HH:mm:ss");
    protected NumberFormat nf = new DecimalFormat("##.00");

    protected long startTime = System.currentTimeMillis();
    protected ErrorHandler errorHandler = null;
    
    public Client(Connection con) throws Exception
    {
       this.con = con;       
       durable = Boolean.getBoolean("durable");
       transacted = Boolean.getBoolean("transacted");
       txSize = Integer.getInteger("tx_size",10);
       contentType = System.getProperty("content_type","application/octet-stream");    
       reportFrequency = Long.getLong("report_frequency", 60000);
    }

    public void close()
    {
    	try
    	{
    		con.close();
    	}
    	catch (Exception e)
    	{
    		handleError("Error closing connection",e);
    	}
    }
    
    public void setErrorHandler(ErrorHandler h)
    {
    	this.errorHandler = h;
    }
    
    public void handleError(String msg,Exception e)
    {
    	if (errorHandler != null)
    	{
    		errorHandler.handleError(msg, e);
    	}
    	else
    	{
    		System.err.println(msg);
    		e.printStackTrace();
    	}
    }
}
