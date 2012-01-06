package org.apache.qpid.management.wsdm.muse.engine;
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


import java.io.File;
import java.net.URI;

import javax.servlet.ServletContext;

import org.apache.muse.core.AbstractEnvironment;
import org.apache.muse.util.FileUtils;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.transport.util.Logger;

/**
 * QMan Adapter enviroment implementation.
 * 
 * @author Andrea Gazzarini
 */
public class WSDMAdapterEnvironment  extends AbstractEnvironment
{
	private final static Logger LOGGER = Logger.get(WSDMAdapterEnvironment.class);
	private final File _realDirectory;
    private final ServletContext _servletContext;
	
    /**
     * Builds a new qman environment with the given application context.
     *  
     * @param servletContext the application context. 
     */
    public WSDMAdapterEnvironment(ServletContext servletContext)
    {
    	this._servletContext = servletContext;
    	String realDirectoryPath = servletContext.getRealPath(Names.WEB_APP_CLASSES_FOLDER);
        
        _realDirectory = (realDirectoryPath != null) 
        	? new File(realDirectoryPath) 
        	: FileUtils.CURRENT_DIR;
        	
        String defaultURI = getDefaultURIPrefix()+"adapter";
        setDefaultURI(defaultURI);
        
        LOGGER.info(Messages.QMAN_000029_DEFAULT_URI, defaultURI);
    }
    
    /**
     * Returns the endpoint created starting by this application default URI.
     * 
     * @return the endpoint created starting by this application default URI.
     */
    public EndpointReference getDeploymentEPR()
    {
        return new EndpointReference(URI.create(getDefaultURI()));
    }

    /**
     * Returns the application classes folder.
     * 
     * @return the application classes folder.
     */
    public File getRealDirectory()
    {
        return _realDirectory;
    }
    
    /**
     * Returns the default endpoint reference URI.
     * 
     * @return the default endpoint reference URI.
     */
    public String getDefaultURIPrefix()
    {
        return new StringBuilder()
    		.append("http://")
    		.append(System.getProperty(
    				Names.ADAPTER_HOST_PROPERTY_NAME,
    				Protocol.DEFAULT_QMAN_HOSTNAME))
    		.append(":")
    		.append(System.getProperty(
    				Names.ADAPTER_PORT_PROPERTY_NAME,
    				String.valueOf(Protocol.DEFAULT_QMAN_PORT_NUMBER)))
    		.append(_servletContext.getContextPath())
    		.append("/services/")
    		.toString();    	
    }
    
    /**
     * Returns the context path name of QMan application.
     * 
     * @return the context path name of QMan application.
     */
    public String getContextPath()
    {
    	return _servletContext.getContextPath();
    }
}
