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
package org.apache.qpid.management.wsdm.muse.resources;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.muse.core.Capability;
import org.apache.muse.core.Environment;
import org.apache.muse.core.ResourceManager;
import org.apache.muse.core.routing.MessageHandler;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.addressing.WsaConstants;
import org.apache.muse.ws.addressing.soap.SoapConstants;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.addressing.soap.SoapUtils;
import org.apache.muse.ws.resource.WsResource;
import org.apache.muse.ws.resource.metadata.MetadataDescriptor;
import org.apache.muse.ws.resource.metadata.WsrmdConstants;
import org.apache.muse.ws.resource.metadata.impl.SimpleMetadataDescriptor;
import org.apache.muse.ws.resource.metadata.impl.WsrmdUtils;
import org.apache.muse.ws.resource.properties.ResourcePropertyCollection;
import org.apache.muse.ws.resource.properties.impl.SimpleResourcePropertyCollection;
import org.apache.muse.ws.resource.properties.impl.WsrpUtils;
import org.apache.muse.ws.resource.properties.schema.ResourcePropertiesSchema;
import org.apache.muse.ws.resource.properties.schema.impl.SimpleResourcePropertiesSchema;
import org.apache.muse.ws.wsdl.WsdlUtils;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.wsdm.common.ThreadSessionManager;
import org.apache.qpid.transport.util.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * QMan WS resource.
 * We could say that this is a QMan manageable entity under the 
 * WS-DM perspective.
 * 
 * @author Andrea Gazzarini
 */
@SuppressWarnings("unchecked")
public class QManWsResource implements WsResource
{    
	private final static Logger LOGGER = Logger.get(QManWsResource.class);
		
	/**
	 * Internal state of this resource.
	 * 
	 * @author Andrea Gazzarini
	 */
	interface State 
	{
		/**
		 * Provides initialization of this resource.
		 * 
		 * @throws SoapFault when the initialization fails.
		 */
		void initialize() throws SoapFault;
		
		/**
		 * Returns true if this resource has been initialized.
		 * 
		 * @return true if this resource has been initialized.
		 */
		boolean hasBeenInitialized();
		
		/**
		 * Returns true if this resource has been shutdown.
		 * 
		 * @return true if this resource has been shutdown.
		 */
		boolean hasBeenShutdown();
		
		/**
		 * Shuts down this resource.
		 * 
		 * @throws SoapFault when the shutdown procedure fails.
		 */
		void shutdown() throws SoapFault;		
	}
	
	private final State _hasBeenShutdown = new State()
	{
		/**
		 * Return false because this resource has been shutdown so therefore 
		 * initialization occurred.
		 * 
		 * @return true;
		 */
		public boolean hasBeenInitialized()
		{
			return true;
		}

		/**
		 * Returns true because this state indicates that resource has been shutdown.
		 * 
		 * @return true.
		 */
		public boolean hasBeenShutdown()
		{
			return true;
		}

		/**
		 * Since this resource has been shutdown the initialization
		 * cannot be performed again.
		 * As conseguence of that this method throws an exception.
		 * 
		 * @throws SoapFault each time this method is called.
		 */
		public void initialize() throws SoapFault
		{
			LOGGER.error(Messages.QMAN_100031_WS_RESOURCE_ALREADY_INITIALIZED);
			throw new SoapFault(Messages.QMAN_100031_WS_RESOURCE_ALREADY_INITIALIZED);
		}

		public void shutdown() throws SoapFault
		{
			LOGGER.error(Messages.QMAN_100033_WS_RESOURCE_ALREADY_SHUTDOWN);
			throw new SoapFault(Messages.QMAN_100033_WS_RESOURCE_ALREADY_SHUTDOWN);			
		}
	};
	
	private final State _hasBeenInitialized = new State()
	{
		/**
		 * Returns true because this is the state where a resource is when it 
		 * has been initialized.
		 * 
		 * @return true.
		 */
		public boolean hasBeenInitialized()
		{
			return true;
		}

		/**
		 * Returns false because this resource has been initialized but no shutdown request
		 * has been received.
		 * 
		 * @return false.
		 */
		public boolean hasBeenShutdown()
		{
			return false;
		}

		/**
		 * A resource in this state cannot be initialized again so if this method is called an
		 * exception is thrown.
		 * 
		 * @throws SoapFault each time this method is called.
		 */
		public void initialize() throws SoapFault
		{
			LOGGER.error(Messages.QMAN_100031_WS_RESOURCE_ALREADY_INITIALIZED);
			throw new SoapFault(Messages.QMAN_100031_WS_RESOURCE_ALREADY_INITIALIZED);
		}
		
		/**
		 * Shuts down this resource.
		 * 
		 * @throws SoapFault when the shutdown procedure fails.
		 */
		public void shutdown() throws SoapFault 
		{
	        shutdownCapabilities();
            
	        ResourceManager manager = getResourceManager();
	        
	        if (manager.getResource(_enpointReference) != null)
	        {
	            manager.removeResource(_enpointReference);
	        }
	        
	        _currentState = _hasBeenShutdown;
		}		
	};
	
	/**
	 * The initial state of this resource.
	 * As the name suggests, it is not yet initialized.
	 */
	private final State _notYetInitialized = new State() 
	{
		/**
		 * Provides initialization of this resource.
		 * 
		 * @throws SoapFault when the initialization fails.
		 */
		public void initialize() throws SoapFault
		{
	        _properties = new SimpleResourcePropertyCollection();
	        _wsdl = ThreadSessionManager.getInstance().getSession().getWsdlDocument();
	        
	        ResourcePropertiesSchema schema = createPropertiesSchema(_wsdl);
	        _properties.setSchema(schema);
	      
	        MetadataDescriptor metadata = createMetadataDescriptor(_wsdl);
	        _properties.setMetadata(metadata);
	                
	        initializeCapabilities();
	        	        
	        _properties.applyMetadata();
	        
	        // Resource intialization completed : Let's make a state change.
	        _currentState = _hasBeenInitialized;
		}

		/**
		 * Shuts down this resource.
		 * 
		 * @throws SoapFault when the shutdown procedure fails.		 */
		public void shutdown() throws SoapFault
		{
			LOGGER.error(Messages.QMAN_100032_WS_RESOURCE_NOT_YET_INITIALIZED);
			throw new SoapFault(Messages.QMAN_100032_WS_RESOURCE_NOT_YET_INITIALIZED);			
		}
		
		/**
		 * Returns false because this state indicates that 
		 * the resource has not yet been initialized.
		 * 
		 * @return false;
		 */
		public boolean hasBeenInitialized()
		{
			return false;
		}

		/**
		 * Returns false because the resource, when is in this state 
		 * hasn't been initialized and as conseguence of that hasn't 
		 * been shutdonm.
		 * 
		 * @return false;
		 */
		public boolean hasBeenShutdown()
		{
			return false;
		}
	};
	
    private Map<String,Capability>  _capabilitiesByAction = new HashMap<String, Capability>();
    private Map<String, Capability> _capabilitiesByURI = new LinkedHashMap<String, Capability>();
    
    private String _contextPath;
    private Environment _environment;    
    private EndpointReference _enpointReference;
    
    private State _currentState = _notYetInitialized;
    
    private ResourceManager _resourceManager;
    private ResourcePropertyCollection _properties;

    private Map<String,String> _initParameters = Collections.EMPTY_MAP;
    
    // Workaround : muse is using and hardcoded java.util.logging.Logger but we should use 
    // SLF4j so this is the original implementatation that won't never be used (on QMan classes)
    private java.util.logging.Logger _logger;
    
    private Document _wsdl;    
    private String _wsdlPath;
    private QName _wsdlPortType;
    
    /**
     * Adds the given capability to this resource.
     * 
     * @param capability the capability to be added.
     */
    public void addCapability(Capability capability)
    {
        capability.setResource(this);
        capability.setLog(getLog());
        capability.setEnvironment(getEnvironment());
        
        String uri = capability.getCapabilityURI();
        _capabilitiesByURI.put(uri, capability);
        
        LOGGER.debug(
        		Messages.QMAN_200033_CAPABILITY_CLASS_HAS_BEEN_ADDED, 
        		capability.getClass(),
        		uri);
    }
    
    /**
     * Returns the capability associated with the given URI.
     * 
     * @return the capability associated with the given URI.
     */
    public Capability getCapability(String capabilityURI)
    {
        return _capabilitiesByURI.get(capabilityURI);
    }
    
    /**
     * Returns a collection with all registered capability URIs.
     * 
     * @return a collection with all registered capability URIs.
     */
    public final Collection getCapabilityURIs()
    {
        return Collections.unmodifiableSet(_capabilitiesByURI.keySet());
    }
    
    /**
     * Returns the context path of this resource.
     * 
     * @return the context path of this resource.
     */
    public final String getContextPath()
    {
        return _contextPath;
    }

    /**
     * Returns the endpoint reference of this resource.
     * 
     * @return the endpoint reference of this resource.
     */
    public EndpointReference getEndpointReference()
    {
        return _enpointReference;
    }
    
    /**
     * Returns the enviroment associated with this resource.
     * 
     * @return the enviroment associated with this resource.
     */
    public final Environment getEnvironment()
    {
        return _environment;
    }
    
    /**
     * Returns the initialization parameter of this resource associated with 
     * the given name.
     * 
     * @param name the init parameter name.
     * @return the initialization parameter associated with the given name.
     */
    public final String getInitializationParameter(String name)
    {
        return (String)getInitializationParameters().get(name);
    }
    
    /**
     * Returns the map containing all init parameters of this resource.
     * 
     * @return the map containing all init parameters of this resource.
     */
    public final Map<String,String> getInitializationParameters()
    {
        return _initParameters;
    }
    
    /**
     * N.A. This resource uses QMan logging instead of plain java.util.logger 
     * implementation.
     */
    public final java.util.logging.Logger getLog()
    {
        return _logger;
    }
    
    /**
     * Returns the resource manager associated with this resource.
     * 
     * @return the resource manager associated with this resource.
     */
    public ResourceManager getResourceManager()
    {
        return _resourceManager;
    }
    
    /**
     * Returns the wsdl (relative) path of this resource.
     * 
     * @return the wsdl (relative) path of this resource.
     */
    public String getWsdlPath()
    {
        return _wsdlPath;
    }
    
    /**
     * Returns the port type of this resource.
     * 
     * @return the port type of this resource.
     */
    public final QName getWsdlPortType()
    {
        return _wsdlPortType;
    }
    
    /**
     * Returns true if this resource has been initialized, false otherwise.
     * 
     * @return true if this resource has been initialized, false otherwise.
     */
    public final boolean hasBeenInitialized()
    {
        return _currentState.hasBeenInitialized();
    }
    
    /**
     * Returns true if this resource has been shutdown, false otherwise.
     * 
     * @return true if this resource has been shutdown, false otherwise.
     */
    public final boolean hasBeenShutdown()
    {
        return _currentState.hasBeenShutdown();
    }
    
    /**
     * Checks if a capability with the given URI is available for this resource.
     * 
     * @return true if a capability with the given URI is available for this resource, false otherwise.
     */
    public final boolean hasCapability(String capabilityURI)
    {
        return getCapability(capabilityURI) != null;
    }
    
    /**
     * Returns the collection containing all properties of this resource.
     * 
     * @return the collection containing all properties of this resource.
     */
    public final ResourcePropertyCollection getPropertyCollection()
    {
        return _properties;
    }    
    
    /**
     * Return the WSDL document of this resource.
     * 
     * @return the WSDL document of this resource.
     */
    public Document getWsdl()
    {
    	return _wsdl;
    }
    
    /**
     * Initializes this resources.
     * Note that the what needs to be done depends on the current state of this
     * resource.
     * 
     * @throws SoapFault when the initialization fails.
     */
    public void initialize() throws SoapFault
    {    	
    	_currentState.initialize();
    }
        
    /**
     * Invokes the action specified in the given soap request on this resource.
     * 
     * @param requestBody the SOAP body.
     * @return the result of the invocation as org.w3c.dom.Element
     */
    public Element invoke(Element requestBody)
    {
        String action =  _environment.getAddressingContext().getAction();  
        Capability capability = getCapabilityForAction(action);
        
        // Sanity check : is there a capability for the given action?
        if (capability == null)
        {
            SoapFault wsaFault = new SoapFault(
            		String.format(
            				Messages.ACTION_NOT_SUPPORTED, 
            				action,getContextPath()));

            wsaFault.setCode(SoapConstants.SENDER_QNAME);
            wsaFault.setSubCode(WsaConstants.ACTION_NOT_SUPPORTED_FAULT_QNAME);
            
            Element detail = XmlUtils.createElement(WsaConstants.PROBLEM_ACTION_QNAME);
            XmlUtils.setElement(detail, WsaConstants.ACTION_QNAME, action);
            wsaFault.setDetail(detail);
            
            LOGGER.error(
            		Messages.QMAN_100020_ACTION_NOT_SUPPORTED, 
            		action,
            		getContextPath());

            return wsaFault.toXML();
        }
        
        MessageHandler handler = capability.getMessageHandler(action);
        Method method = handler.getMethod();
        
        try
        {
        	Object[]parameters = handler.fromXML(requestBody);
            Object result = method.invoke(capability, parameters);
            return handler.toXML(result); 
        }
        catch (Throwable throwable)
        {
        	LOGGER.error(
        			throwable, 
        			Messages.QMAN_100037_INVOKE_OPERATION_FAILURE);
        	
            SoapFault response = SoapUtils.convertToFault(
            		(throwable.getCause()!= null) 
            			? throwable.getCause()
            			: throwable);
            return response.toXML();
        }
    }
    
    /**
     * Sets the context path of this resource.
     * 
     * @param contextPath the context path of this resource.
     */
    public final void setContextPath(String contextPath)
    {
        _contextPath = contextPath;
    }
    
    /**
     * Sets the endpoint reference of this resource.
     * 
     * @param endpointReference the endpoint reference of this resource.
     */
    public final void setEndpointReference(EndpointReference endpointReference)
    {
        if (_enpointReference != null && hasBeenInitialized())
            throw new RuntimeException(("ExistingResourceEPR"));
        
        _enpointReference = endpointReference;
    }
    
    /**
     * Sets the context environment of this resource.
     * 
     * @param environment the context environment of this resource.
     */
    public final void setEnvironment(Environment environment)
    {
        _environment = environment;
    }
    
    /**
     * Sets the initialization parameters of this resource.
     * 
     * @param parameters the init parameters of this resource.
     */
    public final void setInitializationParameters(Map parameters)
    {
        _initParameters = (parameters != null) 
        	? parameters 
        	: Collections.EMPTY_MAP;
    }
    
    /**
     * N.A. for this resource. QMan logging mechanism is used for that.
     */
    public final void setLog(java.util.logging.Logger log)
    {
        _logger = log;
    }
    
    /**
     * Sets the resource manager owner of this resource.
     * 
     * @param manager the resource manager of this resource.
     */
    public void setResourceManager(ResourceManager manager)
    {
        _resourceManager = manager;
    }
    
    /**
     * Sets the WSDL (relative) path of this resource.
     * 
     * @param wsdlPath the WSDL (relative) path of this resource.
     */
    public final void setWsdlPath(String wsdlPath)
    {
    	this._wsdlPath = wsdlPath;
    }
    
    /**
     * Sets the port type of this resource.
     * 
     * @param wsdlPortType the port type of this resource.
     */
    public final void setWsdlPortType(QName wsdlPortType)
    {
        _wsdlPortType = wsdlPortType;
    }
        
    /**
     * Shutdown procedure for this resource.
     * 
     * @throws SoapFault when the shutdown procedure fails.
     */
    public synchronized void shutdown() throws SoapFault
    {
    	_currentState.shutdown();
    }
        
    /**
     * Returns a string representation of this resource.
     * Basically the resource endpoint reference (as a string) is returned.
     * 
     * @return the resource endpoint reference (as a string) is returned.
     */
    public String toString()
    {
        return getEndpointReference().toString();
    }     
    
    /**
     * Initializes capabilities of this resource.
     * 
     * @throws SoapFault when at least one capability fails to initialize.
     */
    private void initializeCapabilities() throws SoapFault
    {
        for (Entry<String, Capability> entry : _capabilitiesByURI.entrySet()) 
        {
        	Capability capability = entry.getValue();
			capability.initialize();
			
			for (Object action : capability.getActions()) 
			{
                _capabilitiesByAction.put((String)action, capability);
			}
			
			capability.initializeCompleted();
		}
    }
    
    /**
     * Shutdown procedure for all registered capabilities of this resource.
     * 
     * @throws SoapFault when at least one capability shutdown fails.
     */
    private void shutdownCapabilities() throws SoapFault
    {
        for (Entry<String,Capability> entry : _capabilitiesByURI.entrySet()) 
        {
        	Capability capabilty = entry.getValue();
        	capabilty.prepareShutdown();
        	capabilty.shutdown();
		}        
    }    
    
    /**
     * Creates a metadata descriptor for this resource.
     * 
     * @param wsdl the WSDL document. 
     * @return a metadata descriptor for this resource.
     * @throws SoapFault when it's not possible build the descriptor.
     */
    private MetadataDescriptor createMetadataDescriptor(Document wsdl) throws SoapFault
    {
        try 
        {    	
	        Element portTypeXML = WsdlUtils.getPortType(wsdl, getWsdlPortType());
	        
	        String rmdName = XmlUtils.getAttribute(
	        		portTypeXML, 
	        		WsrmdConstants.DESCRIPTOR_ATTR_QNAME);
	        
	        String rmdPath = XmlUtils.getAttribute(
	        		portTypeXML, 
	        		WsrmdConstants.DESCRIPTOR_LOCATION_ATTR_QNAME);
	        
	        LOGGER.debug(Messages.QMAN_200034_RMD_NAME, rmdName);
	        LOGGER.debug(Messages.QMAN_200035_RMD_PATH, rmdPath);
	        
	        Environment env = getEnvironment();
	        String path = env.createRelativePath(getWsdlPath(), rmdPath);        
	        Document rmdDoc = env.getDocument(path);
	        
	        Element[] additionalProperties = 
	        	ThreadSessionManager
	        		.getInstance()
	        		.getSession()
	        		.getResourceMetadataDescriptor();
	        
	        Element metadataDescriptor = WsrmdUtils.getMetadataDescriptor(rmdDoc, rmdName);
        
	        for (Element element : additionalProperties) 
	        {
	        	
//	        	rmdDoc.importNode(element, true);
				Element adopted = (Element) rmdDoc.importNode(element,false);
				metadataDescriptor.appendChild(adopted);
			}
			
			return new SimpleMetadataDescriptor(metadataDescriptor);
        } 
        catch(Exception exception)
        {
        	LOGGER.error(
        			exception,
        			Messages.QMAN_100021_RMD_BUID_FAILURE,
        			getContextPath());
        	throw new SoapFault(exception);
        }
    }    
    
    /**
     * Returns the capability associated with the given action.
     * 
     * @param action the wsa:action of the requested capability.
     * @return the capability associated with the given action.
     */
    private Capability getCapabilityForAction(String action)
    {
        return (Capability)_capabilitiesByAction.get(action);
    }    
    
    /**
     * Creates a WSRP document representing schema properties for this resource.
     * 
     * @param wsdl the DOM document holding the resource's WSDL.       
     * @return the WSRP document schema.
     */
    private ResourcePropertiesSchema createPropertiesSchema(Document wsdl)
    {
        QName wsrpName = WsrpUtils.getPropertiesName(wsdl, getWsdlPortType());
        Element wsrpDoc = WsdlUtils.getElementDeclaration(wsdl, wsrpName);        
        return new SimpleResourcePropertiesSchema(wsrpName, wsrpDoc);
    }        
}