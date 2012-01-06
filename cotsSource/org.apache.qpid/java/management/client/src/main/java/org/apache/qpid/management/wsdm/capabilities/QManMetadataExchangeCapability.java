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
package org.apache.qpid.management.wsdm.capabilities;

import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.metadata.WsxConstants;
import org.apache.muse.ws.resource.WsResource;
import org.apache.muse.ws.resource.metadata.MetadataDescriptor;
import org.apache.muse.ws.resource.metadata.WsrmdConstants;
import org.apache.muse.ws.resource.metadata.ext.WsrfMetadataExchange;
import org.apache.muse.ws.wsdl.WsdlUtils;
import org.apache.qpid.management.wsdm.muse.resources.QManWsResource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * QMan resource metadata exchange.
 * We cannot resuse the preexisting classes directly because the wsdl of the service instance
 * is retrieved using a file path.
 * Since the owner resource (QManWsResource) is dynamic (I mean, its interface is dynamic), the corresponding
 * WSDL cannot defined at compile time but needs some changes when the resource is created.
 * As part of that, the WSDL template found under wsdl folder is modified with the additional properties of the given 
 * resource. The metadata exchange capability must include those properties too.
 * 
 * Note that this capability is appliable only to a QManWsResource.
 * 
 * @author Andrea Gazzarini
 */
public class QManMetadataExchangeCapability extends WsrfMetadataExchange 
{		
	/**
	 * Returns the WSDL associated with the owner of this capability.
	 * 
	 *  @return the WSDL associated with the owner of this capability.
	 */
	@Override
	protected Element getWSDL() 
	{
		QManWsResource resource = (QManWsResource) getResource();
        
        Document wsdlDoc = resource.getWsdl();
        Element wsdl = XmlUtils.getFirstElement(wsdlDoc);
        
        WsdlUtils.removeWsdlReferences(wsdl);
        WsdlUtils.removeSchemaReferences(wsdl);
        
        return wsdl;
	}
	
	/**
	 * Returns the resource metadata descriptor associated with the owenr 
	 * resource of thi capability.
	 * 
	 * @return the resource metadata descriptor. 
	 */
	protected Element getResourceMetadataDescriptor()
	{
        WsResource resource = (WsResource)getResource();
        MetadataDescriptor metadataDescriptor = resource.getPropertyCollection().getMetadata();
        return metadataDescriptor.toXML();		
	}
	
    public Element[] getMetadata(String dialect)
    {  
    	if (dialect == null)
    	{
    		return new Element[]{
    				getResourceMetadataDescriptor(),
    				getWSDL()};
    	} else {
    		if (WsrmdConstants.NAMESPACE_URI.equals(dialect))
    		{
    			return new Element[]{getResourceMetadataDescriptor()};
    		} else if (WsxConstants.WSDL_DIALECT.equals(dialect))
    		{
    			return new Element[]{getWSDL()};
    		}
    	}
    	return super.getMetadata(dialect);
    }
}