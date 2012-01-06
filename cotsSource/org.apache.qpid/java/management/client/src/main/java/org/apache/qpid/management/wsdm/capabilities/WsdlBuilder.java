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

import java.net.InetAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.ObjectName;
import javax.xml.transform.TransformerException;

import org.apache.muse.core.Environment;
import org.apache.muse.core.serializer.SerializerRegistry;
import org.apache.muse.util.ReflectUtils;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.wsdl.WsdlUtils;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.wsdm.muse.engine.WSDMAdapterEnvironment;
import org.apache.qpid.management.wsdm.muse.serializer.ObjectSerializer;
import org.apache.qpid.qman.debug.WsdlDebugger;
import org.apache.qpid.transport.util.Logger;
import org.apache.xpath.XPathAPI;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
/**
 * TO BE IMPROVED USING JAXB!!
 * 
 * @author Andrea Gazzarini
 */
class WsdlBuilder implements IArtifactBuilder,Constants {

	private final static Logger LOGGER = Logger.get(WsdlBuilder.class);
	
	private WSDMAdapterEnvironment _environment;
	private Document _document;
	private Element schema;
	private Element _wsrpProperties;
	private ObjectSerializer _serializer;
	private ObjectName _objectName;
	private Map<String, String> arrayTypesAlreadyDeclared = new HashMap<String, String>();
	
	private Element _arrayComplexType;
	private Element _nestedArrayType;
	
	/**
	 * For each attibute the corresponding xml type definition must be inserted on the QMan
	 * schema related section.
	 * After that, a reference to that definition must be declared on the wsrp element .
	 * 
	 * @param attributeMetadata the attribute metadata.
	 * @throws BuilderException only if this builder wasn't able to get a reference (via XPath) 
	 * 				to QMan schema section.
	 */
	public void onAttribute(MBeanAttributeInfo attributeMetadata) throws BuilderException  
	{
		try 
		{
			String attributeName = attributeMetadata.getName();
			schema.appendChild(defineSchemaFor(attributeMetadata.getType(), attributeName));				

			Element propertyRef= XmlUtils.createElement(_document, XSD_ELEMENT_QNAME);
			propertyRef.setAttribute(MIN_OCCURS, "0");		
			propertyRef.setAttribute(
					REF_ATTRIBUTE, 
					Names.PREFIX+":"+attributeName);

			_wsrpProperties.appendChild(propertyRef);
		} catch(Exception exception)
		{
			throw new BuilderException(exception);
		}
	}
	
	@SuppressWarnings("unchecked")
	private Element defineSchemaFor(String type, String attributeName) throws Exception
	{
		Element propertyDeclaration = XmlUtils.createElement(_document, XSD_ELEMENT_QNAME);
		String xmlType = null;
		if (type.equals(Map.class.getName())) 
		{
			xmlType="qman:map";
		} else if (UUID.class.getName().equals(type)) 
		{			
			xmlType = "qman:uuid";
		} else if (type.startsWith("["))
		{
			Class arrayClass =  Class.forName(type);
			Class clazz = ReflectUtils.getClassFromArrayClass(arrayClass);
			String arrayType = arrayClass.getSimpleName().replace("[]", "").trim();
			arrayType = Character.toUpperCase(arrayType.charAt(0))+arrayType.substring(1);
			if (!arrayTypesAlreadyDeclared.containsKey(type))
			{
				_arrayComplexType.setAttribute(NAME_ATTRIBUTE, "arrayOf"+arrayType);
				_nestedArrayType.setAttribute(TYPE_ATTRIBUTE, _serializer.getXmlType(clazz));
				schema.appendChild(_arrayComplexType);
				arrayTypesAlreadyDeclared.put(type, arrayType);
			}
			xmlType = "qman:arrayOf"+arrayTypesAlreadyDeclared.get(type);
		}
		else 
		{			
			xmlType = _serializer.getXmlType(Class.forName(type));
		}			
		propertyDeclaration.setAttribute(NAME_ATTRIBUTE,attributeName);
		propertyDeclaration.setAttribute(TYPE_ATTRIBUTE, xmlType);
		return propertyDeclaration;
	}

	/**
	 * Initializes this builder.
	 * 
	 * @param objectName the name of the current JMX entity.
	 * @throws BuilderException when it's not possible to proceed with the initialization.
	 */
	public void begin(ObjectName objectName) throws BuilderException
	{
		this._objectName = objectName;
		this._serializer = (ObjectSerializer) SerializerRegistry.getInstance().getSerializer(Object.class);
		
		createWsrpPropertiesElement();
		
		createReusableArrayComplextType();
		
		replaceDummyServiceLocationOnWsdl();
		
		createSchemaElement();
	}
	
	public void onOperation(MBeanOperationInfo operationMetadata) throws BuilderException
	{
		// SCHEMA SECTION
		/*
			<xs:element name='purgeRequest' type='qman:purgeRequest' />
			
			<xs:element name='purgeResponse' type='qman:purgeResponse' />
			
			<xs:complexType name='purgeRequest'>
				<xs:sequence>
					<xs:element name='arg0' type='xs:int' />
					<xs:element minOccurs='0' name='arg1' type='qman:hashMap' />
				</xs:sequence>
			</xs:complexType>
			
			<xs:element name='hashMap'>
	        	<xs:complexType>
	         		<xs:sequence>
	          			<xs:element maxOccurs='unbounded' minOccurs='0' name='entry'>
	           				<xs:complexType>
	            				<xs:sequence>
	             					<xs:element minOccurs='0' name='key' type='xs:string'/>
	             					<xs:element minOccurs='0' name='value' type='xs:anyType'/>
	            				</xs:sequence>
	           				</xs:complexType>
	          			</xs:element>
	         		</xs:sequence>
	        	</xs:complexType>
	       </xs:element>
			
			<xs:complexType name='purgeResponse'>
				<xs:sequence />
			</xs:complexType>
		 */
		
		try 
		{
			// <xsd:element xmlns="" name="purgeRequest" type="qman:purgeRequest"/>
			
			Element methodRequestElement= _document.createElement("xsd:element");		
			String methodNameRequest = operationMetadata.getName()+"Request";
			methodRequestElement.setAttribute("name", methodNameRequest);
			methodRequestElement.setAttribute("type", "qman:"+methodNameRequest);
			
			// <xs:element name='purgeResponse' type='qman:purgeResponse' />
//			Element methodResponseElement= _document.createElement("xsd:element");		
			String methodNameResponse= operationMetadata.getName()+"Response";
//			methodResponseElement.setAttribute("name", methodNameResponse);
//			methodResponseElement.setAttribute("type", "qman:result");//+methodNameResponse);
			
			schema.appendChild(methodRequestElement);
//			schema.appendChild(methodResponseElement);
	
			/*
				<xs:complexType name='purgeRequest'>
					<xs:sequence>
						<xs:element name='arg0' type='xs:int' />
						<xs:element minOccurs='0' name='arg1' type='qman:hashMap' />
					</xs:sequence>
				</xs:complexType>
	
			 */
			
			Element methodNameRequestComplexType =  _document.createElement("xsd:complexType");
			methodNameRequestComplexType.setAttribute("name", methodNameRequest);
			Element methodNameRequestComplexTypeSequence = _document.createElement("xsd:sequence");
			
			for(MBeanParameterInfo parameter : operationMetadata.getSignature())
			{
				methodNameRequestComplexTypeSequence.appendChild(defineSchemaFor(parameter.getType(), parameter.getName()));
			}
						
			methodNameRequestComplexType.appendChild(methodNameRequestComplexTypeSequence);
			schema.appendChild(methodNameRequestComplexType);
			
			/*
		<message name="purgeResponseMessage">
			<part element='qman:purgeResponse' name='purgeResponse'></part>
		</message>
		
		<message name='purgeRequestMessage'>
			<part element="qman:purgeRequest" name='purgeRequest'></part>
		</message>
			 */	
			Element definitions = (Element) XPathAPI.selectSingleNode(_document, "/wsdl:definitions");
			
			String requestMessageName = methodNameRequest+"Message";
			String responseMessageName = methodNameResponse+"Message";
			
			Element requestMessage = _document.createElement("message");
			requestMessage.setAttribute("name", requestMessageName);
			Element requestPart = _document.createElement("wsdl:part");
			requestPart.setAttribute("element", "qman:"+methodNameRequest);
			requestPart.setAttribute("name", methodNameRequest);
			requestMessage.appendChild(requestPart);
			
			Element responseMessage = _document.createElement("wsdl:message");
			responseMessage.setAttribute("name", responseMessageName);
			Element responsePart = _document.createElement("wsdl:part");
			responsePart.setAttribute("element", "qman:result");//+methodNameResponse);
			responsePart.setAttribute("name", methodNameResponse);
			responseMessage.appendChild(responsePart);
			
			definitions.appendChild(requestMessage);
			definitions.appendChild(responseMessage);
			
			
			/*
	<operation name='purge'>
			<input message="qman:purgeRequestMessage">
			</input>
			<output message='qman:purgeResponseMessage'>
			</output>
		</operation>		 
			 */
			Element portType = (Element) XPathAPI.selectSingleNode(_document, "/wsdl:definitions/wsdl:portType");
			Element operation = _document.createElement("wsdl:operation");
			operation.setAttribute("name", operationMetadata.getName());
			
			Element input = _document.createElement("wsdl:input");
			input.setAttribute("message", "qman:"+requestMessageName);
			input.setAttribute("name", methodNameRequest);
			input.setAttribute("wsa:action", Names.NAMESPACE_URI+"/"+operationMetadata.getName());
			
			//name="SetResourcePropertiesRequest" wsa:Action="http://docs.oasis-open.org/wsrf/rpw-2/SetResourceProperties/SetResourcePropertiesRequest"/>
			
			operation.appendChild(input);
			
			Element output = _document.createElement("wsdl:output");
			output.setAttribute("message", "qman:"+responseMessageName);
			output.setAttribute("name", methodNameResponse);
			output.setAttribute("wsa:action", Names.NAMESPACE_URI+"/"+methodNameResponse);
			
			operation.appendChild(output);		
			
			portType.appendChild(operation);
			
			/*
			<operation name='purge'>
				<soap:operation soapAction='purge' />
				<input>
					<soap:body use='literal' />
				</input>
				<output>
					<soap:body use='literal' />
				</output>
			</operation>
			 */
			Element binding = (Element) XPathAPI.selectSingleNode(_document, "/wsdl:definitions/wsdl:binding");
			Element bindingOperation = _document.createElement("wsdl:operation");
			bindingOperation.setAttribute("name", operationMetadata.getName());
			
			Element soapOperation = _document.createElement("wsdl-soap:operation");
			soapOperation.setAttribute("soapAction", Names.NAMESPACE_URI+"/"+operationMetadata.getName());
			
			Element bindingInput = _document.createElement("wsdl:input");
			Element bodyIn = _document.createElement("wsdl-soap:body");
			bodyIn.setAttribute("use", "literal");
			
			Element bindingOutput = _document.createElement("wsdl:output");
			Element bodyOut = _document.createElement("wsdl-soap:body");
			bodyOut.setAttribute("use", "literal");
			
			bindingOutput.appendChild(bodyOut);
			bindingInput.appendChild(bodyIn);
			
			bindingOperation.appendChild(soapOperation);
			bindingOperation.appendChild(bindingInput);
			bindingOperation.appendChild(bindingOutput);
			
			binding.appendChild(bindingOperation);
		} catch(Exception exception) 
		{
			throw new BuilderException(exception);
		}
	}

	/**
	 * Director callback : all attributes have been notified.
	 * Nothing to do here.
	 */
	public void endAttributes() 
	{
		// N.A.
	}

	/**
	 * Director callback : all operations have been notified.
	 * Nothing to do here.
	 */
	public void endOperations() 
	{
		// N.A.
	}

	/**
	 * Returns the WSDL built by this builder.
	 * 
	 * @return the WSDL built by this builder.
	 */
	public Document getWsdl() 
	{
		WsdlDebugger.debug(_objectName,_document);
		return _document;
	}

	/**
	 * Injects the application context environment 
	 * on this builder.
	 * 
	 * @param environment the application context environment.
	 */
	public void setEnvironment(Environment environment) 
	{
		this._environment = (WSDMAdapterEnvironment)environment;
	}
	
	/**
	 * Injects the path of the wsdl document.
	 * 
	 * @param wsdlPath the path of the wsdl document.
	 */
	public void setWsdlPath(String wsdlPath)
	{
		_document = WsdlUtils.createWSDL(_environment, wsdlPath, true);
	}
	
	/**
	 * Create a reference to the WSRP properties element. 
	 * 
	 * @throws BuilderException in case of XPath evaluation problem. 
	 */
	private void createWsrpPropertiesElement() throws BuilderException 
	{
		try
		{
			_wsrpProperties = (Element) XPathAPI.selectSingleNode(
					_document, 
					WSRP_PROPERTIES_XPATH);
		} catch (TransformerException exception)
		{
			LOGGER.error(Messages.QMAN_100040_UNABLE_TO_LOCATE_WSRP_PROPERTIES);
			throw new BuilderException(exception);
		}		
	}

	/**
	 * Creates a template element that will be used for array 
	 * type schema declaration(s). 
	 */
	private void createReusableArrayComplextType()
	{
		_arrayComplexType = XmlUtils.createElement(_document, XSD_COMPLEX_TYPE_QNAME);
		Element sequence = XmlUtils.createElement(_document, XSD_SEQUENCE_QNAME);
		_nestedArrayType = XmlUtils.createElement(_document, XSD_ELEMENT_QNAME);
		_nestedArrayType.setAttribute(NAME_ATTRIBUTE, "entry");
		sequence.appendChild(_nestedArrayType);
		_arrayComplexType.appendChild(sequence);
	}
	
	private void createSchemaElement() throws BuilderException
	{
		try 
		{
			schema = (Element) XPathAPI.selectSingleNode(
					_document.getDocumentElement(),
					QMAN_SCHEMA_XPATH);
		} catch(Exception exception)
		{
			LOGGER.error(
					exception,
					Messages.QMAN_100034_WSDL_SCHEMA_SECTION_NOT_FOUND);
			throw new BuilderException(exception);
		}		
	}
	
	/**
	 * The template WSDL contains a dummy URL as service location that 
	 * needs to be replaced with the real service address.
	 * 
	 * @throws BuilderException when replacement fails (XPath problem).
	 */
	private void replaceDummyServiceLocationOnWsdl() throws BuilderException
	{
		try 
		{
			Attr location = (Attr) XPathAPI.selectSingleNode(
					_document, 
					SERVICE_LOCATION_XPATH);
			
			StringBuilder builder = new StringBuilder("http://")
				.append(InetAddress.getLocalHost().getHostName())
				.append(':')
				.append(System.getProperty(Names.ADAPTER_PORT_PROPERTY_NAME,"8080"))
				.append('/')
				.append(_environment.getContextPath())
				.append('/')
				.append("services/QManWsResource");
			location.setValue(builder.toString());
		} catch(Exception exception)
		{
			LOGGER.error(
					exception,
					Messages.QMAN_100026_SOAP_ADDRESS_REPLACEMENT_FAILURE);
			throw new BuilderException(exception);
		}
	}	
}