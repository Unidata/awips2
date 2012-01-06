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

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

public interface Constants
{
	String WSRP_PROPERTIES_XPATH =	"/wsdl:definitions/wsdl:types/xsd:schema[" +
																"@targetNamespace='http://amqp.apache.org/qpid/management/qman']" +
																"/xsd:element[@name='QManWsResourceProperties']/xsd:complexType/xsd:sequence";
	
	String SERVICE_LOCATION_XPATH = "/wsdl:definitions/wsdl:service/wsdl:port/wsdl-soap:address/@location";
	String QMAN_SCHEMA_XPATH = "/wsdl:definitions/wsdl:types/xsd:schema[@targetNamespace='http://amqp.apache.org/qpid/management/qman']";
	
	String MIN_OCCURS = "minOccurs";
	String REF_ATTRIBUTE = "ref";
	String NAME_ATTRIBUTE = "name";
	String TYPE_ATTRIBUTE  ="type";
	
	QName XSD_ELEMENT_QNAME = new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI,"element","xsd");
	QName XSD_COMPLEX_TYPE_QNAME = new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI,"complexType","xsd");
	QName XSD_SEQUENCE_QNAME = new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI,"sequence","xsd");

}