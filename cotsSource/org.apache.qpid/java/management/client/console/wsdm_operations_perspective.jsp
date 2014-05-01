<!--
 
 Licensed to the Apache Software Foundation (ASF) under one
 or more contributor license agreements.  See the NOTICE file
 distributed with this work for additional information
 regarding copyright ownership.  The ASF licenses this file
 to you under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance
 with the License.  You may obtain a copy of the License at
 
   http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied.  See the License for the
 specific language governing permissions and limitations
 under the License.
 
-->

<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib uri='http://java.sun.com/jsp/jstl/core' prefix='c'%>
<%@page import="org.apache.qpid.management.web.action.BrokerModel"%>
<%@page import="java.util.Set"%>
<%@page import="javax.management.ObjectName"%>
<%@page import="org.apache.qpid.management.Names"%>
<%@page import="java.util.*"%>
<%
	Map<String,String> java2Xml = new HashMap<String,String>();
java2Xml.put(UUID.class.getName(),"qman:uuid");
java2Xml.put(Long.class.getName(),"xsd:long");
java2Xml.put(long.class.getName(),"xsd:long");
java2Xml.put(Boolean.class.getName(),"xsd:boolean");
java2Xml.put(boolean.class.getName(),"xsd:boolean");
java2Xml.put(Double.class.getName(),"xsd:double");
java2Xml.put(double.class.getName(),"xsd:double");
java2Xml.put(Float.class.getName(),"xsd:float");
java2Xml.put(float.class.getName(),"xsd:float");
java2Xml.put(Integer.class.getName(),"xsd:integer");
java2Xml.put(int.class.getName(),"xsd:integer");
java2Xml.put(Short.class.getName(),"xsd:short");
java2Xml.put(short.class.getName(),"xsd:short");
java2Xml.put(String.class.getName(),"xsd:string");
java2Xml.put(URI.class.getName(),"xsd:anyURI");
java2Xml.put(Date.class.getName(),"xsd:dateTime");
java2Xml.put(QName.class.getName(),"xsd:QName");
java2Xml.put(Element.class.getName(),"xsd:element");
java2Xml.put(byte[].class.getName(),"xsd:base64Binary");
java2Xml.put(Long[].class.getName(),"qman:arrayOfLong");	
java2Xml.put(long[].class.getName(),"qman:arrayOfLong");
java2Xml.put(Boolean[].class.getName(),"qman:arrayOfBoolean");
java2Xml.put(boolean[].class.getName(),"qman:arrayOfBoolean");
java2Xml.put(Double[].class.getName(),"qman:arrayOfDouble");
java2Xml.put(double[].class.getName(),"qman:arrayOfDouble");
java2Xml.put(Float[].class.getName(),"qman:arrayOfFloat");
java2Xml.put(float[].class.getName(),"qman:arrayOfFloat");
java2Xml.put(Integer[].class.getName(),"qman:arrayOfInteger");
java2Xml.put(int[].class.getName(),"qman:arrayOfInteger");
java2Xml.put(Short[].class.getName(),"qman:arrayOfShort");
java2Xml.put(short[].class.getName(),"qman:arrayOfShort");
java2Xml.put(String[].class.getName(),"qman:arrayOfString");
java2Xml.put(URI[].class.getName(),"qman:arrayOfURI");
java2Xml.put(Date[].class.getName(),"qman:arrayOfDate");
java2Xml.put(Map.class.getName(),"qman:map");
java2Xml.put(HashMap.class.getName(),"qman:map");

pageContext.setAttribute("types",java2Xml);
%>
<%@page import="java.net.URI"%>
<%@page import="javax.xml.namespace.QName"%>
<%@page import="org.w3c.dom.Element"%>
<html>
	<head>
		<link rel="stylesheet" href="<%=request.getContextPath()%>/images/style.css" type="text/css" />
		<title>QMan Administration Console</title>
	</head>
	<body>
		<div id="page" align="center">
			<jsp:include page="/fragments/header.jsp">
				<jsp:param name="title" value="Resource Management - WS-DM Operations Perspective"/>
			</jsp:include>
				
			<div id="content" align="center">
				<jsp:include page="/fragments/menu.jsp"/>
				
			<div id="contenttext">
        	<div id="wsdmmenu" align="left">
                <ul>
                    <li><a href="<%=request.getContextPath()%>/jmx_perspective?resourceId=${resourceId}"><span>JMX</span></a></li>
                    <li><a href="<%=request.getContextPath()%>/wsdm_properties_perspective?wsresourceId=${resourceId}"><span>WS-DM</span></a></li>
                </ul>
            </div>
            <br />
			<div class="panel" align="justify">
				<span class="bodytext">
                	<table width="100%">
                    	<tr>
                        	<td valign="top" colspan="2">
                            	<fieldset>
                                	<legend>Resource ID</legend>
                                    <ul>
                                    	<c:forEach var="property" items="${nameAttributes}">
                                            	<li>
                                            		<c:out value="${property}"/>
                                            	</li>
                                          </c:forEach>      
                                     </ul>
                                </fieldset>
                            </td>
                        </tr>
						<tr>
                        	<td valign="top">
                            	<div id="wsdmmenu" align="left" style="font-size: small;">
                                    <ul>
                                        <li><a href="<%=request.getContextPath()%>/wsdm_properties_perspective?resourceId=${resourceId}"><span>Properties</span></a></li>
                                        <li><a href="<%=request.getContextPath()%>/wsdm_operations_perspective?resourceId=${resourceId}""><span>Operations</span></a></li>
                                        <li><a href="<%=request.getContextPath()%>/wsdm_wsdl_perspective?resourceId=${resourceId}""><span>WSDL</span></a></li>
                                        <li><a href="<%=request.getContextPath()%>/wsdm_rmd_perspective?resourceId=${resourceId}""><span>RDM</span></a></li>
                                    </ul>
                                </div>
                            </td>
                        </tr>                                          
                        <tr>    
                        	<td valign="top">
								<fieldset>
                                	<legend>Attributes</legend>
                                    <table width="100%" cellspacing="1">
	                                    <tr>
	                                    	<th nowrap="nowrap" align="center">Name</th>
	                                    	<th nowrap="nowrap" align="center">Arguments</th>
	                                    	<th nowrap="nowrap" align="center">Faults</th>
	                                    </tr>	
	                                        <c:forEach var="operation" items="${metadata.operations}" varStatus="rowCounter">	
	                                        	<c:choose>
          											<c:when test="${rowCounter.count % 2 == 0}">
            											<c:set var="bgcolor" scope="page" value="EAEAEA"/>
          											</c:when>
								          			<c:otherwise>
								            			<c:set var="bgcolor" scope="page" value="FFFFFF"/>
								          			</c:otherwise>
	                                    		</c:choose>
	                                    	<tr>
	                                        	<td nowrap style="font-size: xx-small; font-weight: bold;"  bgcolor="${bgcolor}"><c:out value="${operation.name}"/></td>
	                                        	<td nowrap style="font-size: xx-small; font-weight: bold;"  bgcolor="${bgcolor}">
	                                            	<ul>
                                        				<c:forEach var="argument" items="${operation.signature}">
	                                                		<li>
	                                                			<c:out value="${argument.name}"/> (<c:out value="${types[argument.type]}"/>)
	                                                		</li>
	                                                	</c:forEach>
	                                                </ul>
	                                            </td>
	                                        	<td nowrap style="font-size: xx-small; font-weight: bold;"  bgcolor="${bgcolor}">
	                                        		<ul>
	                                        			<li>qman:EntityInstanceNotFoundFault</li>
	                                        			<li>qman:OperationInvocationFault</li>
	                                        			<li>qman:QManFault</li>
	                                        		</ul>
	                                        	</td>
	                                        </tr>
                                        </c:forEach>
                                    </table>
                                </fieldset>                            	
                            </td>
                        </tr>
                    </table>
                </span>	
            </div>
			</div>
			</div>
		</div>
	</body>
</html>
