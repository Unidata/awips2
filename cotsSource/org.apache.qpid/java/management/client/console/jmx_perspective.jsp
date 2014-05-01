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
<%@page import="java.util.List"%>
<html>
	<head>
		<link rel="stylesheet" href="<%=request.getContextPath()%>/images/style.css" type="text/css" />
		<title>QMan Administration Console</title>
	</head>
	<body>
		<div id="page" align="center">
			<jsp:include page="/fragments/header.jsp">
				<jsp:param name="title" value="Resource Management - JMX Perspective"/>
			</jsp:include>
				
			<div id="content" align="center">
				<jsp:include page="/fragments/menu.jsp"/>
				
			<div id="contenttext">
        	<div id="wsdmmenu" align="left">
                <ul>
                    <li><a href="<%=request.getContextPath()%>/jmx_perspective?resourceId=${resourceId}"><span>JMX</span></a></li>
                    <li><a href="<%=request.getContextPath()%>/wsdm_properties_perspective?resourceId=${resourceId}"><span>WS-DM</span></a></li>
                </ul>
            </div>
            <br />
			<div class="panel" align="justify">
				<span class="bodytext">
                	<table width="100%">
                    	<tr>
                        	<td valign="top" colspan="2">
                            	<fieldset>
                                	<legend>ObjectName</legend>
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
								<fieldset>
                                	<legend>Attributes</legend>
                                    <table width="100%" cellspacing="1">
                                    	<tr>
                                        	<th>Name</th>
                                        	<th>Type</th>
                                        	<th>Value</th>
                                        	<th>Access</th>
                                        </tr>	
	                                        <c:forEach var="attribute" items="${metadata.attributes}" varStatus="rowCounter">	
	                                        	<c:choose>
          											<c:when test="${rowCounter.count % 2 == 0}">
            											<c:set var="bgcolor" scope="page" value="EAEAEA"/>
          											</c:when>
								          			<c:otherwise>
								            			<c:set var="bgcolor" scope="page" value="FFFFFF"/>
								          			</c:otherwise>
	                                    		</c:choose>
	                                        	<c:choose>
          											<c:when test="${attribute.writable}">
            											<c:set var="access" scope="page" value="RW"/>
          											</c:when>
								          			<c:otherwise>
								            			<c:set var="access" scope="page" value="RO"/>
								          			</c:otherwise>
	                                    		</c:choose>
	                                    	<tr>
	                                        	<td nowrap style="font-size: xx-small; font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${attribute.name}"/></td>
	                                        	<td nowrap style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${attribute.type}"/></td>
	                                        	<td style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${attributes[attribute.name]}"/></td>
	                                        	<td nowrap style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${access}"/></td>
	                                        </tr>
                                        </c:forEach>
                                    </table>
                                </fieldset>                            	
                            </td>
                        	<td valign="top">
								<fieldset>
                                	<legend>Operations</legend>   
									<table width="100%" cellspacing="0">
                                    	<tr>
                                        	<th>Name</th>
                                        	<th>Arguments</th>
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
	                                                			<c:out value="${argument.name}"/> (<c:out value="${argument.type}"/>)
	                                                		</li>
	                                                	</c:forEach>
	                                                </ul>
	                                            </td>
	                                        </tr>
                                        </c:forEach>
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
