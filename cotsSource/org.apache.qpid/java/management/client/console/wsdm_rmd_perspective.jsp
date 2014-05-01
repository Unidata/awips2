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

<%@ taglib uri="http://java.sun.com/jsp/jstl/fmt" prefix="fmt" %>
<%@ taglib uri='http://java.sun.com/jsp/jstl/core' prefix="c"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/xml" prefix="x"%>

<%@page import="org.apache.qpid.management.web.action.BrokerModel"%>
<%@page import="java.util.Set"%>
<%@page import="javax.management.ObjectName"%>
<%@page import="org.apache.qpid.management.Names"%>
<%@page import="java.util.*"%>
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
				<jsp:param name="title" value="Resource Management - WS-DM RMD Perspective"/>
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
								<div class="panel" align="left" style="height:500px; width=200px; overflow-y:auto; font-size: smaller; font-weight:bold;">								
									<pre>  <c:out value="${rmd}" /> </pre>
                            	</div>
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
