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
				<jsp:param name="title" value="Brokers Management"/>
			</jsp:include>
				
			<div id="content" align="center">
				<jsp:include page="/fragments/menu.jsp"/>
				
			<div id="contenttext">
			<div class="panel" align="justify">
				<span class="bodytext">
                	<table width="100%">
                        <tr>    
                        	<td valign="top">
								<fieldset>
                                	<legend>Connected Brokers</legend>
                                    <table width="100%" cellspacing="1">
	                                    <tr>
	                                    	<th nowrap="nowrap" align="center">Host</th>
	                                    	<th nowrap="nowrap" align="center" >Port</th>
	                                    	<th nowrap="nowrap" align="center">Virtual Host</th>
	                                    	<th nowrap="nowrap" align="center">Username</th>
	                                    	<th nowrap="nowrap" align="center">Initial Pool Capacity</th>
	                                    	<th nowrap="nowrap" align="center">Max Pool Capacity</th>
	                                    	<th nowrap="nowrap" align="center">Max Wait Timeout</th>
	                                    </tr>	
	                                        <c:forEach var="broker" items="${model}" varStatus="rowCounter">	
	                                        	<c:choose>
          											<c:when test="${rowCounter.count % 2 == 0}">
            											<c:set var="bgcolor" scope="page" value="EAEAEA"/>
          											</c:when>
								          			<c:otherwise>
								            			<c:set var="bgcolor" scope="page" value="FFFFFF"/>
								          			</c:otherwise>
	                                    		</c:choose>
	                                    	<tr>
	                                        	<td nowrap style="font-size: xx-small; font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${broker.host}"/></td>
	                                        	<td nowrap style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${broker.port}"/></td>
	                                        	<td style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${broker.virtualHost}"/></td>
	                                        	<td style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${broker.username}"/></td>
	                                        	<td style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${broker.initialPoolCapacity}"/></td>
	                                        	<td style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${broker.maxPoolCapacity}"/></td>
	                                        	<td style="font-size: xx-small;  font-weight: bold;" bgcolor="${bgcolor}"><c:out value="${broker.maxWaitTimeout}"/></td>
	                                        </tr>
                                        </c:forEach>
                                    </table>
                                </fieldset>                            	
                            </td>
                        </tr>
                        <tr>
                        <td>
                        		<form name="form" action="<%=request.getContextPath()%>/brokers_management" method="post">
	                        		<fieldset>
                        				<legend>New Broker Connection Data</legend>
	                        		<table>
	                        			<tr>
	                        				<td>
	                        					Host : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="host"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The hostname where the broker is running.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td>
	                        					Port : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="port"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The port number where the broker is running.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td>
	                        					Virtual Host : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="virtualHost"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The virtual host name.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td>
	                        					Username : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="username"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The username used for estabilish connection with broker.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td>
	                        					Password : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="password"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The password  used for estabilish connection with broker.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td>
	                        					Initial Pool Capacity : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="initialCapacity"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The number of connections that must be immediately opened.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td>
	                        					Max Pool Capacity : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="maxCapacity"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The maximum number of open connections.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td>
	                        					Max Wait Timeout : 
	                        				</td>
	                        				<td>
	                        					<input type="text" name="maxWaitTimeout"/>
	                        				</td>
	                        				<td style="font-size: x-small;">
	                        					The maximum amount of time that a client will wait for obtaining a connection.
	                        				</td>
	                        			</tr>
	                        			<tr>
	                        				<td colspan="3" align="center">
	                        					<input type="submit" value="Connect"/>
	                        				</td>
	                        			</tr>
	                        		</table>
	                        	</fieldset>
	                        </td>
	                        </form> 	
                        </tr>
                        <tr>
	                        <td nowrap style="font-size: x-small; font-weight: bold; color=red;">
	                        	<ul>	
	                        	<c:forEach var="errorMessage" items="${errors}">	
			                    	<li><span style="font-size: medium; font-weight: bold; color:red;">${errorMessage}</span></li>
			                    </c:forEach>
			                    </ul>	
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
