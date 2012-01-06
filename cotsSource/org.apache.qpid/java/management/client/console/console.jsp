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
<html>
	<head>
		<link rel="stylesheet" href="<%=request.getContextPath()%>/images/style.css" type="text/css" />
		<title>QMan Administration Console</title>
	</head>
	<body>
		<div id="page" align="center">
			<jsp:include page="/fragments/header.jsp">
				<jsp:param name="title" value="System Overview"/>
			</jsp:include>
				
			<div id="content" align="center">
				<jsp:include page="/fragments/menu.jsp"/>
						
				<div id="contenttext">
					<div class="panel" align="justify" style="height:500px; overflow-y:auto;">
						<span class="bodytext">
                			<table width="100%" border="0">
                    			<tr>
                        			<td valign="top" nowrap align="center">
										<fieldset>
											<legend>QMan</legend>
				                            <table cellspacing="2">
				                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold;" align="center">Version</td></tr>
				                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.version}</td></tr>
				                            	<tr><td><br /></td></tr>
				                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold" align="center">Version Name</td></tr>
				                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.versionName}</td></tr>
				                            	<tr><td><br /></td></tr>
				                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold" align="center">Start Date</td></tr>
				                            	<tr>
				                            		<td style="font-size: xx-small;" align="center">
					                            		<fmt:formatDate 
					                            			value="${requestScope.model.startDate}" 
			    											pattern="MM/dd/yyyy hh:mm:ss"/>
					                            		</td>
				                            	</tr>
				                            	<tr><td><br /></td></tr>
				                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold" align="center">Host</td></tr>
				                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.host}</td></tr>
				                            	<tr><td><br /></td></tr>
				                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold" align="center">Port</td></tr>
				                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.port}</td></tr>
				                            </table>
									  	</fieldset>
										<fieldset>
											<legend>Operating System</legend>			
					                            <table cellspacing="2">
					                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold;" align="center">Name</td></tr>
					                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.osName}</td></tr>
					                            	<tr><td><br /></td></tr>
					                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold" align="center">Version</td></tr>
					                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.osVersion}</td></tr>
					                            	<tr><td><br /></td></tr>
					                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold" align="center">Arch</td></tr>
					                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.archName}</td></tr>
					                            	<tr><td><br /></td></tr>
					                            	<tr><td style="color: #006633; font-size: xx-small; font-weight:bold" align="center">Processors</td></tr>
					                            	<tr><td style="font-size: xx-small;" align="center">${requestScope.model.processors}</td></tr>
					                            </table>
										</fieldset>
                        			</td>
                            		<td valign="top">
										<fieldset>
											<legend>JVM Environment</legend>
				                            <table cellspacing="5">
				                            	<tr>
				                           	  		<td valign="top">
				                                        <h4 style="color: #006633; font-size: xx-small">Boot Classpath : 
				                                            <p/> 
				                                                  <c:forEach var="entry" items="${model.bootClasspath}">
				                                                  		<c:out value="${entry}"/>;
				                                                  	<br/>
				                                                  </c:forEach>
                              							</h4>	
                                    				</td>
				                                   <td valign="top">
                										<h4 style="color: #006633; font-size: xx-small">
	                            							Input Arguments :
	                                						<p/>
															<c:forEach var="argument" items="${model.inputArguments}">
		                                                  		<c:out value="${argument}"/>;
		                                                  		<br/>
		                                                  	</c:forEach>
														</h4>
                                					</td>
                                				</tr>
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
