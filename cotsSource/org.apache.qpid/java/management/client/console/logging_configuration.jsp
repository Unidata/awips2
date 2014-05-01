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
				<jsp:param name="title" value="Logging Configuration"/>
			</jsp:include>
				
			<div id="content" align="center">
				<jsp:include page="/fragments/menu.jsp"/>
						
				<div id="contenttext">
					<div class="panel" align="justify" style="height:500px; overflow-y:auto;">
						<span class="bodytext">
	<form method="post" name="form" action="<%=request.getContextPath() %>/logging_configuration">
							<table>
								<tr>
									<td>
									<fieldset>
									<legend>WSDL & RDM Debugger</legend>
										<table>
											<tr>
												<td>
													<c:choose>
	          											<c:when test="${wsdlDebugEnabled}">
	            											<c:set var="checked" scope="page" value="checked"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="checked" scope="page" value=""/>
									          			</c:otherwise>
		                                    		</c:choose>
													<input type="checkbox" ${checked} name="wsdlDebugEnabled" />
												</td>
												<td nowrap style="font-size: x-small;">
													When this flag is checked all WSDL and RMD messages are written on log file (or console depending on your configuration.)
												 </td>
											</tr>
										</table>
									</fieldset>
									</td>
								</tr>

								<tr>
									<td>
									<fieldset>
												<legend>SOAP Messages Debugger</legend>
										<table>
											
											<tr>
												<td>
													<c:choose>
	          											<c:when test="${soapDebugEnabled}">
	            											<c:set var="checked" scope="page" value="checked"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="checked" scope="page" value=""/>
									          			</c:otherwise>
		                                    		</c:choose>
													<input  ${checked} type="checkbox" name="soapDebugEnabled"/>
												</td>
												<td nowrap style="font-size: x-small;">
													When this flag is checked all SOAP messages (requests & responses) are written on log file (or console depending on your configuration.)
												 </td>
											</tr>
										</table>
											</fieldset>
									</td>
								</tr>

								<tr>
									<td>
											<fieldset>
												<legend>QMan Logger Level</legend>
										<table>
											<tr>
												<td>
													<c:choose>
														<c:when test="${qmanLogLevel == 'DEBUG'}">
	            											<c:set var="qmanDebug" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="qmanDebug" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${qmanLogLevel == 'INFO'}">
	            											<c:set var="qmanInfo" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="qmanInfo" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${qmanLogLevel == 'WARN'}">
	            											<c:set var="qmanWarn" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="qmanWarn" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${qmanLogLevel == 'ERROR'}">
	            											<c:set var="qmanError" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="qmanError" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${qmanLogLevel == 'FATAL'}">
	            											<c:set var="qmanFatal" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="qmanFatal" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>

													<select name="qmanLogLevel">
														<option ${qmanDebug} value="DEBUG">DEBUG</option>
														<option ${qmanWarn} value="WARN">WARNING</option>
														<option ${qmanInfo} value="INFO">INFO</option>
														<option ${qmanError} value="ERROR">ERROR</option>
														<option ${qmanFatal} value="FATAL">FATAL</option>
													</select>
												</td>
												<td nowrap style="font-size: x-small;">
													This is the current priority level set for QMan module (and sub-modules). Note that a WARNING level is recomended in production.
												 </td>
											</tr>
										</table>
											</fieldset>
									</td>
								</tr>

								<tr>
									<td>
											<fieldset>
												<legend>Web Server Logger Level</legend>
										<table>
											
											<tr>
												<td>
													<c:choose>
														<c:when test="${webServerLogLevel == 'DEBUG'}">
	            											<c:set var="webServerDebug" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="webServerDebug" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${webServerLogLevel == 'INFO'}">
	            											<c:set var="webServerInfo" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="webServerInfo" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${webServerLogLevel == 'WARN'}">
	            											<c:set var="webServerWarn" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="webServerWarn" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${webServerLogLevel == 'ERROR'}">
	            											<c:set var="webServerError" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="webServerError" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>
													<c:choose>
														<c:when test="${webServerLogLevel == 'FATAL'}">
	            											<c:set var="webServerFatal" scope="page" value="selected='true'"/>
	          											</c:when>
									          			<c:otherwise>
									            			<c:set var="webServerFatal" scope="page" value=""/>
									          			</c:otherwise>
													</c:choose>

													<select name="webServerLogLevel">
														<option ${webServerDebug} value="DEBUG" >DEBUG</option>
														<option ${webServerWarn} value="WARN">WARNING</option>
														<option ${webServerInfo} value="INFO">INFO</option>
														<option ${webServerError} value="ERROR">ERROR</option>
														<option ${webServerFatak} value="FATAL">FATAL</option>
													</select>
												</td>
												<td nowrap style="font-size: x-small; ">
													This is the current priority level set for QMan module (and sub-modules). Note that a WARNING level is recomended in production.
												 </td>
											</tr>
										</table>
										</fieldset>
									</td>
								</tr>
								<tr>
									<td>
										<input type="submit" value="Submit" title="Submit"/>
									</td>
								</tr>
							</table>
								<br/></br>
								<span style="fony-size: medium; color: red; font-weight: bold">Note that in general a DEBUG level is not reccommended in production (especially for WSDL and SOAP debugger).</span>
						</form>		
                		</span>	
            		</div>
				</div>
			</div>
		</div>
	</body>
</html>
