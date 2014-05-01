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

<%
	BrokerModel model = (BrokerModel) request.getAttribute("model");	
%>
<html>
	<head>
		<link rel="stylesheet" href="<%=request.getContextPath()%>/images/style.css" type="text/css" />
		<title>QMan Administration Console</title>
	</head>
	<body>
		<div id="page" align="center">
			<jsp:include page="/fragments/header.jsp">
				<jsp:param name="title" value="Resources Management"/>
			</jsp:include>
				
			<div id="content" align="center">
				<jsp:include page="/fragments/menu.jsp"/>
				
				
			<div id="contenttext">
        	<div id="wsdmmenu" align="left">
                <ul>
                	<% if (model != null) {%>
                    <li><a href="#"><span>${model.id}</span></a></li>
                    <%} %>
                </ul>
            </div>
            <br />
			<div class="panel" align="justify" style="height:500px; overflow-y:auto;">
				<span class="bodytext">
                	<table width="100%" border="0" cellpadding="1" cellspacing="2">
<%
	if (model != null ){
		Set<String> categoryNames = model.getCategoryNames();
		for(String categoryName : categoryNames)
		{
			List<ObjectName> categoryObjects = model.getCategory(categoryName);
%>                	          	
                    	<tr>
                        	<td valign="top" nowrap align="left">
								<fieldset>
									<legend><%=categoryName%></legend>
                                        		<h4 style="color: #006633; font-size: xx-small">
                                        			<ul>
                                                 	<%
                                                 		for (ObjectName objectName : categoryObjects)
                                                 		{%>
                                                 		
                                                 		<li>
                                                 			<a href="<%=request.getContextPath()%>/jmx_perspective?resourceId=<%=objectName%>">
                                                 				<%=objectName.getKeyProperty(Names.OBJECT_ID)%>
                                                 			</a>
                                                 		</li>
                                                 		<%
                                                 		}
                                                 	%>  
                                                 </ul>	 
								</fieldset>                                             
                        	</td>
						</tr>
<%
		}
	} else {
%>			
<table><tr>
<td nowrap style="font-weight: bold;" >Sorry, but it seems that QMan is not connected with any broker...</td>
</tr>
</table>
<%
	}
%>
				</table>
				</span>			
			</div>
		</div>
	</body>
</html>
