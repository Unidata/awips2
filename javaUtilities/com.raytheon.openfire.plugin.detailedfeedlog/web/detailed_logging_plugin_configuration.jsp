<%--
 - This software was developed and / or modified by Raytheon Company,
 - pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 - 
 - U.S. EXPORT CONTROLLED TECHNICAL DATA
 - This software product contains export-restricted data whose
 - export/transfer/disclosure is restricted by U.S. law. Dissemination
 - to non-U.S. persons whether in the United States or abroad requires
 - an export license or other authorization.
 - 
 - Contractor Name:        Raytheon Company
 - Contractor Address:     6825 Pine Street, Suite 340
 -                         Mail Stop B8
 -                         Omaha, NE 68106
 -                         402.291.0100
 - 
 - See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 - further licensing information.
 -
 - JSP for modifying the purge time and the length of logs to keep
 - 
 - <pre>
 - 
 - SOFTWARE HISTORY
 - 
 - Date         Ticket#    Engineer    Description
 - ------------ ---------- ----------- --------------------------
 - Aug 16, 2012            mnash     Initial creation
 - 
 - </pre>
 - 
 - @author mnash
 - @version 1.0
 -
--%>

<%@ page import="org.jivesoftware.util.ParamUtils" %>
<%@ page import="org.jivesoftware.util.JiveGlobals" %>				 
<%@ page import="java.util.*" %>
<%@ page import="java.lang.*" %>
<%@ page import="java.net.URLEncoder" %>
<%@ taglib uri="http://java.sun.com/jstl/core_rt" prefix="c" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://java.sun.com/jstl/fmt_rt" prefix="fmt" %>

<jsp:useBean id="webManager" class="org.jivesoftware.util.WebManager"  />
<% webManager.init(request, response, session, application, out ); %>

<%  
	// Get parameters
	String LOG_FREQ = "detailedlogfreq";
	String LOG_TTL = "detailedlogttl";
	
	// was this request a save request?
    boolean save = request.getParameter("save") != null;
    
    // was it successful, will determine later based on any error messages
    boolean success = false;
    
    // grab the global for the frequency of purge time (currently set as, otherwise set to 60)
    int logfreqtime = JiveGlobals.getIntProperty(LOG_FREQ, 60);
    
    // grab the global for the amount of time to keep (currently set as, otherwise set to 21600)
    int logttltime = JiveGlobals.getIntProperty(LOG_TTL, 21600);
    
    // see if a parameter was sent for log frequency, otherwise use global value
    int logfreq = ParamUtils.getIntParameter(request,LOG_FREQ,logfreqtime);
    
    // see if a parameter was sent for log time to keep, otherwise use global value
    int logttl = ParamUtils.getIntParameter(request,LOG_TTL, logttltime);

    // Handle a save, we need to set those properties in the JiveGlobals class
    Map<String,String> errors = new HashMap<String,String>();
    if (save) {
        // do validation
        if (logfreq <= 0) {
        	errors.put(LOG_FREQ,LOG_FREQ);
        }
        if (logttl <= 0) {
        	errors.put(LOG_TTL, LOG_TTL);
        } 
        
        if (errors.isEmpty()) {
       		JiveGlobals.setProperty(LOG_FREQ,Integer.toString(logfreq));
       		JiveGlobals.setProperty(LOG_TTL,Integer.toString(logttl));
       		success = true;
        } else {
        	success = false;
        }
    }
%>

<html>
	<head>
		<title>Detailed Room Logging</title>
	</head>
<meta name="pageID" content="detailed-logging"/>
<meta name="subPageID" content="detailed-logging"/>
	<body>

	<p>
		Use the form below to configure logging for chat rooms
	</p>

<%  if (success) { %>
<div class="jive-success">
    <table cellpadding="0" cellspacing="0" border="0">
        <tbody>
            <tr><td class="jive-icon"><img alt="Success" src="images/success-16x16.gif" width="16" height="16"
                                           border="0"></td>
                <td class="jive-icon-label">
                    Successfully update logging settings.
                </td></tr>
        </tbody>
    </table>
</div><br>
<%  } else if (errors.isEmpty() == false) { %>
<div class="jive-error">
    <table cellpadding="0" cellspacing="0" border="0">
        <tbody>
            <tr>
                <td class="jive-icon"><img alt="error" src="images/error-16x16.gif" width="16" height="16"
                                           border="0"/></td>
                <td class="jive-icon-label">
                   <% if (errors.get(LOG_FREQ) != null || errors.get(LOG_TTL) != null) { %>
                   An invalid value was entered.  Please enter a valid value.
                    <% } %>
                </td>
            </tr>
        </tbody>
    </table>
</div>
<%  } %>

<form action="detailed_logging_plugin_configuration.jsp" method="post">
    <div class="jive-contentBoxHeader">Logging Information</div>
	<div class="jive-contentBox">
		<table cellpadding="3" cellspacing="0" border="0">
			<tbody>
            	<tr>
                	<td class="c1">
                   		Detailed logging purge frequency (seconds)
                	</td>
            	    <td>
        	            <input type="number" size="30" maxlength="20" name="detailedlogfreq" value="<%= (logfreq > 0 ? logfreq : JiveGlobals.getIntProperty("detailedlogfreq", 60)) %>">
    	            </td>
	            </tr>
            	<tr>
                	<td class="c1">
                		Detailed logging time to keep (seconds) 
                	</td>
                	<td>
                    	<input type="number" size="30" maxlength="20" name="detailedlogttl" value="<%= (logttl > 0 ? logttl : JiveGlobals.getIntProperty("detailedlogttl", 21600)) %>">
                	</td>
            	</tr>
        	</tbody>
        </table>
	</div>
    <input type="submit" name="save" value="Save Properties">
</form>
</body>
</html>