<%@ page
   import="org.jivesoftware.openfire.XMPPServer,
           com.raytheon.openfire.plugin.configuration.collaboration.HttpConfigurationPlugin,
           org.jivesoftware.util.ParamUtils,
           java.util.HashMap,
           java.util.Map"
   errorPage="error.jsp"%>

<%@ taglib uri="http://java.sun.com/jstl/core_rt" prefix="c"%>
<%@ taglib uri="http://java.sun.com/jstl/fmt_rt" prefix="fmt"%>

<%
        final long DEFAULT_INTERVAL_S = 60;
	boolean save = ((request.getParameter("save") == null) == false);
	long interval = ParamUtils.getLongParameter(request, "txtInterval", DEFAULT_INTERVAL_S);
	boolean legacy = ParamUtils.getBooleanParameter(request, "chkLegacy", false);
	String dataserverUsers = ParamUtils.getParameter(request, "txtdataserverUsers", true);
	if (dataserverUsers == null){
		dataserverUsers = "";
	}

	HttpConfigurationPlugin plugin = (HttpConfigurationPlugin) XMPPServer.getInstance().getPluginManager().getPlugin("com.raytheon.openfire.plugin.configuration.collaboration");

	if (save)
	{
		plugin.setHttpMonitorInterval(interval * 1000);
		plugin.setLegacySupport(legacy);
		plugin.setDataserverUsers(dataserverUsers);
		response.sendRedirect("http-collaboration-monitor-admin.jsp?settingsSaved=true");
		return;
	}

	interval = (plugin.getHttpMonitorInterval() / 1000);
	String legacyChkValue = plugin.hasLegacySupport() ? "checked" : "";
	dataserverUsers = plugin.getDataserverUsers();
%>

<html>
	<head>
	  <title>Http Collaboration Monitor Settings</title>
	  <meta name="pageID" content="http-collaboration-monitor-admin" />

          <script type="text/javascript">
             function validateInterval()
             {
               var txtInterval = $('txtInterval');
               var spanIntervalError = $('spanIntervalError');
               var btnSubmit = $('btnSubmit');

               var interval = txtInterval.value;
               var isNumber = true;
               var validNumbers = '0123456789';
               if (interval.length == 0)
               {
                  isNumber = false;
               }
               for (var i = 0; i < interval.length; i++)
               {
                  if (validNumbers.indexOf(interval.charAt(i)) == -1)
                  {
                     isNumber = false;
                     break;
                  }
               }   

               if (isNumber)
               {
                  spanIntervalError.style.display = 'none';
                  btnSubmit.disabled = false;
               }
               else
               {
                  spanIntervalError.style.display = 'block';
                  // Disable the submit button
                  btnSubmit.disabled = true;
               }
             }
             function validateWhiteList()
             {
               var txtdataserverUsers = $('txtdataserverUsers');
               var spanWhiteListError = $('spanWhiteListError');
               var btnSubmit = $('btnSubmit');

               var whiteList = txtdataserverUsers.value;  

               if (whiteList.match('^[^,@]+@[^,@]+(,[^,@]+@[^,@]+)*$'))
               {
                  spanWhiteListError.style.display = 'none';
                  btnSubmit.disabled = false;
               }
               else
               {
                  spanWhiteListError.style.display = 'block';
                  // Disable the submit button
                  btnSubmit.disabled = true;
               }
             }
          </script>
	</head>
	<body>
	  <form name="collabPrefForm" action="http-collaboration-monitor-admin.jsp?save" method="post" onsubmit="return validTest();" >
            <div class="jive-contentBoxHeader">
	      Http Collaboration Monitor Settings
            </div>
			<div class="jive-contextBox">
            <% if (ParamUtils.getBooleanParameter(request, "settingsSaved")) { %>

            <div class="jive-success">
              <table cellpadding="0" cellspacing="0" border="0">
                <tbody>
                  <tr>
                    <td class="jive-icon"><img src="images/success-16x16.gif" width="16" height="16" border="0"></td>
                    <td class="jive-icon-label">Settings Saved Successfully!</td>
                  </tr>
                </tbody>
              </table>
            </div>

            <% } %>

            <p>
               Set how often (in seconds) the Http Collaboration Monitor should verify that the dataserver is online.
            </p>

            <table cellpadding="3" cellspacing="0" border="0" width="100%">
              <tbody>
                <tr>
                  <td width="5%" valign="top">interval:&nbsp;</td>
                  <td width="95%">
                    <input type="text" id="txtInterval" name="txtInterval" onkeyup="validateInterval()" 
		    value="<%= interval %>">
                    <span id="spanIntervalError" class="jive-error-text" style="display: none;">
                      The interval must be numeric!
                    </span>
                  </td>
                </tr>
              </tbody>
            </table>
            </br></br></br>
            <p>
               Legacy client support. If enabled, HTTP server security is disabled and configuration is sent in chat messages to support clients older than version 14.3.
            </p>

            <table cellpadding="3" cellspacing="0" border="0" width="100%">
              <tbody>
                <tr>
                  <td width="15%" valign="top">legacy mode:&nbsp;</td>
                  <td width="85%">
                    <input type="checkbox" id="chkLegacy" name="chkLegacy" value="true" <%= legacyChkValue %> >
                  </td>
                </tr>
              </tbody>
            </table>
            </br></br></br>
            <p>
               HTTP Dataserver users list. User ids that http servers can use to communication with openfire. Comma separated list of full user ids
               the first id in the list will be used as the primary HTTP Dataserver. Only user ids that are in this list will be allowed to query for session public keys.
            </p>

             <table cellpadding="3" cellspacing="0" border="0" width="100%">
              <tbody>
                <tr>
                  <td width="5%" valign="top">Dataserver User Ids:&nbsp;</td>
                  <td width="95%">
                    <input type="text" id="txtdataserverUsers" name="txtdataserverUsers" onkeyup="validateWhiteList()" 
		    value="<%= dataserverUsers %>">
                    <span id="spanWhiteListError" class="jive-error-text" style="display: none;">
                      The white list must be in the form 'user1@hostname,user2@hostname'
                    </span>
                  </td>
                </tr>
              </tbody>
            </table>
	   </div>
       <input id="btnSubmit" type="submit" value="Save Settings" />
	  </form>
	</body>
</html>











