<%@ page
   import="org.jivesoftware.openfire.XMPPServer,
           com.raytheon.openfire.plugin.configuration.collaboration.HttpdCollaborationConfigurationPlugin,
           org.jivesoftware.util.ParamUtils,
           java.util.HashMap,
           java.util.Map"
   errorPage="error.jsp"%>

<%@ taglib uri="http://java.sun.com/jstl/core_rt" prefix="c"%>
<%@ taglib uri="http://java.sun.com/jstl/fmt_rt" prefix="fmt"%>

<%
        final long DEFAULT_INTERVAL_S = 60;
	boolean save = ((request.getParameter("save") == null) == false);

        // Currently, users are not allowed to modify the httpd-collaboration location
	// because the rpm is not relocatable.
	long interval = ParamUtils.getLongParameter(request, "txtInterval", DEFAULT_INTERVAL_S);
	boolean legacy = ParamUtils.getBooleanParameter(request, "chkLegacy", false);

	HttpdCollaborationConfigurationPlugin plugin = (HttpdCollaborationConfigurationPlugin) XMPPServer.getInstance().getPluginManager().getPlugin("com.raytheon.openfire.plugin.configuration.collaboration");

	if (save)
	{
		plugin.setHttpdMonitorInterval(interval * 1000);
		plugin.setLegacySupport(legacy);
		response.sendRedirect("httpd-collaboration-monitor-admin.jsp?settingsSaved=true");
		return;
	}

	String location = plugin.getHttpdCollaborationLocation();
	interval = (plugin.getHttpdMonitorInterval() / 1000);
	String legacyChkValue = plugin.hasLegacySupport() ? "checked" : "";
%>

<html>
	<head>
	  <title>Httpd Collaboration Monitor Settings</title>
	  <meta name="pageID" content="httpd-collaboration-monitor-admin" />

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
          </script>
	</head>
	<body>
	  <form action="httpd-collaboration-monitor-admin.jsp?save" method="post">
            <div class="jive-contentBoxHeader">
	      Httpd Collaboration Monitor Settings
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
               Set how often (in seconds) the Httpd Collaboration Monitor should verify that the httpd-collaboration process is running.
            </p>

            <table cellpadding="3" cellspacing="0" border="0" width="100%">
              <tbody>
                <tr>
                  <td width="5%" valign="top">location:&nbsp;</td>
                  <td width="95%">
                    <%= location %>
                  </td>
                </tr>
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
               Legacy message format support. If enabled, configuration is sent in chat message to support clients older than version 14.3.
            </p>

            <table cellpadding="3" cellspacing="0" border="0" width="100%">
              <tbody>
                <tr>
                  <td width="15%" valign="top">legacy message format:&nbsp;</td>
                  <td width="85%">
                    <input type="checkbox" id="chkLegacy" name="chkLegacy" value="true" <%= legacyChkValue %> >
                  </td>
                </tr>
              </tbody>
            </table>
	   </div>
       <input id="btnSubmit" type="submit" value="Save Settings" />
	  </form>
	</body>
</html>











