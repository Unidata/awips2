<%@ page
   import="org.jivesoftware.openfire.XMPPServer,
           com.raytheon.openfire.plugin.configuration.collaboration.iq.FeedVenueConfigHandler,
           org.jivesoftware.util.ParamUtils,
           java.util.HashMap,
           java.util.Map"
   errorPage="error.jsp"%>

<%@ taglib uri="http://java.sun.com/jstl/core_rt" prefix="c"%>
<%@ taglib uri="http://java.sun.com/jstl/fmt_rt" prefix="fmt"%>

<%!
	public static String getSafeParam(HttpServletRequest request, String name){
		String rval = ParamUtils.getParameter(request, name, true);
		return rval != null ? rval : "";
	}
%>

<%
	boolean save = ((request.getParameter("save") == null) == false);
	String feedVenueName = getSafeParam(request, "feedVenueName");
	String feedVenueSubdomain = getSafeParam(request, "feedVenueSubdomain");
	/* feeVenueType will be added when aggregate rooms are implemented */

	if (save)
	{
	    boolean success = FeedVenueConfigHandler.setFeedVenueConfig(feedVenueSubdomain, feedVenueName);
		response.sendRedirect("feed-venue-admin.jsp?settingsSaved=" + success);
		return;
	}

	feedVenueName = FeedVenueConfigHandler.getFeedVenueName();
	feedVenueSubdomain = FeedVenueConfigHandler.getFeedVenueSubdomain();
%>

<html>
	<head>
	  <title>Collaboration Feed Venue Settings</title>
	  <meta name="pageID" content="feed-venue-admin" />
	</head>
	<body>
	  <form name="collabPrefForm" action="feed-venue-admin.jsp?save" method="post" onsubmit="return validTest();" >
            <div class="jive-contentBoxHeader">
	      Collaboration Feed Venue Settings
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

            <% }else if (ParamUtils.getParameter(request, "settingsSaved") != null){ %>
            
             <div class="jive-error">
              <table cellpadding="0" cellspacing="0" border="0">
                <tbody>
                  <tr>
                    <td class="jive-icon"><img src="images/error-16x16.gif" width="16" height="16" border="0"></td>
                    <td class="jive-icon-label">Invalid configuration settings, check that subdomain and room exist on server</td>
                  </tr>
                </tbody>
              </table>
            </div>
            
            <% } %>

            <p>
               Set Multi-User Chat Subdomain for Feed Venue chatroom
            </p>

            <table cellpadding="3" cellspacing="0" border="0" width="100%">
              <tbody>
                <tr>
                  <td width="5%" valign="top">Subdomain:&nbsp;</td>
                  <td width="95%">
                    <input type="text" id="feedVenueSubdomain" name="feedVenueSubdomain"  
		    value="<%= feedVenueSubdomain %>">
                  </td>
                </tr>
              </tbody>
            </table>
            </br></br></br>
            <p>
               Set Feed Venue chatroom name
            </p>

            <table cellpadding="3" cellspacing="0" border="0" width="100%">
              <tbody>
                <tr>
                  <td width="5%" valign="top">Room Name:&nbsp;</td>
                  <td width="95%">
                    <input type="text" id="feedVenueName" name="feedVenueName"  
		    value="<%= feedVenueName %>">
                  </td>
                </tr>
              </tbody>
            </table>
	   </div>
       <input id="btnSubmit" type="submit" value="Save Settings" />
	  </form>
	</body>
</html>











