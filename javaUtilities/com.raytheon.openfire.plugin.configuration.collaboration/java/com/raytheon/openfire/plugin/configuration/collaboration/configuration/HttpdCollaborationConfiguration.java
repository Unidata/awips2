/**
 * 
 */
package com.raytheon.openfire.plugin.configuration.collaboration.configuration;

import com.raytheon.openfire.plugin.configuration.collaboration.util.HttpdCollaborationUtil;

/**
 * Used to store and format the information retrieved from the
 * httpd-collaboration httpd.conf file that will be returned to the client.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2012            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class HttpdCollaborationConfiguration {
	private static final String SESSION_DATA_URL_FORMAT_STRING = "http://%s:%s/session_data/";

	private static final String URL_PARAMETER_NAME = "sessionDataHttpURL";

	private String sessionDataHost;

	private String sessionDataPort;

	/**
	 * 
	 */
	public HttpdCollaborationConfiguration() {
		this.sessionDataHost = null;
		this.sessionDataPort = null;
	}

	@Override
	public String toString() {
		return HttpdCollaborationUtil.CONFIG_PREAMBLE + URL_PARAMETER_NAME
				+ " : " + this.getHttpdCollaborationURL()
				+ HttpdCollaborationUtil.DIRECTIVE_SUFFIX;
	}

	public String getHttpdCollaborationURL() {
		return String.format(SESSION_DATA_URL_FORMAT_STRING,
				this.sessionDataHost, this.sessionDataPort);
	}

	public String getSessionDataHost() {
		return sessionDataHost;
	}

	public void setSessionDataHost(String sessionDataHost) {
		this.sessionDataHost = sessionDataHost;
	}

	public String getSessionDataPort() {
		return sessionDataPort;
	}

	public void setSessionDataPort(String sessionDataPort) {
		this.sessionDataPort = sessionDataPort;
	}
}