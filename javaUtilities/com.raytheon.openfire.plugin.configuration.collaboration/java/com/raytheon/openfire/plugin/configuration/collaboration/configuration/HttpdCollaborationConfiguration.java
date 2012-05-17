/**
 * 
 */
package com.raytheon.openfire.plugin.configuration.collaboration.configuration;

/**
 * @author bkowal
 * 
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
	public String toString()
	{
		return URL_PARAMETER_NAME + " : " + this.getHttpdCollaborationURL();
	}
	
	private String getHttpdCollaborationURL() {
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