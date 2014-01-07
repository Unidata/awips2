/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.openfire.plugin.configuration.collaboration.configuration;


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
 * Aug 07, 2012            bkowal      Initial creation
 * Jan 06, 2013  2563      bclement    removed config preamble
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
        return URL_PARAMETER_NAME + " : " + this.getHttpdCollaborationURL();
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