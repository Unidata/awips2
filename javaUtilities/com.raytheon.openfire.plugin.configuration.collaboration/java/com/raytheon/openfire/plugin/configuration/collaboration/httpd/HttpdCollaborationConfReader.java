/**
 * 
 */
package com.raytheon.openfire.plugin.configuration.collaboration.httpd;

import org.apache.commons.configuration.ConfigurationException;
/**
 * @author bkowal
 *
 */
public class HttpdCollaborationConfReader 
{
	/*
	 * The file location can be hard-coded because the
	 * awips2-httpd-collaboration does not support prefixes, so it
	 * will always be installed beneath /awips2/httpd-collaboration.
	 * And awips2-openfire cannot be installed until
	 * awips2-httpd-collaboration has been installed.
	 */
	private static final String CONF_FILE = 
		"/awips2/httpd_collaboration/etc/httpd/conf/httpd.conf";
	private static final String LISTEN_PROPERTY = "Listen";
	
	private HttpdCollaborationPropertiesConfiguration propertiesConfiguration;
	/**
	 * 
	 */
	public HttpdCollaborationConfReader() 
	{
	}
	
	public void execute()
	throws ConfigurationException
	{
		this.propertiesConfiguration =
			new HttpdCollaborationPropertiesConfiguration();
		this.propertiesConfiguration.load(CONF_FILE);
	}
	
	public String getListenPort()
	{
		return this.propertiesConfiguration.getString(LISTEN_PROPERTY);
	}
}