/**
 * 
 */
package com.raytheon.openfire.plugin.configuration.collaboration.httpd;

import java.io.File;
import java.net.URL;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;

/**
 * @author bkowal
 * 
 *         This class was created because version 1.6 of commons configuration
 *         does not allow you to decide whether you want to allow includes or
 *         not. There is a 'setIncludesAllowed' method; however, it is
 *         protected.
 * 
 */
public class HttpdCollaborationPropertiesConfiguration extends PropertiesConfiguration {
	/**
	 * 
	 */
	public HttpdCollaborationPropertiesConfiguration() {
		super();
	}

	/**
	 * @param fileName
	 * @throws ConfigurationException
	 */
	public HttpdCollaborationPropertiesConfiguration(String fileName)
			throws ConfigurationException {
		super(fileName);
	}

	/**
	 * @param file
	 * @throws ConfigurationException
	 */
	public HttpdCollaborationPropertiesConfiguration(File file) throws ConfigurationException {
		super(file);
	}

	/**
	 * @param url
	 * @throws ConfigurationException
	 */
	public HttpdCollaborationPropertiesConfiguration(URL url) throws ConfigurationException {
		super(url);
	}

	@Override
	public void setBasePath(String basePath) {
		super.setBasePath(basePath);
		this.setIncludesAllowed(false);
	}
}