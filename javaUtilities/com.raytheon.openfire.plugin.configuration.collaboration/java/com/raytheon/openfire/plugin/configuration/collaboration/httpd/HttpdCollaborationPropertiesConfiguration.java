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
package com.raytheon.openfire.plugin.configuration.collaboration.httpd;

import java.io.File;
import java.net.URL;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;

/**
 * This class was created because version 1.6 of commons configuration does not
 * allow you to decide whether you want to allow includes or not. There is a
 * 'setIncludesAllowed' method; however, it is protected.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------    
 * Aug 2012                bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
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