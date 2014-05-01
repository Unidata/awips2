/**
 * 
 */
package com.raytheon.openfire.plugin.configuration.collaboration.httpd;

import org.apache.commons.configuration.ConfigurationException;

import com.raytheon.openfire.plugin.configuration.collaboration.util.HttpdCollaborationUtil;

/**
 * Reads the httpd.conf file for the configurable httpd-collaboration instance.
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
public class HttpdCollaborationConfReader {
    private static String CONF_FILE;

    private static final String LISTEN_PROPERTY = "Listen";

    private HttpdCollaborationPropertiesConfiguration propertiesConfiguration;

    /**
	 * 
	 */
    public HttpdCollaborationConfReader(String _location) {
        CONF_FILE = HttpdCollaborationUtil.endPathIfNecessary(_location)
                + "etc/httpd/conf/httpd.conf";
    }

    public void execute() throws ConfigurationException {
        this.propertiesConfiguration = new HttpdCollaborationPropertiesConfiguration();
        this.propertiesConfiguration.load(CONF_FILE);
    }

    public String getListenPort() {
        return this.propertiesConfiguration.getString(LISTEN_PROPERTY);
    }
}