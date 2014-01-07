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