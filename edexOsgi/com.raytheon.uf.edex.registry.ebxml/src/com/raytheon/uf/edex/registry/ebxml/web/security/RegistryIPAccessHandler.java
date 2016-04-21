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
package com.raytheon.uf.edex.registry.ebxml.web.security;

import org.eclipse.jetty.server.handler.IPAccessHandler;

/**
 * 
 * IP Access handler class used by Jetty to control white/black list IPs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/27/2014    1712       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistryIPAccessHandler extends IPAccessHandler {

	private static final String DELIMITER = ";";

	private static final String WHITELIST_PROPERTY = "ebxml-webserver-ip-whitelist";

	private static final String BLACKLIST_PROPERTY = "ebxml-webserver-ip-blacklist";

	public void setIPAccessControl() {
		String whiteList = System.getProperty(WHITELIST_PROPERTY);
		if (whiteList != null && !whiteList.trim().isEmpty()) {
			setWhite(whiteList.split(DELIMITER));
		}

		String blackList = System.getProperty(BLACKLIST_PROPERTY);
		if (blackList != null && !blackList.trim().isEmpty()) {
			setBlack(blackList.split(DELIMITER));
		}
	}
}
