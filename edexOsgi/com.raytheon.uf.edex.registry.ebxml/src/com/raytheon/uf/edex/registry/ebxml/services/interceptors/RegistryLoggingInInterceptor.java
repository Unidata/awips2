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
package com.raytheon.uf.edex.registry.ebxml.services.interceptors;

import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.LoggingInInterceptor;
import org.apache.cxf.message.Message;

/**
 * <pre>
 * 
 * Custom inbound traffic logging interceptor.  
 * Logging of web service traffic may be enabled/disabled via the
 * ebxml.registry.webservices.log.enabled properties item 
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/11/2015    4448        bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistryLoggingInInterceptor extends LoggingInInterceptor {

    /**
     * System property holding the status (enabled/disabled) of the CXF traffic
     * logging
     */
    private static final Boolean ENABLED = Boolean.parseBoolean(System
            .getProperty("ebxml.registry.webservices.log.enabled"));

    /**
     * Creates a new RegistryLoggingInInterceptor
     */
    public RegistryLoggingInInterceptor() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.cxf.interceptor.LoggingInInterceptor#handleMessage(org.apache
     * .cxf.message.Message)
     */
    @Override
    public void handleMessage(Message message) throws Fault {
        // If traffic loggin is enabled, proceed with logging
        if (ENABLED) {
            super.handleMessage(message);
        }
    }
}
