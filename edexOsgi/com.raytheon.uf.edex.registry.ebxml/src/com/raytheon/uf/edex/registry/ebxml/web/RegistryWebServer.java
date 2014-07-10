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
package com.raytheon.uf.edex.registry.ebxml.web;

import java.io.FileInputStream;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.xml.XmlConfiguration;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;
import com.raytheon.uf.edex.security.SecurityConfiguration;

/**
 * 
 * Wrapper for the Registry web server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 6/5/2014     1712        bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistryWebServer implements RegistryInitializedListener {

    /** The logger instance */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryWebServer.class);

    /** The jetty server instance */
    private final Server jettyServer;
   

    /**
     * Creates a new Jetty Server with the given configuration file
     * 
     * @param jettyConfigFile
     *            The Jetty configuration file
     * @throws Exception
     *             If errors occur while configuring the Jetty Server
     */
    public RegistryWebServer(String jettyConfigFile,
            SecurityConfiguration securityConfiguration) throws Exception {
        try {
            statusHandler.info("Configuring registry web server from file ["
                    + jettyConfigFile + "]");
            FileInputStream fis = null;
            try {
                System.getProperties().putAll(
                        securityConfiguration.getSecurityProperties());
                fis = new FileInputStream(jettyConfigFile);
                XmlConfiguration configuration = new XmlConfiguration(fis);
                jettyServer = (Server) configuration.configure();
            } finally {
                if (fis != null) {
                    fis.close();
                }
            }
            statusHandler.info("Registry web server configured!");
            Runtime.getRuntime().addShutdownHook(new Thread() {
                public void run() {
                    statusHandler.info("Stopping Registry web server...");
                    try {
                        if (jettyServer != null && jettyServer.isRunning()) {
                            jettyServer.stop();
                        }
                    } catch (Exception e) {
                        statusHandler.error(
                                "Error shutting down Registry Web Server!", e);
                    }
                    statusHandler.info("Registry web server stopped.");
                }
            });
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error starting registry web server!", e);
        }
    }

    @Override
    public void executeAfterRegistryInit() throws EbxmlRegistryException {
        statusHandler.info("Starting Registry web server...");
        try {
            jettyServer.start();
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error starting Registry web server!", e);
        }
        statusHandler.info("Registry web server started!");
    }

    public Server getJettyServer() {
        return jettyServer;
    }

}
