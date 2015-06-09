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

import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.apache.cxf.helpers.IOUtils;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.xml.XmlConfiguration;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
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
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistryWebServer implements RegistryInitializedListener {

    /** The logger instance */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryWebServer.class);

    /** File name filter used for getting jars from the plugin directory */
    private final FilenameFilter JAR_FILTER = new FilenameFilter() {

        private static final String JAR_SUFFIX = ".jar";

        @Override
        public boolean accept(File dir, String name) {
            return name.endsWith(JAR_SUFFIX);
        }
    };

    /**
     * The directory in which the web contributions are contained in each plugin
     */
    private static final String WEB_DIR_PATTERN = "res/web/registry/";

    /** The base directory of the EBXML web server */
    private static final String WEB_SERVER_BASE_DIR = System
            .getProperty("ebxml.registry.webserver.home");

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
            getPluginWebContributions();
            jettyServer.start();
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error starting Registry web server!", e);
        }
        statusHandler.info("Registry web server started!");
    }

    /**
     * Scans the jars in the plugin directory in search of files to contribute
     * to the registry web server. This method allows non-ebxml plugins to
     * contribute items, web pages, javascript, etc. to the webserver
     * 
     * @throws IOException
     *             If errors occur with IO to and from the jars
     */
    private void getPluginWebContributions() throws IOException {
        statusHandler
                .info("Searching for plugin contributions to EBXML web server...");

        // Get the plugin jars from the plugin directory
        File[] jars = new File(EDEXUtil.getEdexPlugins()).listFiles(JAR_FILTER);

        // Iterate over the jars
        for (File p : jars) {
            try (JarFile jar = new JarFile(p);) {
                Enumeration<JarEntry> entries = jar.entries();

                /*
                 * Iterate over the entries in the jar and look for the
                 * WEB_DIR_PATTERN directory which holds any contributions to
                 * the web server
                 */
                while (entries.hasMoreElements()) {
                    JarEntry e = entries.nextElement();
                    String sourceName = e.getName();
                    if (sourceName.startsWith(WEB_DIR_PATTERN)) {
                        // Remove the search pattern
                        String resourceWithoutPath = sourceName
                                .substring(WEB_DIR_PATTERN.length());
                        if (!resourceWithoutPath.isEmpty()) {
                            // Create the destination file object
                            File destination = new File(WEB_SERVER_BASE_DIR
                                    + resourceWithoutPath);

                            /*
                             * If the destination does not exist, create the
                             * directory or extract the file from the jar and
                             * copy
                             */
                            if (!destination.exists()) {

                                // This is a directory. Create the directory in
                                // the destination location
                                if (sourceName.endsWith(File.separator)) {
                                    if (!destination.exists()) {
                                        statusHandler
                                                .info("Creating directory: "
                                                        + destination.getPath());
                                        destination.mkdir();
                                    }
                                }
                                // This is a file. Extract from the jar and
                                // write to the destination location
                                else {
                                    InputStream inStream = null;
                                    try {
                                        statusHandler.info("Creating file: "
                                                + destination);
                                        inStream = jar.getInputStream(e);
                                        IOUtils.transferTo(inStream,
                                                destination);
                                    } finally {
                                        if (inStream != null) {
                                            inStream.close();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

        }
    }

    public Server getJettyServer() {
        return jettyServer;
    }

}
