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
package com.raytheon.collaboration.dataserver;

import java.io.File;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

/**
 * Start and run jetty webserver
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WebServerRunner implements Runnable {

    private Server server;

    /* (non-Javadoc)
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
        server = new Server(Config.getInt(Config.PORT_KEY,
                Config.PORT_DEFAULT));

        ServletContextHandler context = new ServletContextHandler(
                ServletContextHandler.SESSIONS);
        context.setContextPath("/");
        server.setHandler(context);

        File base = new File(Config.getProp(Config.STOREDIR_KEY,
                Config.STOREDIR_DEFAULT));
        if (!base.exists()) {
            base.mkdirs();
        }

        String datapath = Config.getPath(Config.DATAPATH_KEY,
                Config.DATAPATH_DEFAULT);
        context.addServlet(new ServletHolder(new DataService(base)), datapath
                + "*");
        try {
            server.start();
            System.out.println("Server started");
            server.join();
        } catch (Exception e) {
            System.err.println("Unable to start web server");
            e.printStackTrace();
        }
    }

    /**
     * Shutdown web server
     */
    public void stop() {
        try {
            server.stop();
        } catch (Exception e) {
            System.err.println("Unable to stop web server");
            e.printStackTrace();
        }
    }

}
