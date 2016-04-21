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
import java.io.IOException;
import java.io.PrintStream;
import java.util.TimeZone;

import org.eclipse.jetty.util.RolloverFileOutputStream;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;

import com.raytheon.collaboration.dataserver.auth.ServerAuthManager;

/**
 * Entry class for dataserver
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb  5, 2014  2756      bclement     Initial creation
 * Feb 28, 2014  2756      bclement     added authManager
 * Mar 06, 2014  2756      bclement     added logging level config
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DataserverMain {
    
    private static final int CONNECTION_DELAY = 10000; // 10 seconds

    private static final int SHUTDOWN_DELAY = 1000; // 1 second

    /**
     * @param args
     * @throws IOException
     */
    public static void main(String[] args) throws IOException {
        try {
            configureLogging();
        } catch (IOException e) {
            System.err.println("Unable to configure logging: "
                    + e.getLocalizedMessage());
            System.err
                    .println("Continuing using standard out and standard error");
        }
        final XmppServerConnection xmppConnection;
        final ServerAuthManager authManager;
        try {
            xmppConnection = new XmppServerConnection();
            authManager = new ServerAuthManager(xmppConnection);
        } catch (Exception e) {
            System.err
                    .println("Unable to connect to XMPP server, shutting down");
            e.printStackTrace();
            return;
        }
        final WebServerRunner webServer = new WebServerRunner(authManager);
        new Thread(webServer).start();
        wait(CONNECTION_DELAY);
        new Thread(xmppConnection).start();
        Runtime.getRuntime().addShutdownHook(new Thread() {
            public void run() {
                System.out.println("Server shutting down");
                xmppConnection.disconnect();
                webServer.stop();
                DataserverMain.wait(SHUTDOWN_DELAY);
            }
        });
    }

    /**
     * sleep thread for specified milliseconds
     * 
     * @param millis
     */
    private static void wait(int millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Set up rolling file log unless standard out is to be used
     * 
     * @throws IOException
     */
    private static void configureLogging() throws IOException {
        Logger root = (Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
        root.setLevel(Level.valueOf(Config.loggingLevel));
        if (Config.useStdOut){
            return;
        }
        File logDir = new File(Config.getProp(Config.LOGDIR_KEY, Config.LOGDIR_DEFAULT));
        if ( !logDir.exists()){
            if (!logDir.mkdirs()) {
                throw new IOException(
                        "Unable to create configured logging directory: "
                                + logDir.getAbsolutePath());
            }
        } else if (!logDir.isDirectory()){
            throw new IOException(
                    "Configured logging directory exists but is not a directory: "
                            + logDir.getAbsolutePath());
        }
        String logFileName = logDir.getAbsolutePath() + File.separator
                + Config.getProp(Config.LOGNAME_KEY, Config.LOGNAME_DEFAULT);
        RolloverFileOutputStream out = new RolloverFileOutputStream(
                logFileName, true, 0, TimeZone.getTimeZone("GMT"));
        System.setErr(new PrintStream(out));
        System.setOut(new PrintStream(out));
    }

}
