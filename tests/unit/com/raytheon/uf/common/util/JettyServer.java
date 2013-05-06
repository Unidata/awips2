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
package com.raytheon.uf.common.util;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.http.security.Constraint;
import org.eclipse.jetty.http.security.Credential;
import org.eclipse.jetty.security.ConstraintMapping;
import org.eclipse.jetty.security.ConstraintSecurityHandler;
import org.eclipse.jetty.security.HashLoginService;
import org.eclipse.jetty.security.SecurityHandler;
import org.eclipse.jetty.security.authentication.BasicAuthenticator;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.bio.SocketConnector;
import org.eclipse.jetty.server.ssl.SslSocketConnector;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

import com.raytheon.uf.common.comm.HttpTestConstants;

/**
 * This class sets up a Jetty Server that can serve up HttpServlets in either
 * http or https mode. When in https mode the authentication credentials are
 * username = user and password = password as defined in TestHttpConstants. This
 * server can be run as part of a unit test or stand alone from the main.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2013   1786     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class JettyServer {
    /** The port number */
    private int port = HttpTestConstants.PORT;

    /** Jetty server instance */
    private Server server;

    private final ServletContextHandler servletContextHandler;

    /**
     * Construct the Jetty Server instance.
     * 
     * @param port
     *            The port number
     */
    public JettyServer(int port) {
        this.port = port;

        servletContextHandler = new ServletContextHandler(
                ServletContextHandler.SESSIONS);
        servletContextHandler.setContextPath("/");
    }

    /**
     * Start the server as an SSL server not requiring authentication.
     * 
     * @throws Exception
     *             if error occurs
     */
    public void startSSLServer() throws Exception {
        startSSLServer(false);
    }

    /**
     * Start the server as an SSL server
     * 
     * @param useAuthentication
     *            true to use authentication. user/password
     * 
     * @throws Exception
     *             if error occurs
     */
    public void startSSLServer(boolean useAuthentication) throws Exception {
        String keystorePath = TestUtil.getFileForResource(JettyServer.class,
                "/http/keystore").getAbsolutePath();

        server = new Server();

        SslSocketConnector sslConnector = new SslSocketConnector();
        sslConnector.setPort(port);
        sslConnector.setMaxIdleTime(30000);
        sslConnector.setKeystore(keystorePath);
        sslConnector.setPassword(HttpTestConstants.PASSWD);
        sslConnector.setKeyPassword(HttpTestConstants.PASSWD);
        sslConnector.setTruststore(keystorePath);
        sslConnector.setTrustPassword(HttpTestConstants.PASSWD);

        server.addConnector(sslConnector);

        if (useAuthentication) {
            servletContextHandler.setSecurityHandler(basicAuth(
                    HttpTestConstants.USERNAME, HttpTestConstants.PASSWD,
                    HttpTestConstants.REALM));
        }
        server.setHandler(servletContextHandler);

        server.start();
    }

    /**
     * Start the server as a standard http server
     * 
     * @throws Exception
     *             exception
     */
    public void startServer() throws Exception {
        server = new Server();
        Connector connector = new SocketConnector();
        connector.setPort(port);
        server.setConnectors(new Connector[] { connector });
        server.addConnector(connector);
        server.setHandler(servletContextHandler);
        server.start();
    }

    /**
     * Stop the server.
     * 
     * @throws Exception
     *             If error occurs
     */
    public void stopServer() throws Exception {
        server.setStopAtShutdown(true);
        server.setGracefulShutdown(1000);
        server.stop();
    }

    /**
     * Add a servlet to the server.
     * 
     * @param servlet
     *            The servlet to add
     * @param pathSpec
     *            The path of the servlet
     */
    public void addServlet(HttpServlet servlet, String pathSpec) {
        servletContextHandler.addServlet(new ServletHolder(servlet), pathSpec);
    }

    /**
     * Create a SecurityHandler for the SSL server.
     * 
     * @param username
     *            The username
     * @param password
     *            The password
     * @param realm
     *            The realm name
     * @return The SecurityHandler
     */
    private final SecurityHandler basicAuth(String username, String password,
            String realm) {

        HashLoginService l = new HashLoginService();
        l.putUser(username, Credential.getCredential(password),
                new String[] { HttpTestConstants.USERNAME });
        l.setName(realm);

        Constraint constraint = new Constraint();
        constraint.setName(Constraint.__BASIC_AUTH);
        constraint.setRoles(new String[] { HttpTestConstants.USERNAME });
        constraint.setAuthenticate(true);

        ConstraintMapping cm = new ConstraintMapping();
        cm.setConstraint(constraint);
        cm.setPathSpec("/*");

        ConstraintSecurityHandler csh = new ConstraintSecurityHandler();
        csh.setAuthenticator(new BasicAuthenticator());
        // csh.setRealmName("myrealm");
        csh.setRealmName(HttpTestConstants.REALM);
        csh.addConstraintMapping(cm);
        csh.setLoginService(l);

        return csh;
    }

    /**
     * A test servlet
     */
    public class TestServletInstance extends HttpServlet {

        @Override
        protected void doGet(HttpServletRequest req, HttpServletResponse resp)
                throws ServletException, IOException {
            resp.getWriter().print("<h1>Test Successful</h1>");
        }
    }

    /**
     * Main method. Uncomment to run in the desired mode.
     * 
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        // Https
        JettyServer server = new JettyServer(8443);
        server.addServlet(server.new TestServletInstance(), "/test");
        server.startSSLServer();

        // Http
        // JettyServer server = new JettyServer(8888);
        // server.addServlet(server.new TestServletInstance(), "/test");
        // server.startServer();
    }
}
