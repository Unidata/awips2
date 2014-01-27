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

import javax.servlet.Servlet;

import org.eclipse.jetty.security.ConstraintMapping;
import org.eclipse.jetty.security.ConstraintSecurityHandler;
import org.eclipse.jetty.security.HashLoginService;
import org.eclipse.jetty.security.SecurityHandler;
import org.eclipse.jetty.security.authentication.BasicAuthenticator;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.server.nio.SelectChannelConnector;
import org.eclipse.jetty.server.ssl.SslSocketConnector;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.servlets.ProxyServlet;
import org.eclipse.jetty.util.security.Constraint;
import org.eclipse.jetty.util.security.Credential;

/**
 * This class sets up a Jetty Server that can serve up Proxied HttpServlets in
 * either http or https mode.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2013            dhladky    Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ProxiedJettyServer {
    /** The https port number */
    private int httpsport = 8888;

    /** HTTP port number */
    private int httpport = 8085;

    /** The realm string **/
    private final String realm;

    /** Jetty server instance */
    private Server server;

    /** user for checks **/
    private String user = "user";

    /** password for checks **/
    private String password = "password";

    private final ServletContextHandler servletContextHandler;

    /**
     * Start the Proxied servlet
     * 
     * @param redirectHost
     * @param port
     * @param context
     * @param realm
     * @param user
     * @param password
     */
    public ProxiedJettyServer(String redirectUrl, Integer httpport,
            Integer httpsport, String context, String realm, String user,
            String password) {
        servletContextHandler = new ServletContextHandler(
                ServletContextHandler.SESSIONS);
        servletContextHandler.setContextPath("/");
        ProxyServlet.Transparent servlet = new ProxyServlet.Transparent();
        ServletHolder servletHolder = new ServletHolder(servlet);
        servletHolder.setInitParameter("ProxyTo", redirectUrl);
        servletHolder.setInitParameter("Prefix", "/" + context);
        addServlet(servletHolder, "/" + context + "/*");
        
        this.httpport = httpport;
        this.httpsport = httpsport;
        this.realm = realm;
        this.user = user;
        this.password = password;

        server = new Server();

    }

    /**
     * Start the server as an SSL server
     * 
     * @throws Exception
     */
    public void startSSLServer() throws Exception {
        String keystorePath = TestUtil.getFileForResource(JettyServer.class,
                "/http/keystore").getAbsolutePath();

        SslSocketConnector sslConnector = new SslSocketConnector();
        sslConnector.setPort(httpsport);
        sslConnector.setMaxIdleTime(30000);
        sslConnector.setKeystore(keystorePath);
        sslConnector.setPassword(password);
        sslConnector.setKeyPassword(password);
        sslConnector.setTruststore(keystorePath);
        sslConnector.setTrustPassword(password);

        server.addConnector(sslConnector);

        servletContextHandler.setSecurityHandler(basicAuth(user, password,
                realm));
        server.setHandler(servletContextHandler);

        server.start();
    }

    /**
     * Start the server as a standard http server
     * 
     * @throws Exception
     */
    public void startServer() throws Exception {
        server = new Server(httpsport);

        server.setHandler(servletContextHandler);

        server.start();

    }

    public void stopServer() throws Exception {
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
    public void addServlet(Servlet servlet, String pathSpec) {
        addServlet(new ServletHolder(servlet), pathSpec);
    }

    public void addServlet(ServletHolder holder, String pathSpec) {
        servletContextHandler.addServlet(holder, pathSpec);
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
                new String[] { user });
        l.setName(realm);

        Constraint constraint = new Constraint();
        constraint.setName(Constraint.__BASIC_AUTH);
        constraint.setRoles(new String[] { user });
        constraint.setAuthenticate(true);

        ConstraintMapping cm = new ConstraintMapping();
        cm.setConstraint(constraint);
        cm.setPathSpec("/*");

        ConstraintSecurityHandler csh = new ConstraintSecurityHandler();
        csh.setAuthenticator(new BasicAuthenticator());
        csh.setRealmName(realm);
        csh.addConstraintMapping(cm);
        csh.setLoginService(l);

        return csh;
    }

    private void addConnector(SelectChannelConnector connector) {
        server.addConnector(connector);
    }

    private void setHandler(HandlerCollection handlers) {
        server.setHandler(handlers);
    }

    // I kept this because I use it
    public static void main(String[] args) throws Exception {

        String redirectHost = args[0];
        Integer port = Integer.parseInt(args[1]);
        Integer sport = Integer.parseInt(args[2]);
        String context = args[3];
        String realm = args[4];
        String user = args[5];
        String password = args[6];

        // Run this server
        ProxiedJettyServer server = new ProxiedJettyServer(redirectHost, port,
                sport, context, realm, user, password);
        server.startSSLServer();
    }
}