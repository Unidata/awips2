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
import java.io.FileInputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Properties;

/**
 * Global configuration for dataserver
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb  5, 2014 2756       bclement     Initial creation
 * Feb 28, 2014 2756       bclement     added auth cache size
 * Mar 04, 2014 2756       bclement     added xmpp server retry
 * Mar 06, 2014 2756       bclement     added logging level
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Config{

    public static final String HOST_KEY = "host";

    public static final String PORT_KEY = "port";

    public static final int PORT_DEFAULT = 9682;

    public static final String DATAPATH_KEY = "datapath";

    public static final String DATAPATH_DEFAULT = "/session_data/";

    public static final String LOGDIR_KEY = "logdir";

    public static final String LOGDIR_DEFAULT = "logs";

    public static final String LOGNAME_KEY = "logname";

    public static final String LOGNAME_DEFAULT = "serverlog_yyyy_mm_dd.log";

    public static final String STOREDIR_KEY = "storedir";

    public static final String STOREDIR_DEFAULT = "store";

    public static final String LEGACY_MODE_KEY = "legacy.mode";

    public static final boolean LEGACY_MODE_DEFAULT = true;

    public static final String XMPP_USERNAME_KEY = "xmpp.username";

    public static final String XMPP_PASSWORD_KEY = "xmpp.password";

    public static final String XMPP_SERVER_KEY = "xmpp.server";

    public static final String XMPP_SERVER_DEFAULT = "localhost";

    public static final String AUTH_CACHE_SIZE_KEY = "auth.key.cache.size";

    public static final int AUTH_CACHE_SIZE_DEFAULT = 64;

    public static final String XMPP_SERVER_RETRY_PERIOD_KEY = "xmpp.server.retry.period";

    // 5 seconds
    public static final int XMPP_SERVER_RETRY_PERIOD_DEFAULT = 5000;

    public static final String config = System.getProperty(
            "collaboration.dataserver.config", "config/settings.properties");

    public static final String credentialsFile = System.getProperty(
            "collaboration.dataserver.credentials",
            "config/credentials.properties");

    public static final Boolean useStdOut = Boolean
            .getBoolean("collaboration.dataserver.stdout");
    
    public static final String loggingLevel = System.getProperty(
            "dataserver.logging.level", "INFO");

    private static Properties _props = null;

    private static Properties _credProps = null;

    private Config() {
    }

    /**
     * Get cached properties. Loads if not initialized.
     * 
     * @return
     */
    private static synchronized Properties getProperties() {
        if (_props == null) {
            _props = new Properties();
            loadProperties(_props, config, false);
        }

        return _props;
    }
    
    /**
     * Get cached credentials properties. Loads if not initialized.
     * 
     * @return
     */
    private static synchronized Properties getCredProperties()
            throws RuntimeException {
        if (_credProps == null) {
            _credProps = new Properties();
            loadProperties(_credProps, credentialsFile, true);
        }

        return _credProps;
    }

    /**
     * Loads properties from file system. If fatal is true, runtime exception is
     * thrown on error. Otherwise, an error message is logged and defaults are
     * used.
     * 
     * @param props
     * @param propertyFile
     * @param fatal
     */
    private static void loadProperties(Properties props, String propertyFile,
            boolean fatal) throws RuntimeException {
        try {
            props.load(new FileInputStream(new File(propertyFile)));
        } catch (Exception e) {
            System.err.println("Unable to open properties file: "
                    + propertyFile + ". " + e.getLocalizedMessage());
            if (fatal) {
                throw new RuntimeException(
                        "Unable to continue without configuration: "
                                + propertyFile, e);
            } else {
                e.printStackTrace();
                System.err.println("Continuing using defaults");
            }
        }
    }

    /**
     * Get integer value from properties
     * 
     * @param key
     * @param defaultValue
     * @return
     */
    public static int getInt(String key, int defaultValue) {
        Properties props = getProperties();
        String value = props.getProperty(key);
        if (value == null) {
            return defaultValue;
        } else {
            return Integer.parseInt(value);
        }
    }

    /**
     * Get string value from properties
     * 
     * @param key
     * @param defaultValue
     * @return
     */
    public static String getProp(String key, String defaultValue) {
        return getProperties().getProperty(key, defaultValue);
    }

    /**
     * Get boolean value from properties
     * 
     * @param key
     * @param defaultValue
     * @return
     */
    public static boolean getBool(String key, boolean defaultValue) {
        Properties props = getProperties();
        String value = props.getProperty(key);
        if (value == null) {
            return defaultValue;
        } else {
            return Boolean.parseBoolean(value);
        }
    }

    /**
     * @return username for XMPP account
     * @throws RuntimeException
     *             if configuration for username cannot be found
     */
    public static String getXmppUsername() throws RuntimeException {
        return getRequiredCredential(XMPP_USERNAME_KEY);
    }

    /**
     * @return password for XMPP account
     * @throws RuntimeException
     *             if configuration for password cannot be found
     */
    public static String getXmppPassword() throws RuntimeException {
        return getRequiredCredential(XMPP_PASSWORD_KEY);
    }

    /**
     * Get property that represents a web service path. Ensures a slash begins
     * and ends the path.
     * 
     * @param key
     * @param defaultValue
     * @return
     */
    public static String getPath(String key, String defaultValue) {
        String path = getProp(key, defaultValue).trim();
        if (!path.startsWith("/")) {
            path = "/" + path;
        }
        if (!path.endsWith("/")) {
            path = path + "/";
        }
        return path;
    }

    /**
     * Get property from credentials file
     * 
     * @param key
     * @return
     * @throws RuntimeException
     *             if credential configuration cannot be found
     */
    private static String getRequiredCredential(String key)
            throws RuntimeException {
        Properties credProperties = getCredProperties();
        String rval = credProperties.getProperty(key);
        if (rval == null) {
            throw new RuntimeException(
                    "Missing required credentials property: " + key);
        }
        return rval;
    }

    /**
     * Get URL for data service. If host portion of URL is not found in config,
     * this method will attempt to determine the canonical hostname for the
     * machine
     * 
     * @return
     * @throws UnknownHostException
     */
    public static String getDataserverUrl() throws UnknownHostException {
        Properties props = getProperties();
        String host = props.getProperty(HOST_KEY);
        if (host == null) {
            host = InetAddress.getLocalHost().getCanonicalHostName();
        }
        int port = getInt(PORT_KEY, PORT_DEFAULT);
        String datapath = getPath(DATAPATH_KEY, DATAPATH_DEFAULT);
        return "http://" + host + ":" + port + datapath;
    }

}
