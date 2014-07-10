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
package com.raytheon.uf.edex.security;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.KeyStore;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.TrustManagerFactory;

import org.apache.cxf.configuration.jsse.TLSClientParameters;
import org.apache.cxf.configuration.security.AuthorizationPolicy;

import com.raytheon.uf.edex.core.modes.EDEXModesUtil;

/**
 * 
 * Object containing the security configuration items.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 6/5/2014     1712        bphillip    Initial Creation
 * 7/10/2014    1717        bphillip    Added authorization policy
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class SecurityConfiguration {

    /** The directory containing security related files such as keystores */
    private static final String SECURITY_DIR = EDEXModesUtil.CONF_DIR
            + File.separator + "resources/site" + File.separator
            + System.getenv("AW_SITE_IDENTIFIER") + File.separator;

    /** The properties file containing the security configuration items */
    private static final String SECURITY_PROPERTIES_FILE = SECURITY_DIR
            + "security.properties";

    /** Properties object for the security configuration */
    private EncryptedProperties securityProperties;

    /** The https configuration */
    private TLSClientParameters tlsParams;

    /** The authorization policy */
    private AuthorizationPolicy authPolicy;

    /** Keystore factory */
    private KeyManagerFactory kmf;

    /** Trust store factory */
    private TrustManagerFactory tmf;

    /**
     * Creates and initializes a new Security configuration object based on the
     * security properties specified
     * 
     * @throws IOException
     */
    public SecurityConfiguration() throws IOException {
        securityProperties = new EncryptedProperties(SECURITY_PROPERTIES_FILE);
        initKeyStore();
        initTrustStore();
        initTLSParams();
        initAuthPolicy();
    }

    /**
     * Initializes the authorization policy
     */
    private void initAuthPolicy() {
        authPolicy = new AuthorizationPolicy();
        String user = getProperty("edex.security.auth.user");
        authPolicy.setUserName(user);
        authPolicy.setPassword(getProperty("edex.security.auth.password"));
        authPolicy
                .setAuthorizationType(getProperty("edex.security.auth.authorizationType"));
    }

    /**
     * Initializes the TLS parameters
     */
    private void initTLSParams() {
        tlsParams = new TLSClientParameters();
        tlsParams.setKeyManagers(kmf.getKeyManagers());
        tlsParams.setTrustManagers(tmf.getTrustManagers());
        tlsParams.setDisableCNCheck(Boolean
                .parseBoolean(getProperty("edex.security.disableCNCheck")));
    }

    /**
     * Initializes the keystore
     */
    private void initKeyStore() {
        FileInputStream fis = null;
        KeyStore keystore = null;
        char[] storepass = getProperty("edex.security.keystore.password")
                .toCharArray();

        try {
            kmf = KeyManagerFactory
                    .getInstance(getProperty("edex.security.keystore.algorithm"));
            fis = new FileInputStream(
                    getProperty("edex.security.keystore.path"));
            keystore = KeyStore
                    .getInstance(getProperty("edex.security.keystore.type"));
            keystore.load(fis, storepass);
            kmf.init(keystore, storepass);
        } catch (Exception e) {
            throw new SecurityException("Error initializing keystore", e);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                    throw new RuntimeException(
                            "Error closing file input stream!", e);
                }
            }
        }
    }

    /**
     * Initializes the trust store
     */
    private void initTrustStore() {
        FileInputStream fis = null;
        KeyStore truststore = null;
        char[] storepass = getProperty("edex.security.truststore.password")
                .toCharArray();

        try {
            tmf = TrustManagerFactory
                    .getInstance(getProperty("edex.security.truststore.algorithm"));
            fis = new FileInputStream(
                    getProperty("edex.security.truststore.path"));
            truststore = KeyStore
                    .getInstance(getProperty("edex.security.truststore.type"));
            truststore.load(fis, storepass);
            tmf.init(truststore);
        } catch (Exception e) {
            throw new SecurityException("Error initializing truststore", e);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                    throw new RuntimeException(
                            "Error closing file input stream!", e);
                }
            }
        }
    }

    public String getEncryptionKey() {
        return getProperty("edex.security.encryption.key");
    }

    /**
     * Gets a security property.
     * 
     * @param propertyName
     *            The name of the property to get
     * @return The property value
     */
    public String getProperty(String propertyName) {
        String prop = securityProperties.getProperty(propertyName);
        if (prop == null || prop.trim().isEmpty()) {
            throw new SecurityException("Required property not set: "
                    + propertyName);
        }
        return prop;
    }

    /**
     * Gets the TLSClientParameters
     * 
     * @return The TLSClientParameters
     */
    public TLSClientParameters getTlsParams() {
        return tlsParams;
    }

    public EncryptedProperties getSecurityProperties() {
        return securityProperties;
    }

    /**
     * @return the authPolicy
     */
    public AuthorizationPolicy getAuthPolicy() {
        return authPolicy;
    }

}
