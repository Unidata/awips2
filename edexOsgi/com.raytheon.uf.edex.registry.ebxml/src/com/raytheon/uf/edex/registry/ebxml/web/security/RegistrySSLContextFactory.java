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

import java.io.IOException;

import org.eclipse.jetty.util.ssl.SslContextFactory;

import com.raytheon.uf.common.security.encryption.AESEncryptor;
import com.raytheon.uf.edex.security.SecurityConfiguration;

/**
 * 
 * Custom SslContextFacotry implementation which accepts encrypted values for passwords
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/10/2014    1717        bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistrySSLContextFactory extends SslContextFactory {

    private AESEncryptor encryption;

    private SecurityConfiguration securityConfiguration;

    public RegistrySSLContextFactory() throws IOException {
        super();
        this.securityConfiguration = new SecurityConfiguration();
        this.encryption = new AESEncryptor();
    }

    @Override
    public void setKeyStorePassword(String password) {
        try {
            super.setKeyStorePassword(encryption.decrypt(this.securityConfiguration.getEncryptionKey(),
                    password));
        } catch (Exception e) {
            throw new RuntimeException("Error setting web server properties!",
                    e);
        }
    }

    @Override
    public void setTrustStorePassword(String password) {
        try {
            super.setTrustStorePassword(encryption.decrypt(this.securityConfiguration.getEncryptionKey(),
                    password));
        } catch (Exception e) {
            throw new RuntimeException("Error setting web server properties!",
                    e);
        }
    }

    public void setKeyManagerPassword(String password) {
        try {
            super.setKeyManagerPassword(encryption.decrypt(this.securityConfiguration.getEncryptionKey(),
                    password));
        } catch (Exception e) {
            throw new RuntimeException("Error setting web server properties!",
                    e);
        }
    }

}
