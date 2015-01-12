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

package com.raytheon.uf.edex.registry.ebxml.acp;

import java.io.IOException;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.UnsupportedCallbackException;

import org.apache.ws.security.WSPasswordCallback;

import com.raytheon.uf.edex.registry.ebxml.web.security.CredentialCache;

/**
 * 
 * Password callback class used with WS security.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2012            bphillip     Initial creation
 * 7/10/2014    1717       bphillip    Get user information from CredentialCache
 * 7/24/2014    1712       bphillip    Spring injection of CredentialCache
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class PasswordCallback implements CallbackHandler {
    
    /** Cache of user credentials */
    private CredentialCache credentialCache;

    /**
     * Creates a new PasswordCallback
     */
    public PasswordCallback() {
    }

    @Override
    public void handle(Callback[] callbacks) throws IOException,
            UnsupportedCallbackException {
        for (Callback call : callbacks) {
            if (call instanceof WSPasswordCallback) {
                WSPasswordCallback cb = (WSPasswordCallback) call;
                cb.setPassword(credentialCache.getUserPassword(
                        ((WSPasswordCallback) call).getIdentifier()));
            }
        }
    }

    /**
     * @param credentialCache the credentialCache to set
     */
    public void setCredentialCache(CredentialCache credentialCache) {
        this.credentialCache = credentialCache;
    }
    
    
}
