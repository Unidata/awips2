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
package com.raytheon.collaboration.dataserver.auth;

import java.io.IOException;
import java.security.Signature;
import java.security.SignatureException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.util.log.Log;
import org.eclipse.jetty.util.log.Logger;

import com.raytheon.collaboration.dataserver.RestException;
import com.raytheon.uf.common.http.auth.SignatureAuthScheme;
import com.raytheon.uf.common.http.auth.SignedCredential;
import com.raytheon.uf.common.util.StringUtil;

/**
 * Auth handler for HTTP DELETE requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2014            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DeleteAuthHandler extends MethodAuthHandler {

    private final Logger log = Log.getLogger(this.getClass());

    /**
     * @param authManager
     */
    public DeleteAuthHandler(ServerAuthManager authManager) {
        super(authManager, "delete");
    }

    /* (non-Javadoc)
     * @see com.raytheon.collaboration.dataserver.auth.MethodAuthHandler#authorize(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse, javax.servlet.FilterChain, com.raytheon.uf.common.http.auth.SignedCredential, com.raytheon.collaboration.dataserver.auth.ParsedUrl)
     */
    @Override
    public void authorize(HttpServletRequest httpReq,
            HttpServletResponse httpResp, FilterChain chain,
            SignedCredential credential, ParsedUrl urlParts)
            throws RestException, ServletException, IOException {
        String userid = urlParts.getUserid();
        Signature sig;
        if (StringUtil.isEmptyString(userid)) {
            // deleting entire session, verify that user is owner
            sig = authManager.getAuthorizedSignatureDirect(credential,
                    urlParts.getSessionid());
        } else {
            // verify that user owns resource to delete
            if (!userid.equals(credential.getUserid())) {
                throw new RestException(HttpServletResponse.SC_FORBIDDEN,
                        "Credential does not own resource");
            }
            sig = authManager.getAuthorizedSignature(credential);
        }
        try {
            if (verify(sig, credential, urlParts)) {
                chain.doFilter(httpReq, httpResp);
            } else {
                // possible that we have an outdated public key cached
                sig = authManager.getAuthorizedSignatureDirect(credential);
                if (verify(sig, credential, urlParts)) {
                    chain.doFilter(httpReq, httpResp);
                } else {
                    throw new RestException(HttpServletResponse.SC_FORBIDDEN,
                            "Credentials not authenticated");
                }
            }
        } catch (SignatureException e) {
            log.warn("Problem using signature object", e);
            throw new RestException(
                    HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error using authentication server");
        }
    }

    /**
     * @param sig
     * @param credential
     * @param urlParts
     * @return true if signature matches credentials
     * @throws SignatureException
     */
    private boolean verify(Signature sig, SignedCredential credential,
            ParsedUrl urlParts) throws SignatureException {
        synchronized (sig) {
            sig.update(urlParts.getHostString().getBytes());
            sig.update(urlParts.getPathString().getBytes());
            byte[] sigBytes = SignatureAuthScheme.base64Decode(credential
                    .getSignature());
            return sig.verify(sigBytes);
        }
    }

}
