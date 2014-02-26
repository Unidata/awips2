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

/**
 * Auth handler for HTTP PUT requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PutAuthHandler extends MethodAuthHandler {

    private final Logger log = Log.getLogger(this.getClass());

    /**
     * @param method
     */
    public PutAuthHandler(ServerAuthManager authManager) {
        super(authManager, "put");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.collaboration.dataserver.auth.MethodAuthHandler#authorize
     * (javax.servlet.http.HttpServletRequest,
     * javax.servlet.http.HttpServletResponse, javax.servlet.FilterChain,
     * com.raytheon.uf.common.http.auth.SignedCredential,
     * com.raytheon.collaboration.dataserver.auth.ParsedUrl)
     */
    @Override
    public void authorize(HttpServletRequest httpReq,
            HttpServletResponse httpResp, FilterChain chain,
            SignedCredential credential, ParsedUrl urlParts)
            throws RestException, ServletException {
        if (!urlParts.getUserid().equals(credential.getUserid())) {
            throw new RestException(HttpServletResponse.SC_FORBIDDEN,
                    "Credential does not own resource");
        }
        Signature sig = authManager.getAuthorizedSignature(credential);
        try {
            VerifyingRequest wrapper = new VerifyingRequest(httpReq);
            if (verify(sig, wrapper, credential, urlParts)) {
                chain.doFilter(wrapper, httpResp);
            } else {
                // possible that we have an outdated public key cached
                sig = authManager.getAuthorizedSignatureDirect(credential);
                if (verify(sig, wrapper, credential, urlParts)) {
                    chain.doFilter(wrapper, httpResp);
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
        } catch (IOException e) {
            log.warn("Problem transferring stream", e);
            throw new RestException(
                    HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Problem reading from stream");
        }
    }

    /**
     * @param sig
     * @param wrapper
     * @param credential
     * @param urlParts
     * @return true if signature matches the credentials
     * @throws SignatureException
     * @throws IOException
     */
    private boolean verify(Signature sig, VerifyingRequest wrapper,
            SignedCredential credential, ParsedUrl urlParts)
            throws SignatureException, IOException {
        synchronized (sig) {
            sig.update(urlParts.getHostString().getBytes());
            sig.update(urlParts.getPathString().getBytes());
            byte[] sigBytes = SignatureAuthScheme.base64Decode(credential
                    .getSignature());
            return wrapper.verify(sig, sigBytes);
        }
    }

}
