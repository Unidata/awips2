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

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.raytheon.collaboration.dataserver.RestException;
import com.raytheon.uf.common.http.auth.SignedCredential;

/**
 * Base auth handler for HTTP methods
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
public abstract class MethodAuthHandler {

    protected final String method;
    
    protected final ServerAuthManager authManager;


    /**
     * @param authManager
     * @param method
     */
    public MethodAuthHandler(ServerAuthManager authManager, String method) {
        this.authManager = authManager;
        this.method = method;
    }
    
    /**
     * Authorize request using provided credentials. Sends the request down the
     * FilterChain if authorized.
     * 
     * @param httpReq
     * @param httpResp
     * @param chain
     * @param credential
     * @param urlParts
     * @throws RestException
     *             if request is not authorized or invalid
     * @throws ServletException
     * @throws IOException
     */
    abstract public void authorize(HttpServletRequest httpReq,
            HttpServletResponse httpResp, FilterChain chain,
            SignedCredential credential, ParsedUrl urlParts)
            throws RestException, ServletException, IOException;

    /**
     * @return the method
     */
    public String getMethod() {
        return method;
    }

    /**
     * @return the authManager
     */
    public ServerAuthManager getAuthManager() {
        return authManager;
    }

}
