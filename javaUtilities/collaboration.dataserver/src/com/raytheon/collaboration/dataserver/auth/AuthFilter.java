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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.raytheon.collaboration.dataserver.RestException;
import com.raytheon.uf.common.http.auth.SignatureAuthScheme;
import com.raytheon.uf.common.http.auth.SignedCredential;

/**
 * Servlet filter for authentication and authorization of requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AuthFilter implements Filter {

    private final Map<String, MethodAuthHandler> handlerMap;

    /**
     * If a method handler isn't provided for a method, method requests will be
     * forwarded on to the servlet
     * 
     * @param methodHandlers
     */
    public AuthFilter(List<MethodAuthHandler> methodHandlers) {
        Map<String, MethodAuthHandler> map = new HashMap<String, MethodAuthHandler>(
                methodHandlers.size());
        for (MethodAuthHandler handler : methodHandlers) {
            map.put(handler.getMethod(), handler);
        }
        this.handlerMap = Collections.unmodifiableMap(map);
    }

    /* (non-Javadoc)
     * @see javax.servlet.Filter#destroy()
     */
    @Override
    public void destroy() {
        // do nothing
    }

    /* (non-Javadoc)
     * @see javax.servlet.Filter#doFilter(javax.servlet.ServletRequest, javax.servlet.ServletResponse, javax.servlet.FilterChain)
     */
    @Override
    public void doFilter(ServletRequest req, ServletResponse resp,
            FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpReq = (HttpServletRequest) req;
        MethodAuthHandler handler = handlerMap.get(httpReq.getMethod()
                .toLowerCase());
        if (handler == null || !handler.getAuthManager().isEnabled()) {
            chain.doFilter(req, resp);
            return;
        }
        try {
            SignedCredential credential = parseCredentials(httpReq);
            ParsedUrl urlparts = ParsedUrl.parse(httpReq);
            HttpServletResponse httpResp = (HttpServletResponse) resp;
            handler.authorize(httpReq, httpResp, chain, credential, urlparts);
        }catch(RestException e){
            HttpServletResponse httpResp = (HttpServletResponse) resp;
            httpResp.sendError(e.getCode(), e.getLocalizedMessage());
        }
    }

    /**
     * Parse Authorization header
     * 
     * @param httpReq
     * @return
     * @throws RestException
     *             if header does not exist or is malformed
     */
    private SignedCredential parseCredentials(HttpServletRequest httpReq)
            throws RestException {
        String header = httpReq.getHeader(SignatureAuthScheme.HTTP_AUTH_HEADER);
        if (header == null || header.trim().isEmpty()) {
            throw new RestException(HttpServletResponse.SC_UNAUTHORIZED,
                    "Missing header: " + SignatureAuthScheme.HTTP_AUTH_HEADER);
        }
        SignedCredential credential = SignatureAuthScheme
                .parseAuthHeader(header);
        if (credential == null || credential.getAlgorithm() == null
                || credential.getSignature() == null
                || credential.getUserid() == null) {
            throw new RestException(HttpServletResponse.SC_BAD_REQUEST,
                    "Malformed header: " + SignatureAuthScheme.HTTP_AUTH_HEADER);
        }
        return credential;
    }



    /* (non-Javadoc)
     * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
     */
    @Override
    public void init(FilterConfig arg0) throws ServletException {
        // do nothing
    }

}
