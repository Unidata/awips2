/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.output;

import java.io.IOException;
import java.io.OutputStream;

import javax.servlet.http.HttpServletResponse;

/**
 * OGC HTTP response adapter for standard servlet response object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ServletOgcResponse implements IOgcHttpResponse {

    private final HttpServletResponse response;

    /**
     * 
     */
    public ServletOgcResponse(HttpServletResponse response) {
        this.response = response;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#getOutputStream()
     */
    @Override
    public OutputStream getOutputStream() throws IOException {
        return response.getOutputStream();
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#setStatus(int)
     */
    @Override
    public void setStatus(int status) {
        response.setStatus(status);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#setContentType(java.lang.String)
     */
    @Override
    public void setContentType(String type) {
        response.setContentType(type);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#setCharacterEncoding(java.lang.String)
     */
    @Override
    public void setCharacterEncoding(String encoding) {
        response.setCharacterEncoding(encoding);
    }

}
