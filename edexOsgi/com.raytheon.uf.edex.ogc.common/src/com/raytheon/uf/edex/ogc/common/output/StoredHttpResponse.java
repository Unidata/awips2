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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.servlet.http.HttpServletResponse;

/**
 * OGC HTTP response adapter for capturing OGC HTTP responses as byte arrays
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
public class StoredHttpResponse implements IOgcHttpResponse {

    private ByteArrayOutputStream baos;

    private int status = HttpServletResponse.SC_OK;

    private String contentType;

    private String characterEncoding;

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#getOutputStream()
     */
    @Override
    public OutputStream getOutputStream() throws IOException {
        baos = new ByteArrayOutputStream();
        return baos;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#setStatus(int)
     */
    @Override
    public void setStatus(int status) {
        this.status = status;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#setContentType(java.lang.String)
     */
    @Override
    public void setContentType(String type) {
        this.contentType = type;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.output.OgcHttpResponse#setCharacterEncoding(java.lang.String)
     */
    @Override
    public void setCharacterEncoding(String encoding) {
        this.characterEncoding = encoding;
    }

    /**
     * @return the status
     */
    public int getStatus() {
        return status;
    }

    /**
     * @return the contentType
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * @return the characterEncoding
     */
    public String getCharacterEncoding() {
        return characterEncoding;
    }

    /**
     * @return empty byte array if {@link StoredHttpResponse#getOutputStream()}
     *         hasn't been called
     */
    public byte[] getBody() {
        if (baos == null) {
            return new byte[0];
        }
        return baos.toByteArray();
    }
}
