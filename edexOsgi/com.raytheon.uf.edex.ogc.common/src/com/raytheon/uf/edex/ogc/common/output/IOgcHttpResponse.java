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

/**
 * Interface for OGC HTTP output
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
public interface IOgcHttpResponse {

    /**
     * This can only be called once
     * 
     * @return
     * @throws IOException
     */
    public OutputStream getOutputStream() throws IOException;

    /**
     * @param status
     */
    public void setStatus(int status);

    /**
     * @param type
     */
    public void setContentType(String type);

    /**
     * @param encoding
     */
    public void setCharacterEncoding(String encoding);
}
