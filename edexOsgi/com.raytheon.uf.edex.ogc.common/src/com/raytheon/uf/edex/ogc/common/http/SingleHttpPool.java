/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.http;

/**
 * HTTP pooling for handlers that are thread safe and don't require a separate
 * instance per request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2012            bclement     Initial creation
 * Oct 23, 2015  #5004     dgilling     Update function signatures to match 
 *                                      interface.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class SingleHttpPool implements IOgcHttpPooler {

    private final OgcHttpHandler handler;

    /**
     * @param handler
     */
    public SingleHttpPool(OgcHttpHandler handler) {
        this.handler = handler;
    }

    @Override
    public OgcHttpHandler borrowObject(Long key) {
        return handler;
    }

    @Override
    public void returnObject(Long key, OgcHttpHandler borrowed) {
        // nothing to do
    }

    @Override
    public void drain() {
        // nothing to do
    }

}
