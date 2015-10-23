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
 * Interface for pooling OGC http handlers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2012            bclement     Initial creation
 * Oct 23, 2015  #5004     dgilling     Add types to interface methods.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public interface IOgcHttpPooler {

    public OgcHttpHandler borrowObject(Long key);

    public void returnObject(Long key, OgcHttpHandler borrowed);

    public void drain();

}
