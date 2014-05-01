/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.request;

import java.util.ArrayList;
import java.util.List;

import net.opengis.wfs.v_2_0_0.DescribeStoredQueriesType;

/**
 * Request wrapper for WFS describe stored query request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 8, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DescQueryReq extends WfsRequest {

    private final List<String> ids;

    /**
     * @param type
     */
    public DescQueryReq(List<String> ids) {
        super(Type.DescribeStoredQueries);
        this.ids = ids;
    }

    public DescQueryReq(DescribeStoredQueriesType req) {
        super(Type.DescribeStoredQueries);
        if (req.isSetStoredQueryId()) {
            this.ids = req.getStoredQueryId();
        } else {
            this.ids = new ArrayList<String>(0);
        }
    }

    /**
     * @return the ids
     */
    public List<String> getIds() {
        return ids;
    }

}
