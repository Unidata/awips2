/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

/**
 * Abstract base for parsed functions from OGC filters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class FilterFunction {

    public abstract String toSql();

    public static FilterFunction create(String name, String[] args)
            throws Exception {
        // TODO Time functions are the only functions for now,
        // need to put logic to call appropriate functions, not always time
        return TimeFunction.create(name, args);
    }
}
