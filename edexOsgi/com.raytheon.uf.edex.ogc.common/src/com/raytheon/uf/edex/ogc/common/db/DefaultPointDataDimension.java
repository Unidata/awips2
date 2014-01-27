/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.db;

import java.util.HashSet;
import java.util.Set;

/**
 * Default dimension object for point data that has no extra dimension
 * information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DefaultPointDataDimension extends SimpleDimension {

    private static final long serialVersionUID = 2947254963150347405L;

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getValues() {
        return new HashSet<String>(0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDefaultValue(SimpleLayer<? extends SimpleDimension> layer) {
        return null;
    }

}
