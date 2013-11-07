/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.filter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Abstract base for in memory filtering of data records
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractPdoFilter {

    public abstract boolean matches(PluginDataObject pdo);

    public static AbstractPdoFilter noFilter() {
        return new AbstractPdoFilter() {

            @Override
            public boolean matches(PluginDataObject pdo) {
                return true;
            }
        };
    }
}
