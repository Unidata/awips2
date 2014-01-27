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

import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Factory for creating collector addons
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface CollectorAddonFactory<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject> {

    /**
     * @return new instance of collector addon
     */
    public ICollectorAddon<D, L, R> create();

}
