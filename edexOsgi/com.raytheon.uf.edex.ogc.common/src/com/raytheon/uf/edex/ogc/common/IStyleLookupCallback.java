/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Callback interface used by stylers to have access to plugin metadata.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface IStyleLookupCallback<R extends PluginDataObject> {

    /**
     * @param layerName
     * @return a sample data record that supports layer
     * @throws OgcException
     */
    public R lookupSample(String layerName) throws OgcException;

    /**
     * Return a list of samples for plugin. One sample per layer advertised by
     * source.
     * 
     * @return
     * @throws OgcException
     */
    public List<R> getAllSamples() throws OgcException;

}
