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

import java.util.List;

import com.raytheon.uf.edex.ogc.common.OgcException;

/**
 * Storage interface for layers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface ILayerStore {

    /**
     * Retrieve layer from store
     * 
     * @param id
     * @param c
     * @return null if layer is not in store
     * @throws OgcException
     */
    public <D extends SimpleDimension, L extends SimpleLayer<D>> L get(
            String id, Class<L> c) throws OgcException;

    /**
     * Retrieve all layers for class
     * 
     * @param c
     * @return
     * @throws OgcException
     */
    public <D extends SimpleDimension, L extends SimpleLayer<D>> List<L> getAll(
            Class<L> c) throws OgcException;

    /**
     * Persist layer
     * 
     * @param layer
     * @throws OgcException
     */
    public void createOrUpdate(SimpleLayer<? extends SimpleDimension> layer)
            throws OgcException;

    /**
     * Persist layers
     * 
     * @param layers
     * @throws OgcException
     */
    public void createOrUpdate(
            List<SimpleLayer<? extends SimpleDimension>> layers)
            throws OgcException;

    /**
     * Remove layer with id from store
     * 
     * @param id
     * @param c
     * @throws OgcException
     */
    public void delete(String id, Class<? extends SimpleLayer<?>> c)
            throws OgcException;

    /**
     * Delete all layers with class c from store
     * 
     * @param c
     * @throws OgcException
     */
    public void deleteAll(Class<? extends SimpleLayer<?>> c)
            throws OgcException;

}
