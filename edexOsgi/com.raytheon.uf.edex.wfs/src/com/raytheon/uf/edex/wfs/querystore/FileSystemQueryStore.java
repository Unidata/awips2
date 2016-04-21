/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.querystore;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import net.opengis.wfs.v_2_0_0.CreateStoredQueryType;
import net.opengis.wfs.v_2_0_0.ObjectFactory;
import net.opengis.wfs.v_2_0_0.QueryType;
import net.opengis.wfs.v_2_0_0.StoredQueryDescriptionType;

import com.raytheon.uf.edex.ogc.common.AbstractFSQueryStore;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;

/**
 * Caching File System query store implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FileSystemQueryStore extends
        AbstractFSQueryStore<StoredQueryDescriptionType> implements IQueryStore {

    private static final String STORE_NAME = "wfsquerystore";

    protected final WfsRegistryImpl registry;

    protected final StoredQueryResolver resolver;

    private static final ObjectFactory wfsFactory = new ObjectFactory();

    /**
     * @param registry
     * @param storeLocation
     * @throws IllegalArgumentException
     *             if storage location cannot be created or is not a writable
     *             directory
     */
    public FileSystemQueryStore(WfsRegistryImpl registry, File storeLocation)
            throws IllegalArgumentException {
        super(storeLocation);
        this.registry = registry;
        this.resolver = new StoredQueryResolver(registry);
    }

    /**
     * Create a store in a directory named storeName in the default location
     * 
     * @param registry
     * @param storeName
     * @throws IllegalArgumentException
     */
    public FileSystemQueryStore(WfsRegistryImpl registry, String storeName)
            throws IllegalArgumentException {
        this(registry, findStore(storeName));
    }

    /**
     * @param registry
     */
    public FileSystemQueryStore(WfsRegistryImpl registry) {
        this(registry, STORE_NAME);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.querystore.QueryStore#resolve(java.lang.String,
     * java.util.Map)
     */
    public List<QueryType> resolve(String id, Map<String, String> parameters)
            throws OgcException {
        String desc = retrieveString(id);
        if (desc == null) {
            return new ArrayList<QueryType>(0);
        }
        return resolver.resolve(desc, parameters);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.querystore.QueryStore#getResolver()
     */
    @Override
    public StoredQueryResolver getResolver() {
        return resolver;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.AbstractFSQueryStore#marshal(java.lang
     * .Object)
     */
    @Override
    protected String marshal(StoredQueryDescriptionType query)
            throws OgcException {
        CreateStoredQueryType holder = new CreateStoredQueryType();
        holder.setStoredQueryDefinition(Arrays.asList(query));
        JAXBElement<CreateStoredQueryType> elem = wfsFactory
                .createCreateStoredQuery(holder);
        try {
            return registry.marshal(elem);
        } catch (JAXBException e) {
            throw new OgcException(Code.InternalServerError, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.querystore.QueryStore#store(net.opengis.wfs.
     * v_2_0_0.StoredQueryDescriptionType)
     */
    @Override
    public void store(StoredQueryDescriptionType query) throws OgcException {
        store(query.getId(), query);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.AbstractFSQueryStore#unmarshal(java.lang
     * .String)
     */
    @Override
    protected StoredQueryDescriptionType unmarshal(String xml)
            throws OgcException {
        try {
            CreateStoredQueryType holder = (CreateStoredQueryType) registry
                    .unmarshal(xml);
            return holder.getStoredQueryDefinition().get(0);
        } catch (JAXBException e) {
            throw new OgcException(Code.InternalServerError, e);
        }
    }

}
