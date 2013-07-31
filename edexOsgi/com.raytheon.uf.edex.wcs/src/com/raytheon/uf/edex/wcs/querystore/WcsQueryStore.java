/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs.querystore;

import java.io.File;

import javax.xml.bind.JAXBException;

import net.opengis.wcs.v_1_1_2.GetCoverage;

import com.raytheon.uf.edex.ogc.common.AbstractFSQueryStore;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class WcsQueryStore extends AbstractFSQueryStore<GetCoverage> {

    private static final String STORE_NAME = "wcsquerystore";

    protected final OgcJaxbManager jaxbManager;

    /**
     * @param registry
     * @param storeLocation
     * @throws IllegalArgumentException
     *             if storage location cannot be created or is not a writable
     *             directory
     */
    public WcsQueryStore(OgcJaxbManager jaxbManager, File storeLocation)
            throws IllegalArgumentException {
        super(storeLocation);
        this.jaxbManager = jaxbManager;
    }

    public WcsQueryStore(OgcJaxbManager jaxbManager) {
        super(findStore(STORE_NAME));
        this.jaxbManager = jaxbManager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.AbstractFSQueryStore#marshal(java.lang
     * .Object)
     */
    @Override
    protected String marshal(GetCoverage query) throws OgcException {
        try {
            return jaxbManager.marshal(query);
        } catch (JAXBException e) {
            throw new OgcException(Code.InternalServerError, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.AbstractFSQueryStore#unmarshal(java.lang
     * .String)
     */
    @Override
    protected GetCoverage unmarshal(String xml) throws OgcException {
        try {
            return (GetCoverage) jaxbManager.unmarshal(xml);
        } catch (JAXBException e) {
            throw new OgcException(Code.InternalServerError, e);
        }
    }

}
