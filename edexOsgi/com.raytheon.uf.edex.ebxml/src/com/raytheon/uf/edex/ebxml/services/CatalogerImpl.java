/**
 * 
 */
package com.raytheon.uf.edex.ebxml.services;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Cataloger;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsResponse;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory;

/**
 * @author jsherida
 *
 */
public class CatalogerImpl implements Cataloger {

    /** {@inheritDoc} */
    @Override
    public CatalogObjectsResponse catalogObjects(
            CatalogObjectsRequest partCatalogObjectsRequest)
            throws MsgRegistryException {
        // TODO Auto-generated method stub
        
        ObjectFactory factory = new ObjectFactory();
        CatalogObjectsResponse response = factory.createCatalogObjectsResponse();
        response.setRequestId(partCatalogObjectsRequest.getId());
        response.setStatus("Request Not Implemented");
        return response;
    }

}
