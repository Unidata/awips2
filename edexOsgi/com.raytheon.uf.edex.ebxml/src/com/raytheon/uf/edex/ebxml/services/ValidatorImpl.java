/**
 * 
 */
package com.raytheon.uf.edex.ebxml.services;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Validator;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

/**
 * @author jsherida
 *
 */
public class ValidatorImpl implements Validator {

    /** {@inheritDoc} */
    @Override
    public ValidateObjectsResponse validateObjects(
            ValidateObjectsRequest partValidateObjectsRequest)
            throws MsgRegistryException {
        // TODO Auto-generated method stub
        
        ObjectFactory factory = new ObjectFactory();
        ValidateObjectsResponse response = factory.createValidateObjectsResponse();
        response.setRequestId(partValidateObjectsRequest.getId());
        response.setStatus("Request Not Implemented");
        return response;
    }

}
