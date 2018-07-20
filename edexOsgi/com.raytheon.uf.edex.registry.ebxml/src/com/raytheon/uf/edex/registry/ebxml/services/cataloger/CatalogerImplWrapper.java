/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.registry.ebxml.services.cataloger;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Cataloger;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsResponse;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;

/**
 * 
 * Wrapper for the cataloger service to be used with the SOAP interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/11/2013    1707        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
public class CatalogerImplWrapper implements Cataloger {

    private CatalogerImpl cataloger;

    public CatalogerImplWrapper() {

    }

    public CatalogerImplWrapper(CatalogerImpl cataloger) {
        this.cataloger = cataloger;
    }

    @Override
    @WebMethod(action = CATALOG_OBJECTS_ACTION)
    @WebResult(name = "CatalogObjectsResponse", targetNamespace = EbxmlNamespaces.SPI_URI, partName = "partCatalogObjectsResponse")
    public CatalogObjectsResponse catalogObjects(
            @WebParam(name = "CatalogObjectsRequest", targetNamespace = EbxmlNamespaces.SPI_URI, partName = "partCatalogObjectsRequest") CatalogObjectsRequest partCatalogObjectsRequest)
            throws MsgRegistryException {
        return cataloger.catalogObjects(partCatalogObjectsRequest);
    }

}
