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

import javax.annotation.Resource;
import javax.xml.ws.WebServiceContext;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Cataloger;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsResponse;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Implementation of the RegRep cataloger service
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012 184        bphillip     Initial creation
 * 3/18/2013    1082       bphillip     Commented out old incorrect code until this class is properly implemented
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class CatalogerImpl implements Cataloger {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CatalogerImpl.class);

    /** The data access object */
    private RegistryObjectTypeDao registryObjectDao;

    @Resource
    private WebServiceContext wsContext;

    @Override
    public CatalogObjectsResponse catalogObjects(
            CatalogObjectsRequest partCatalogObjectsRequest)
            throws MsgRegistryException {
        statusHandler.info("Cataloger received catalogObjects Request from ["
                + EbxmlObjectUtil.getClientHost(wsContext) + "]");
        return EbxmlObjectUtil.spiObjectFactory.createCatalogObjectsResponse();
    }

    public void setRegistryObjectDao(RegistryObjectTypeDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
