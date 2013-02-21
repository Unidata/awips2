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

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Cataloger;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsResponse;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.CanonicalIndices;
import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

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

    @Override
    public CatalogObjectsResponse catalogObjects(
            CatalogObjectsRequest partCatalogObjectsRequest)
            throws MsgRegistryException {
        statusHandler.info("Cataloger received catalogObjects Request");
        throw EbxmlExceptionUtil.createMsgRegistryException(
                "Cataloger service not yet implemented",
                UnsupportedCapabilityExceptionType.class, "",
                "Unsupported Service", "Unsupported Service",
                ErrorSeverity.ERROR, statusHandler);
    }

    /**
     * Indexes a registry object
     * 
     * @param obj
     *            The registry object to index
     * @throws EbxmlRegistryException
     *             If the indexing process produces errors
     */
    public void index(RegistryObjectType obj) throws EbxmlRegistryException {
        if (obj instanceof AuditableEventType) {
            return;
        }
        statusHandler.info("Cataloging object [" + obj.getId() + "]");
        obj = registryObjectDao.getById(obj.getId());

        List<IndexEntry> entries = new ArrayList<IndexEntry>();

        if (obj.getName() != null) {
            IndexEntry nameEntry = new IndexEntry(obj.getId(),
                    obj.getObjectType(), CanonicalIndices.NAME, obj.getName()
                            .getLocalizedString().get(0).getValue());
            entries.add(syncEntry(nameEntry));
        }

        if (obj.getDescription() != null) {
            IndexEntry descEntry = new IndexEntry(obj.getId(),
                    obj.getObjectType(), CanonicalIndices.DESCRIPTION, obj
                            .getDescription().getLocalizedString().get(0)
                            .getValue());
            entries.add(syncEntry(descEntry));
        }
        statusHandler.info("Persisting indexed values to registry for object ["
                + obj.getId() + "]");
        new RegistryDao(IndexEntry.class).saveOrUpdate(entries);
        statusHandler.info("Catalog information persisted for object ["
                + obj.getId() + "]");
    }

    /**
     * Syncs the given index entry with the database
     * 
     * @param entry
     *            The entry to check
     * @return The persistent entry if it exists, else the given object
     * @throws EbxmlRegistryException
     *             If errors occur during the query
     */
    private IndexEntry syncEntry(IndexEntry entry)
            throws EbxmlRegistryException {
        List<IndexEntry> result = registryObjectDao.executeHQLQuery("from "
                + IndexEntry.class.getName() + " x where x.key= "
                + entry.getKey());
        if (result.isEmpty()) {
            return entry;
        } else {
            return result.get(0);
        }
    }

    public void setRegistryObjectDao(RegistryObjectTypeDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
