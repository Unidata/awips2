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
package com.raytheon.uf.edex.registry.ebxml.services.rest;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.services.rest.IRepositoryItemsRestService;
import com.raytheon.uf.edex.registry.ebxml.dao.ExtrinsicObjectDao;

/**
 * 
 * REST service implementation for getting repository items according to
 * specifications in Section 12.1.2 of the ebRS specification
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/19/2013    1931        bphillip    Initial implementation
 * 5/21/2013    2022        bphillip    Added interface and changed method name
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path("/rest/repositoryItems/{repositoryItemId}")
@Service
@Transactional
public class RepositoryItemsRestService implements IRepositoryItemsRestService {

    /**
     * The data access object for getting extrinsic objects containing
     * repository items
     */
    private ExtrinsicObjectDao extrinsicObjectDao;

    /**
     * Gets the repository item for the object with the id specified in the url
     * 
     * @param repositoryItemId
     *            The id of the object containing the repository item
     * @return The repository item for the object specified
     */
    @GET
    @Produces("application/octet-stream")
    public byte[] getRepositoryItem(
            @PathParam("repositoryItemId") String repositoryItemId) {
        ExtrinsicObjectType obj = extrinsicObjectDao.getById(repositoryItemId);
        if (obj == null) {
            throw new WebApplicationException(404);
        }
        if (obj.getRepositoryItem() == null) {
            throw new WebApplicationException(404);
        }
        return obj.getRepositoryItem();

    }

    public void setExtrinsicObjectDao(ExtrinsicObjectDao extrinsicObjectDao) {
        this.extrinsicObjectDao = extrinsicObjectDao;
    }
}
