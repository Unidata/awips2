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
import javax.ws.rs.Produces;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.services.rest.IRegistryAvailableRestService;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Rest service used to check availability of a registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/21/2013    2022        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path("/rest/registryAvailable")
@Service
@Transactional
public class RegistryAvailableRestService implements
        IRegistryAvailableRestService {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryAvailableRestService.class);

    /** Arbitrary message retured from this service */
    private static final String REGISTRY_AVAILABLE_MESSAGE = "Registry services available.";

    /**
     * Creates a new RegistryAvailableRestService
     */
    public RegistryAvailableRestService() {

    }

    @GET
    @Produces("text/plain")
    public String isRegistryAvailable() {
        statusHandler.info("Received request checking registry availability");
        return REGISTRY_AVAILABLE_MESSAGE;
    }
}
