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

import com.raytheon.uf.common.registry.constants.RegistryAvailability;
import com.raytheon.uf.common.registry.services.rest.IRegistryAvailableRestService;
import com.raytheon.uf.edex.registry.ebxml.dao.DbInit;

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
 * 9/5/2013     1538        bphillip    Removed log message
 * 10/30/2013   1538        bphillip    Moved data delivery specific services out of registry plugin
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * 9/23/2019    7836        bsteffen    Use RegistryFederationManager.isInitialized();
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path(IRegistryAvailableRestService.REGISTRY_AVAILABILITY_PATH_PREFIX)
@Service
@Transactional
public class RegistryAvailableRestService
        implements IRegistryAvailableRestService {

    /**
     * Creates a new RegistryAvailableRestService
     */
    public RegistryAvailableRestService() {

    }

    @GET
    @Produces("text/plain")
    @Override
    public String isRegistryAvailable() {
        if (DbInit.isDbInitialized()) {
            if (RegistryFederationManager.isInititialized()) {
                if (RegistryFederationManager.SYNC_IN_PROGRESS.get()) {
                    return RegistryAvailability.SYNC_IN_PROGRESS;
                }
                return RegistryAvailability.AVAILABLE;
            }
        }
        return RegistryAvailability.DB_NOT_INITIALIZED;
    }
}
