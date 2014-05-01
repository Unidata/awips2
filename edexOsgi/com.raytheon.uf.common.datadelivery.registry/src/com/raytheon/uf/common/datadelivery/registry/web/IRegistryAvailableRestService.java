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
package com.raytheon.uf.common.datadelivery.registry.web;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

/**
 * 
 * Interface for the registry available service
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/21/2013    2022        bphillip    Initial implementation
 * 10/30/2013   1538        bphillip    Moved data delivery specific servics out of registry plugin
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path(IRegistryAvailableRestService.REGISTRY_AVAILABILITY_PATH_PREFIX)
public interface IRegistryAvailableRestService {

    public static final String REGISTRY_AVAILABILITY_PATH_PREFIX = "/registryAvailable";

    /**
     * Method that simply returns a string. This method is called to see if
     * registry services are available for a registry
     * 
     * @return A string
     */
    @GET
    @Produces("text/plain")
    public String isRegistryAvailable();

}
