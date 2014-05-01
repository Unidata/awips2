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
package com.raytheon.uf.edex.registry.ebxml.web;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PartyType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.PartyDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;

/**
 * 
 * Servlet implementation used to add a user or organization to the registry.
 * FIXME: This class will be refactored in a later ticket
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 7/31/2012    #724       bphillip     Initial creation
 * 3/13/2013    1082       bphillip     Made transactional
 * 4/19/2013    1931       bphillip     Refactored to use web application spring container and cxf services
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Path("/AddRegistryParty")
@Service
@Transactional
public class AddRegistryParty {

    /** Serial */
    private static final long serialVersionUID = 1422748054768442033L;

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AddRegistryParty.class);

    /** The page to display upon success */
    private static final String SUCCESS_RESPONSE_PAGE = "addPartySuccess";

    /** the page to display upon failure */
    private static final String ERROR_RESPONSE_PAGE = "addPartyFailure";

    private PartyDao partyDao;

    private LifecycleManagerImpl lcm;

    private RegistryWebUtil webUtil;

    @POST
    @Produces("text/html")
    public Response doPost(@Context HttpServletRequest request)
            throws IOException {
        String partyId = request.getParameter(WebFields.ID.fieldName());
        String objectType = request
                .getParameter(WebFields.OBJ_TYPE.fieldName());
        PartyType existingParty = null;

        // The EDEX internal user cannot be modified
        if (partyId.equals(RegistryUtil.DEFAULT_OWNER)) {
            return Response.serverError().build();
            // webUtil.sendErrorResponse(request, response,
            // ERROR_RESPONSE_PAGE, partyId, objectType,
            // "Cannot modify EDEX Internal User");
        }

        /*
         * Check to see if the party already exists. If so, the user cannot be
         * added
         */
        existingParty = partyDao.getById(partyId);

        if (existingParty != null) {
            statusHandler.error("Error adding " + objectType + " to registry. "
                    + objectType + " " + partyId + " already exists");
            return Response.serverError().build();
            // webUtil.sendErrorResponse(request, response,
            // ERROR_RESPONSE_PAGE, partyId, objectType, objectType
            // + " Already Exists");

        }

        /*
         * Create a new submit request containing the new party
         */
        SubmitObjectsRequest submitRequest = null;
        try {
            submitRequest = webUtil.createParty(request);
        } catch (EbxmlRegistryException e) {
            statusHandler.error("Error creating " + objectType, e);
            return Response.serverError().build();
            // webUtil.sendErrorResponse(
            // request,
            // response,
            // ERROR_RESPONSE_PAGE,
            // partyId,
            // objectType,
            // "Error creating " + objectType + "\n"
            // + e.getLocalizedMessage());
        }

        // Submit the objects to the registry
        try {
            lcm.submitObjects(submitRequest);
            webUtil.updatePC(request);
        } catch (Exception e) {
            statusHandler.error("Error submitting new " + objectType
                    + " to the registry", e);
            return Response.serverError().build();
            // webUtil.sendErrorResponse(
            // request,
            // response,
            // ERROR_RESPONSE_PAGE,
            // partyId,
            // objectType,
            // "Error submitting new " + objectType
            // + " to the registry\n"
            // + e.getLocalizedMessage());
        }

        // Send success response back to the caller
        return Response.ok().build();
        // webUtil.sendSuccessResponse(request, response, SUCCESS_RESPONSE_PAGE,
        // partyId, objectType);
    }

    public void setWebUtil(RegistryWebUtil webUtil) {
        this.webUtil = webUtil;
    }

    public void setPartyDao(PartyDao partyDao) {
        this.partyDao = partyDao;
    }

    public void setLcm(LifecycleManagerImpl lcm) {
        this.lcm = lcm;
    }
}
