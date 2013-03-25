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

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PartyType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.PartyDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;

/**
 * 
 * Servlet implementation used to add a user or organization to the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 7/31/2012    #724       bphillip     Initial creation
 * 3/13/2013    1082       bphillip     Made transactional
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional
public class AddRegistryParty extends javax.servlet.http.HttpServlet implements
        javax.servlet.Servlet {

    /** Serial */
    private static final long serialVersionUID = 1422748054768442033L;

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AddRegistryParty.class);

    /** The page to display upon success */
    private static final String SUCCESS_RESPONSE_PAGE = "addPartySuccess";

    /** the page to display upon failure */
    private static final String ERROR_RESPONSE_PAGE = "addPartyFailure";

    // FIXME: Add spring support to servlets

    private PartyDao partyDao = (PartyDao) EDEXUtil.getESBComponent("partyDao");

    private LifecycleManagerImpl lcm = (LifecycleManagerImpl) EDEXUtil
            .getESBComponent("lcmServiceImpl");

    private RegistryWebUtil webUtil = (RegistryWebUtil) EDEXUtil
            .getESBComponent("webUtil");

    @Override
    protected void doPost(HttpServletRequest request,
            HttpServletResponse response) throws IOException {
        String partyId = request.getParameter(WebFields.ID.fieldName());
        String objectType = request
                .getParameter(WebFields.OBJ_TYPE.fieldName());
        PartyType existingParty = null;

        try {
            // The EDEX internal user cannot be modified
            if (partyId.equals(RegistryUtil.DEFAULT_OWNER)) {
                webUtil.sendErrorResponse(request, response,
                        ERROR_RESPONSE_PAGE, partyId, objectType,
                        "Cannot modify EDEX Internal User");
            }

            /*
             * Check to see if the party already exists. If so, the user cannot
             * be added
             */
            existingParty = partyDao.getById(partyId);

            if (existingParty != null) {
                statusHandler.error("Error adding " + objectType
                        + " to registry. " + objectType + " " + partyId
                        + " already exists");
                webUtil.sendErrorResponse(request, response,
                        ERROR_RESPONSE_PAGE, partyId, objectType, objectType
                                + " Already Exists");

            }

            /*
             * Create a new submit request containing the new party
             */
            SubmitObjectsRequest submitRequest = null;
            try {
                submitRequest = webUtil.createParty(request);
            } catch (EbxmlRegistryException e) {
                statusHandler.error("Error creating " + objectType, e);
                webUtil.sendErrorResponse(
                        request,
                        response,
                        ERROR_RESPONSE_PAGE,
                        partyId,
                        objectType,
                        "Error creating " + objectType + "\n"
                                + e.getLocalizedMessage());
            }

            // Submit the objects to the registry
            try {
                lcm.submitObjects(submitRequest);
                webUtil.updatePC(request);
            } catch (Exception e) {
                statusHandler.error("Error submitting new " + objectType
                        + " to the registry", e);
                webUtil.sendErrorResponse(
                        request,
                        response,
                        ERROR_RESPONSE_PAGE,
                        partyId,
                        objectType,
                        "Error submitting new " + objectType
                                + " to the registry\n"
                                + e.getLocalizedMessage());
            }

            // Send success response back to the caller
            webUtil.sendSuccessResponse(request, response,
                    SUCCESS_RESPONSE_PAGE, partyId, objectType);

        } catch (ServletException e) {
            statusHandler.error("Error generating response", e);
            response.sendError(1,
                    "Error generating response: " + e.getLocalizedMessage());
        }
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
