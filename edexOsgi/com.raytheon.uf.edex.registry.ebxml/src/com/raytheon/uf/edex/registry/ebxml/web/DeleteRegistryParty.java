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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PartyType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.PartyDao;

/**
 * 
 * Servlet implementation used to delete a user or organization from the
 * registry
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
public class DeleteRegistryParty extends javax.servlet.http.HttpServlet
        implements javax.servlet.Servlet {

    /** The serial ID */
    private static final long serialVersionUID = -9009661529309992652L;

    /** The page to display upon success */
    private static final String SUCCESS_RESPONSE_PAGE = "deletePartySuccess";

    /** The page to display upon failure */
    private static final String ERROR_RESPONSE_PAGE = "deletePartyFailure";

    // FIXME: Add spring support to servlets

    private PartyDao partyDao = (PartyDao) EDEXUtil.getESBComponent("partyDao");

    private RegistryWebUtil webUtil = (RegistryWebUtil) EDEXUtil
            .getESBComponent("webUtil");

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DeleteRegistryParty.class);

    @Override
    protected void doPost(HttpServletRequest request,
            HttpServletResponse response) throws IOException {
        try {
            String partyId = request.getParameter(WebFields.ID.fieldName());
            String objectType = request.getParameter(WebFields.OBJ_TYPE
                    .fieldName());
            PartyType existingParty = null;

            // The EDEX internal user cannot be modified
            if (partyId.equals(RegistryUtil.DEFAULT_OWNER)) {
                webUtil.sendErrorResponse(request, response,
                        ERROR_RESPONSE_PAGE, partyId, objectType,
                        "Cannot remove EDEX Internal User");
            }

            /*
             * Check if the party exists. If not, the party obviously cannot be
             * deleted
             */

            existingParty = partyDao.getById(partyId);

            if (existingParty == null) {
                webUtil.sendErrorResponse(request, response,
                        ERROR_RESPONSE_PAGE, partyId, objectType,
                        "Unable to delete " + objectType + " " + partyId + ". "
                                + objectType + " does not exist");
            }

            /*
             * Remove any associations to the party
             */
            try {
                webUtil.removeAssociations(existingParty);
                webUtil.removeParty(existingParty);
            } catch (Exception e) {
                statusHandler.error("Error modifying user", e);
                webUtil.sendErrorResponse(request, response,
                        ERROR_RESPONSE_PAGE, partyId, objectType,
                        "Error removing associations to " + objectType + "\n"
                                + e.getLocalizedMessage());
            }

            // Send back a successful response to the requester
            webUtil.sendSuccessResponse(request, response,
                    SUCCESS_RESPONSE_PAGE, partyId, objectType);
        } catch (ServletException e) {
            statusHandler.error("Error generating response", e);
            response.sendError(1,
                    "Error generating response: " + e.getLocalizedMessage());
        }
    }

    public void setPartyDao(PartyDao partyDao) {
        this.partyDao = partyDao;
    }

    public void setWebUtil(RegistryWebUtil webUtil) {
        this.webUtil = webUtil;
    }

}
