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
package com.raytheon.uf.edex.auth.resp;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.auth.resp.UserNotAuthenticated;
import com.raytheon.uf.common.auth.resp.UserNotAuthorized;
import com.raytheon.uf.common.auth.user.IAuthenticationData;

/**
 * Factory class used to generate response objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ResponseFactory {

    public static UserNotAuthorized constructNotAuthorized(
            AbstractPrivilegedRequest request, String responseMessage) {
        UserNotAuthorized response = new UserNotAuthorized();
        response.setRequest(request);
        response.setMessage(responseMessage);
        return response;
    }

    public static UserNotAuthenticated constructNotAuthenticated(
            AbstractPrivilegedRequest request, IAuthenticationData authData) {
        UserNotAuthenticated response = new UserNotAuthenticated();
        response.setRequest(request);
        response.setAuthenticationData(authData);
        return response;
    }

    public static SuccessfulExecution constructSuccessfulExecution(
            Object responseObject, IAuthenticationData updatedData) {
        SuccessfulExecution execution = new SuccessfulExecution();
        execution.setResponse(responseObject);
        execution.setUpdatedData(updatedData);
        return execution;
    }
}
