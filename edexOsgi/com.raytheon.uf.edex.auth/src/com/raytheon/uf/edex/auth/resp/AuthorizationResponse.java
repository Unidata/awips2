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

/**
 * Response object for authorization attempts
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

public class AuthorizationResponse {
    private boolean authorized;

    public String responseMessage;

    public AuthorizationResponse(boolean authorized) {
        this(authorized, null);
    }

    public AuthorizationResponse(String message) {
        this(false, message);
    }

    /**
     * @param authorized2
     * @param object
     */
    public AuthorizationResponse(boolean authorized, String message) {
        this.authorized = authorized;
        this.responseMessage = message;
    }

    public boolean isAuthorized() {
        return authorized;
    }

    public String getResponseMessage() {
        return responseMessage;
    }

}
