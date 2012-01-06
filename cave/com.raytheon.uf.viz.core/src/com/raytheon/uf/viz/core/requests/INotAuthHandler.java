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
package com.raytheon.uf.viz.core.requests;

import com.raytheon.uf.common.auth.resp.UserNotAuthenticated;
import com.raytheon.uf.common.auth.resp.UserNotAuthorized;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Interface for handling unauthenticted/unauthorized privileged request
 * responses
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

public interface INotAuthHandler {

    /**
     * Called when user sends a privileged request they are not authorized to be
     * sending
     * 
     * @param response
     * @return
     * @throws VizException
     */
    public Object notAuthorized(UserNotAuthorized response) throws VizException;

    /**
     * Called when user sends a privileged request and they are not
     * authenticated with the server. *Should* get authentication data and
     * resend the request
     * 
     * @param response
     * @return
     * @throws VizException
     */
    public Object notAuthenticated(UserNotAuthenticated response)
            throws VizException;

}
