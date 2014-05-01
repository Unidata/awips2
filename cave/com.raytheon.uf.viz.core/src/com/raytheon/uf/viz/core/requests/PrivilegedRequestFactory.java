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

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Factory class that should be used to construct privileged requests
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

public class PrivilegedRequestFactory {

    /**
     * Given the class, construct the request
     * 
     * @param <T>
     * @param requestClass
     * @return
     * @throws VizException
     */
    public static <T extends AbstractPrivilegedRequest> T constructPrivilegedRequest(
            Class<T> requestClass) throws VizException {
        try {
            return AbstractPrivilegedRequest.createRequest(requestClass,
                    UserController.getUserObject());
        } catch (AuthException e) {
            throw new VizException("Error constructing request class: "
                    + requestClass, e);
        }
    }

}
