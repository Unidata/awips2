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
package com.raytheon.viz.gfe.dialogs.sbu;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dataplugin.gfe.request.CheckPermissionsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.site.requests.GetPrimarySiteRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class CheckPermissions {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupDlg.class);

    @SuppressWarnings("unchecked")
    public static boolean getAuthorization() {
        boolean authorized = false;
        IUser user = UserController.getUserObject();
        CheckPermissionsRequest request = new CheckPermissionsRequest();
        request.setUser(user);
        try {
            ServerResponse<String> obj = (ServerResponse<String>) ThriftClient
                    .sendRequest(request);
            if (obj.isOkay()) {
                authorized = true;
            } else {
                authorized = false;
            }
        } catch (VizException e) {
            statusHandler.error("Error checking permissions for: " + user, e);
            authorized = false;
        }
        return authorized;
    }

    public static boolean runningAsPrimary() {
        boolean isPrimary = false;

        GetPrimarySiteRequest request = new GetPrimarySiteRequest();
        try {
            String obj = (String)ThriftClient
                    .sendRequest(request);
                return LocalizationManager.getInstance().getCurrentSite()
                        .equalsIgnoreCase(obj);
        } catch (VizException e) {
            statusHandler
                    .error("Error checking if running as primary site!", e);
        }
        return isPrimary;
    }
}
